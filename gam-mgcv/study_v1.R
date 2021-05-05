
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019


## TO DO ####
#
# Models for sigma in NOtr
# Alternative dist in gamlss
# qgam()



## Begin... ####
require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("jbrowell/ProbCast")
require(ProbCast)
require(readxl)
require(gamlss.tr)
require(gamlss.add)
gen.trun(par = c(0),family = NO,type="left")
gen.trun(par = c(0),family = TF2,type="left")

setwd(dirname(getActiveDocumentContext()$path))

JB_results <- list()
# load("../results/JethroResults_2021-04-26.Rda")

## Load and Prep Data ####
{
  # load("../data/hw_hourly.rds")
  # h2 <- fread("../data/h2_hourly.csv")
  h2 <- fread("../data/h2_hourly_gb.csv")
  h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
  h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"
  
  ## Check DLS?
  # plot(h2[month(targetTime)%in%c(11:12,1:3),mean(n_attendance),by=hour(targetTime)])
  # points(h2[month(targetTime)%in%c(4:10),mean(n_attendance),by=hour(targetTime)],pch=2)
  # 
  # plot(h2[month(targetTime_UK)%in%c(11:12,1:3),mean(n_attendance),by=hour(targetTime_UK)])
  # points(h2[month(targetTime_UK)%in%c(4:10),mean(n_attendance),by=hour(targetTime_UK)],pch=2)
  
  
  ## Add smooths/level tracking
  setkey(h2,targetTime)
  h2[,unique(diff(targetTime))]
  h2[,n_att_rollmean:=frollmean(n_attendance,n=12*7*24,align = "right")]
  h2[,n_att_rollmean_lag48:=shift(n_att_rollmean,n=48,type="lag")]
  # h2[,.(plot(targetTime,n_attendance,pch="."),lines(targetTime,n_att_rollmean,col="red",lwd=3))]
  
  add_calendar_variables(h2,datetimecol = "targetTime_UK")
  
  ## Load Holidays
  # hols <- as.data.table(read_xlsx("../data/holiday_rugby.xlsx"))
  hols <- as.data.table(read_xlsx("../data/holiday_rugby_all.xlsx"))
  hols[,Date:=as.Date(Date)]
  h2[,Date:=as.Date(targetTime_UK)]
  h2 <- merge(h2,hols,by="Date",all.x=T)
  rm(hols)
  h2[,school_holiday:=as.factor(school_holiday)]
  h2[is.na(school_holiday),school_holiday:="No School Holiday"]
  h2[,festive_day:=as.factor(festive_day)]
  h2[is.na(festive_day),festive_day:="No Festive Day"]
  h2[,public_holiday:=as.factor(public_holiday)]
  h2[is.na(public_holiday),public_holiday:="No Public Holiday"]
  h2[,is_rug_in_Cardiff:=is_rug_in_Cardiff==1]
  h2[,is_rug_out_Cardiff:=is_rug_out_Cardiff==1]
  
  # hols <- data.table(read_xlsx("../data/bank_holiday.xlsx"))
  # hols[,Date:=as.Date(ds)]; hols[,ds:=NULL]
  # h2 <- merge(h2,hols,by="Date",all.x=T)
  # h2[,holiday:=as.factor(holiday)]
  
  
  #load weather data
  weather_data <- data.table()
  for(f in c(list.files(path="../data/",pattern = "weather",full.names = T))){
    load(f)
    weather_data <- rbind(weather_data,features)
  }
  weather_data <- weather_data[issueTime>="2014-04-01",]
  h2 <- merge(weather_data,h2,by = "targetTime")
  rm(f,features,weather_data)
  h2[,wind10m:=sqrt(`10U`^2+`10V`^2)]
  h2[,T2T:=`2T`]
  
  ## Fill small number of missing values
  h2[is.na(T2T),T2T:=mean(h2$T2T,na.rm = T)]
  
  ## Force issuetimes into LocalTime. Not ideal but also not significant either...
  h2[,issueTime:=lubridate::force_tz(issueTime,tzone = "Europe/London")]
  # h2[,targetTime:=NULL]
  #h2[!(h2[,issueTime]%in%h2[,targetTime_UK]),]
  h2 <- h2[(targetTime_UK-issueTime)/3600<=48,]
  setcolorder(h2, c("issueTime","targetTime_UK"))
  
  ## Set-up CV - Test Data: 1/3/2018 to 28/2/2019
  h2[issueTime>="2018-03-01" & issueTime<"2019-03-01",kfold:="Test"]
  h2[issueTime<"2018-03-01",kfold:=paste0("fold",rep(rep(1:2,each=24*7),length.out=.N))]
  
  
  ## Features
  setkey(h2,issueTime,targetTime_UK)
  # Weekday groups
  # Monday, Saturday and Sunday are quite distinct...
  h2[!(dow%in%c("Tue","Wed","Thu","Fri")),dow2:=dow]
  h2[dow%in%c("Tue","Wed","Thu","Fri"),dow2:="TueFri"]
  h2[,dow2:=as.factor(dow2)]
  h2[public_holiday=="No Public Holiday",hol_factor:=public_holiday]
  h2[public_holiday!="No Public Holiday",hol_factor:="Public Holiday"]
  h2[public_holiday=="New Years Day",hol_factor:="New Years Day"]
  h2[public_holiday=="Christmas Day",hol_factor:="Christmas Day"]
  
  ## Add public holiday as day-type
  h2[public_holiday=="No Public Holiday",dow3:=dow]
  h2[public_holiday!="No Public Holiday",dow3:=hol_factor]
  
  ## Quick plot ####
  require(ggplot2)
  plot_data <- h2[,.(n_attendance=mean(n_attendance)),by=c("dow","clock_hour")]
  ggplot(data=plot_data,aes(x=clock_hour,y=n_attendance,color=dow))+
    geom_line()
  
}

## Benchmark 1 #### Empirical distribution by hour of the day and day type

BenchPred <- h2[kfold!="Test",as.list(quantile(n_attendance,probs = 1:19/20)),by=c("dow2","clock_hour")]
setnames(BenchPred,paste0((1:19/20)*100,"%"),paste0("q",(1:19/20)*100))

Bench_mqr <- copy(h2[,.(issueTime,targetTime_UK,dow2,clock_hour)])
Bench_mqr <- merge(Bench_mqr,BenchPred,by=c("dow2","clock_hour"),all.x=T)
Bench_mqr <- Bench_mqr[,-(1:2)]
class(Bench_mqr) <- c("MultiQR",class(Bench_mqr))

JB_results[["Benchmark_1"]] <- Bench_mqr

issue <- unique(h2$issueTime)[7]
plot(Bench_mqr[issueTime==issue,-(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")

reliability(Bench_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold)
pinball(Bench_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold)




## Fit Poisson-GAM and visualise model ####

## Version 1: All days, no holiday effects, no smooted lags

for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  gam1 <- bam(n_attendance ~
                # s(clock_hour,k=24,by=dow2) + t + I(t^2) +
                # ti(doy,clock_hour,k=c(6,6)),
                # te(doy,clock_hour,k=c(6,24),by=dow2) + t,
                dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
              # family = ziP(link = "identity"), # error
              family = poisson(link = "log"), # log, sqrt
              data=h2[kfold!=fold & kfold!="Test",],discrete = T) 
  
  h2[kfold==fold,lambda:=predict(gam1,newdata =h2[kfold==fold,],type="response")]
  
}


## Families: 
# gaussian()
# poisson()
# nb() / negbin() is useful for overdispersed count data, but computation is slow.
# So far: poisson a little better than other two...

## In-sample RMSE
sqrt(mean((gam1$y-gam1$fitted.values)^2))
mean(gam1$y-gam1$fitted.values)

## Check fit - is the model using all DOF? Is so, consider increasing availability...
gam.check(gam1)
plot(gam1,pages = 2)
plot(gam1,select = 3,scheme = 1)


# ## GAM VIS
# gam1 <- getViz(gam1,nsim = 200)
# 
# check1D(gam1,"clock_hour")
# check1D(gam1,"doy")
# check2D(gam1,"dow","clock_hour")
# check2D(gam1,"doy","clock_hour")
# 
# ## Lag effects
# check1D(gam1,h2[kfold!="Test" ,n_att_rollmean])
# check1D(gam1,h2[kfold!="Test" ,n_att_rollmean_lag48])
# 
# ## Any holiday effects?
# check2D(gam1,h2[kfold!="Test" ,school_holiday],"clock_hour") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# check2D(gam1,h2[kfold!="Test" ,festive_day],"clock_hour") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# check2D(gam1,h2[kfold!="Test" ,public_holiday],"clock_hour") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))



#check2D(gam1,h2[,is_rug_in_Cardiff],"clock_hour")

## Any weather effects?
# check1D(gam1,h2[kfold!="Test",T2T])
# check2D(gam1,h2[kfold!="Test",T2T],"clock_hour")
# check1D(gam1,h2[kfold!="Test",TP])
# check1D(gam1,h2[kfold!="Test",SSRD])
# check1D(gam1,h2[kfold!="Test",LCC+MCC+HCC])
# check1D(gam1,h2[kfold!="Test",wind10m])
# check2D(gam1,h2[kfold!="Test",wind10m],"clock_hour")
# check2D(gam1,h2[kfold!="Test",wind10m],h2[,TP])


## Quantiles and evaluation ####

# h2_mqr <- data.table(q5=qpois(p = 0.05,lambda = h2[,lambda]) )
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
for(p in 1:19/20){
  h2_mqr[[paste0("q",p*100)]] <- qpois(p = p,lambda = h2[,lambda]) 
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

JB_results[["Poisson-GAM-te_v1"]] <- h2_mqr


issue <- unique(h2$issueTime)[7]
plot(h2_mqr[issueTime==issue,-(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


# save(h2_mqr,file = "../data/example_forecast_format.R")


## Version 2: Grouped days, some holiday effects, no smooted lags ####

## School hols and public hol dummies

for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  gam2 <- bam(n_attendance ~
                ## v1 above:
                # dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                # s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
                ## v2 here:
                dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
                school_holiday + s(clock_hour,k=6,by=school_holiday,bs = "cr") +
                s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
              data=h2[kfold!=fold & kfold!="Test",],family = poisson(),discrete = T)
  
  h2[kfold==fold,lambda:=predict(gam2,newdata =h2[kfold==fold,],type="response")]
  
}

## Families: 
# gaussian()
# poisson()
# nb() / negbin() is useful for overdispersed count data, but computation is slow.
# So far: poisson a little better than other two...

## In-sample RMSE
sqrt(mean((gam2$y-gam2$fitted.values)^2))
mean(gam2$y-gam2$fitted.values)

## Check fit - is the model using all DOF? Is so, consider increasing availability...
gam.check(gam2)
summary(gam2)
plot(gam2,pages = 2)
plot(gam2,select = 3,scheme = 1)
plot(gam2,select = 7,scheme = 1)

## GAM VIS
gam2 <- getViz(gam2,nsim = 200)

check1D(gam2,"clock_hour")
check1D(gam2,"doy")
check2D(gam2,"dow3","clock_hour")
check2D(gam2,"doy","clock_hour")

## Lag effects
# check1D(gam2,h2[kfold!="Test",n_att_rollmean])
# check1D(gam2,h2[kfold!="Test",n_att_rollmean_lag48])

## Any remaining holiday effects?
check2D(gam2,h2[kfold!="Test",school_holiday],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam2,h2[kfold!="Test",festive_day],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam2,h2[kfold!="Test",public_holiday],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#check2D(gam2,h2[,is_rug_in_Cardiff],"clock_hour")

## Any weather effects?
# check1D(gam2,h2[kfold!="Test",T2T])
# check2D(gam2,h2[kfold!="Test",T2T],"clock_hour")
# check2D(gam2,h2[kfold!="Test",doy],"clock_hour")
# check1D(gam2,h2[kfold!="Test",TP])
# check1D(gam2,h2[kfold!="Test",SSRD])
# check1D(gam2,h2[kfold!="Test",LCC+MCC+HCC])
# check1D(gam2,h2[kfold!="Test",wind10m])
# check2D(gam2,h2[kfold!="Test",wind10m],"clock_hour")
# check2D(gam2,h2[kfold!="Test",wind10m],h2[,TP])


## Quantiles and evaluation ####

# h2_mqr <- data.table(q5=qpois(p = 0.05,lambda = h2[,lambda]) )
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
for(p in 1:19/20){
  h2_mqr[[paste0("q",p*100)]] <- qpois(p = p,lambda = h2[,lambda]) 
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

JB_results[["Poisson-GAM-te_v2"]] <- h2_mqr


issue <- unique(h2$issueTime)[7]
plot(h2_mqr[issueTime==issue,-(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold)
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))



## GAMLSS ####
require(gamlss.tr)
# require(gamlss.add) # for ga()
# # check out http://opisthokonta.net/?p=1157
# # PO Same results as gam()
# # Double Poisson DPO
# # Negative Binomial NBI
# # Poisson-inverse Gaussian distribution (PIG)
# # Delaporte distribution (DEL)
# # Sichel distribution (SI, SICHEL)
# # Truncated normal?


# h2_gamlss <- ppd_gamlss(data = h2,
#                         formula = n_attendance ~
#                           dow + dow*cs(clock_hour,df=23) +
#                           t:cs(doy,k=6),
#                         sigma.formula = ~cs(clock_hour,df=12),
#                         family =  NO,
#                         method=mixed(20,10))
# 
# h2_gamlss_mqr <- PPD_2_MultiQR(data=h2,
#                                models = h2_gamlss,
#                                params = F)
# 
# issue <- unique(h2$issueTime)[9]
# plot(h2_gamlss_mqr[h2[,which(issueTime==issue)],],
#      xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
#      ylim=c(0,40),Legend = "topleft")


# reliability(h2_gamlss_mqr,h2$n_attendance)
# reliability(h2_gamlss_mqr,h2$n_attendance,subsets = h2$clock_hour)
# pinball(h2_gamlss_mqr,h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))

# 
# JB_results[["NBI-gamlss"]] <- cbind(h2[,.(issueTime,targetTime_UK)],h2_gamlss_mqr)


## GAMLSS Manual, v1 formula bam() as above ####
#
# NB: fit takes approx. 10 min per fold


h2_gamlss1_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])
for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  train_data <- na.omit(h2[kfold!=fold & kfold!="Test",
                           .(issueTime,targetTime_UK,
                             n_attendance,dow,dow3,clock_hour,t,doy,T2T)])
  
  h2_gamlss1 <- gamlss(data = train_data,
                       formula = n_attendance ~ ba(
                         ## v1 above:
                         ~ dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                           s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
                         ## v2 here:
                         # ~ dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
                         #   school_holiday + s(clock_hour,k=6,by=school_holiday,bs = "cr") +
                         #   s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T)
                       ),
                       ## Consider ba() here, and by dow/dow3
                       sigma.formula = ~cs(clock_hour,df=12),
                       family =  NOtr,
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T)]
  
  temp_params <- predictAll(h2_gamlss1,newdata = test_data,
                            data = train_data)
  h2_gamlss1_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Take a look at smooth effects...
pef <- getPEF(h2_gamlss1,term="clock_hour",parameter = "sigma")
curve(pef,from = 0,to = 23)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss1_params$mu) | is.na(h2_gamlss1_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNOtr(p = p,
                                                 mu=h2_gamlss1_params$mu[na_index],
                                                 sigma=h2_gamlss1_params$sigma[na_index])
  
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## Save forecasts for evaluation
JB_results[["gamlss-NOtr_v1"]] <- h2_mqr


## Quick plot and evaluation
issue <- unique(h2$issueTime)[9]
plot(h2_mqr[h2[,which(issueTime==issue)],-c(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


## GAMLSS v2 ####
#
# NB: fit takes approx. 15 min per fold

h2_gamlss2_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])

for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  train_data <- na.omit(h2[kfold!=fold & kfold!="Test",
                           .(issueTime,targetTime_UK,
                             n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)])
  
  h2_gamlss2 <- gamlss(data = train_data,
                       formula = n_attendance ~ ba(
                         ## v1 above:
                         # ~ dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                         #   s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
                         ## v2 here:
                         ~ dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
                           school_holiday + s(clock_hour,k=12,by=school_holiday,bs = "cr") +
                           s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T)
                       ),
                       ## Consider ba() here, and by dow/dow3
                       sigma.formula = ~ dow3*cs(clock_hour,df=12),
                       # sigma.formula = ~ba(~s(clock_hour,df=24,by=dow3,bs="cr")), # throws error...
                       family =  NOtr,
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)]
  
  temp_params <- predictAll(h2_gamlss2,newdata = test_data,
                            data = train_data)
  h2_gamlss2_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Take a look at smooth effects...
pef <- getPEF(h2_gamlss2,term="clock_hour",parameter = "sigma")
curve(pef,from = 0,to = 23)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss2_params$mu) | is.na(h2_gamlss2_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNOtr(p = p,
                                                 mu=h2_gamlss2_params$mu[na_index],
                                                 sigma=h2_gamlss2_params$sigma[na_index])
  
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## Save forecasts for evaluation
JB_results[["gamlss-NOtr_v2"]] <- h2_mqr


## Quick plot and evaluation
issue <- unique(h2$issueTime)[9]
plot(h2_mqr[h2[,which(issueTime==issue)],-c(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = as.numeric(h2$dow3),breaks = 9)
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))




## GAMLSS v3 ####
#
# NB: fit takes approx. 15 min per fold

h2_gamlss3_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])

for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  train_data <- na.omit(h2[kfold!=fold & kfold!="Test",
                           .(issueTime,targetTime_UK,
                             n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)])
  
  h2_gamlss3 <- gamlss(data = train_data,
                       formula = n_attendance ~ ba(
                         ## v1 above:
                         # ~ dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                         #   s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
                         ## v2 here:
                         ~ dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
                           school_holiday + s(clock_hour,k=12,by=school_holiday,bs = "cr") +
                           s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T)
                       ),
                       ## Consider ba() here, and by dow/dow3
                       sigma.formula = ~ dow3*cs(clock_hour,df=12),
                       # sigma.formula = ~ba(~s(clock_hour,df=24,by=dow3,bs="cr")), # throws error...
                       nu.formula = ~1,
                       family =  TF2tr,
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)]
  
  temp_params <- predictAll(h2_gamlss3,newdata = test_data,
                            data = train_data)
  h2_gamlss3_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Take a look at smooth effects...
pef <- getPEF(h2_gamlss3,term="clock_hour",parameter = "sigma")
curve(pef,from = 0,to = 23)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss3_params$mu) | is.na(h2_gamlss3_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qTF2tr(p = p,
                                                  mu=h2_gamlss3_params$mu[na_index],
                                                  sigma=h2_gamlss3_params$sigma[na_index],
                                                  nu=h2_gamlss3_params$nu[na_index])
  
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## Save forecasts for evaluation
JB_results[["gamlss-TF2tr_v3"]] <- h2_mqr


## Quick plot and evaluation
issue <- unique(h2$issueTime)[9]
plot(h2_mqr[h2[,which(issueTime==issue)],-c(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = as.numeric(h2$doy))
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


## GAMLSS v4 - Negative Binomial ####
#
# Approx 10 min per fold
#

h2_gamlss4_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])

for(fold in unique(h2$kfold)){
  
  print(paste(fold,Sys.time()))
  
  train_data <- na.omit(h2[kfold!=fold & kfold!="Test",
                           .(issueTime,targetTime_UK,
                             n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)])
  
  h2_gamlss4 <- gamlss(data = train_data,
                       formula = n_attendance ~ ba(
                         ## v1 above:
                         # ~ dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                         #   s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
                         ## v2 here:
                         ~ dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
                           school_holiday + s(clock_hour,k=12,by=school_holiday,bs = "cr") +
                           s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T)
                       ),
                       ## Consider ba() here, and by dow/dow3
                       sigma.formula = ~ dow3*cs(clock_hour,df=12),
                       # sigma.formula = ~ba(~s(clock_hour,df=24,by=dow3,bs="cr")), # throws error...
                       family =  NBI,
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)]
  
  temp_params <- predictAll(h2_gamlss4,newdata = test_data,
                            data = train_data)
  h2_gamlss4_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Take a look at smooth effects...
pef <- getPEF(h2_gamlss4,term="clock_hour",parameter = "sigma")
curve(pef,from = 0,to = 23)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss4_params$mu) | is.na(h2_gamlss4_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNBI(p = p,
                                                mu=h2_gamlss4_params$mu[na_index],
                                                sigma=h2_gamlss4_params$sigma[na_index])
  
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## Save forecasts for evaluation
JB_results[["gamlss-NBI_v4"]] <- h2_mqr


## Quick plot and evaluation
issue <- unique(h2$issueTime)[9]
plot(h2_mqr[h2[,which(issueTime==issue)],-c(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = as.numeric(h2$doy))
pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


## GBM... ####

# Maybe need to de-trend before GBM and replace after?
# Or lagged values/long term smooth?

h2_gbm_mqr <- MQR_gbm(data = h2,
                      formula = n_attendance ~ clock_hour + dow3 + school_holiday + T2T + doy,
                      quantiles = seq(0.05,0.95,by=0.05),
                      gbm_params = list(n.tree=500,
                                        interaction.depth=2,
                                        shrinkage=0.1,
                                        cv.folds=3),
                      cores = detectCores()-1)


issue <- unique(h2$issueTime)[9]
plot(h2_gbm_mqr[h2[,which(issueTime==issue)],],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_gbm_mqr,h2$n_attendance,kfolds = h2$kfold)
reliability(h2_gbm_mqr,h2$n_attendance,subsets = h2$clock_hour)

pinball(h2_gbm_mqr,h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))

JB_results[["GBM"]] <- cbind(h2[,.(issueTime,targetTime_UK)],h2_gbm_mqr)



## To do: qgam/mboost quantile regressions

# require(qgam) ## Super slow...
# 
# h2_mqr <- copy(h2[,.(issueTime,targetTime_UK,kfold)])
# for(fold in unique(h2$kfold)){
#   
#   for(p in 1:19/20){
#     
#     qgam1 <- qgam(n_attendance ~
#                     ## v1 above:
#                     # dow + s(clock_hour,k=24,by=dow,bs = "cr") +
#                     # s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
#                     ## v2 here:
#                     dow3 + s(clock_hour,k=24,by=dow3,bs = "cr") +
#                     school_holiday + s(clock_hour,k=6,by=school_holiday,bs = "cr") +
#                     s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T)
#                   data=h2[kfold!=fold & kfold!="Test",],
#                   qu=p,
#                   multicore=T)
#     
#     h2_mqr[kfold==fold,(paste0("q",p*100))=predict(qgam1,newdata =h2[kfold==fold,])]
#     
#   }
# }
# 
# class(h2_mqr) <- c("MultiQR",class(h2_mqr))
# 
# JB_results[["qgam"]] <- h2_mqr

## qgam with mboost ####

for(Ver in 1:2){
  
  h2_mboost_mqr <- qreg_mboost(data = h2[!is.na(T2T),],
                               formula = 
                                 if(Ver==1){
                                   ## v1 above:
                                   n_attendance ~ bols(dow) + 
                                     bbs(clock_hour,knots=24,df=20) +
                                     bbs(clock_hour,knots=24, by=dow,df=30,center = T) +
                                     bbs(doy,knots=6,by=t,df=6) +
                                     bbs(clock_hour,T2T,knots = 6,df=6)
                                 }else if(Ver==2){
                                   ## v2 here:
                                   n_attendance ~ bols(dow3) +
                                     bbs(clock_hour,knots=24,df=20) +
                                     bbs(clock_hour,knots=24, by=dow3,df=30,center = T) +
                                     bols(school_holiday) + bbs(clock_hour,knots=24,by=school_holiday,df=30,center = T) +
                                     bbs(doy,knots=6,by=t,df=6) +
                                     bbs(clock_hour,T2T,knots = 6,df=6)},
                               quantiles = seq(0.05,0.95,by=0.05),
                               cv_folds = "kfold",
                               cores = detectCores()-1,
                               pckgs = c("data.table"),
                               sort=T,
                               sort_limits = list(L=0,U=Inf),
                               control = mboost::boost_control(mstop = 500,nu=0.1))
  
  plot.varimp(varimp(h2_mboost_mqr$models$Test$q50))
  
  plot(h2_mboost_mqr$models$Test$q50$risk())
  
  issue <- unique(h2$issueTime)[403]
  plot(h2_mboost_mqr$mqr_pred[h2[,which(issueTime==issue)],],
       xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
       ylim=c(0,40),Legend = "topleft",q50_line = T)
  
  
  reliability(h2_mboost_mqr$mqr_pred,h2$n_attendance,kfolds = h2$kfold)
  reliability(h2_mboost_mqr$mqr_pred,h2$n_attendance,subsets = h2$clock_hour)
  
  pinball(h2_mboost_mqr$mqr_pred,h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))
  
  JB_results[[paste0("qreg_boost_V",Ver)]] <- cbind(h2[,.(issueTime,targetTime_UK)],h2_mboost_mqr$mqr_pred)
}


## Save Results ####

save(JB_results,file=paste0("../results/JethroResults_",Sys.Date(),".Rda"))



