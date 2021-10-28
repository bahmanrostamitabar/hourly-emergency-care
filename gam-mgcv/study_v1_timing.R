
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019


## TO DO ####
#
# Models for sigma in NOtr
# Alternative dist in gamlss
# qgam()
# Extract expectation for RMSE. Column name "expectation".

## Begin... ####
require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
remotes::install_github("jbrowell/ProbCast")
require(ProbCast)
require(readxl)


## Utilities for fitting truncated normal
require(gamlss.tr)
require(gamlss.add)
gen.trun(par = c(0),family = NO,type="left")
gen.trun(par = c(0),family = TF2,type="left")
# Function to calculate mean of truncated normal and t, and estimate from quantiles
mean_t_norm <- function(mu,sigma,a,b){
  
  mu + sigma*(dnorm((a-mu)/sigma) - dnorm((b-mu)/sigma))/
    (pnorm(b)-pnorm(a))
  
}

mean_t_t <- function(mu,sigma,nu,a,b){
  # from https://doi.org/10.1016/j.jspi.2011.06.006
  
  if(any(nu<=1,na.rm = T)){stop("All nu must be greater than 1 in mean_t_t()")}
  
  ## For standard ~ mu=1; sigma=0
  a_x <- a*sigma+mu
  b_x <- b*sigma+mu
  tau_0 <- 1
  alp_0 <- pt(b*tau_0^0.5,df = nu) - pt(a*tau_0^0.5,df = nu)
  k <- gamma((nu+1)/2)/(alp_0*gamma(nu/2)*(nu*pi)^0.5)
  X_bar <- (k*nu/(nu-1))*( (1+a_x^2/nu)^((1-nu)/2) - (1+b_x^2/nu)^((1-nu)/2) )
  
  ## Re-scale:
  mu + sigma*X_bar
  
}

mean_from_qs <- function(mqr){
  
  q_cols <- grep("q",colnames(mqr))
  qs <- as.numeric(gsub("q","",colnames(mqr)[q_cols]))/100
  diff_left <- diff(c(0,qs))
  
  mqr$expectation <- mqr[[q_cols[1]]]*diff_left[1]
  for(qq in 2:length(q_cols)){
    mqr$expectation <- mqr$expectation + mqr[[q_cols[qq]]]*diff_left[qq]
  }
  
  return(mqr$expectation)
}

setwd(dirname(getActiveDocumentContext()$path))

JB_results_time <- list()
# load("../results/JethroResults_2021-05-05.Rda")






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

## Benchmark 1 ####
# Empirical distribution by hour of the day and day type

## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%

# Quantiles
BenchPred <- h2[kfold!="Test",as.list(quantile(n_attendance,probs = 1:19/20)),by=c("dow","clock_hour")]
setnames(BenchPred,paste0((1:19/20)*100,"%"),paste0("q",(1:19/20)*100))

# Expectation and merge with quantiles
BenchPred_exp <- h2[kfold!="Test",.(expectation=mean(n_attendance)),by=c("dow","clock_hour")]
BenchPred <- merge(BenchPred,BenchPred_exp,by=c("dow","clock_hour"))

Bench_mqr <- copy(h2[,.(issueTime,targetTime_UK,dow,clock_hour)])
Bench_mqr <- merge(Bench_mqr,BenchPred,by=c("dow","clock_hour"),all.x=T)
Bench_mqr <- Bench_mqr[,-(1:2)]
setkey(Bench_mqr,issueTime,targetTime_UK)
class(Bench_mqr) <- c("MultiQR",class(Bench_mqr))

## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["Benchmark_1"]] <- time_temp
## %%%% Time for Ivan %%%%


rm(BenchPred,BenchPred_exp,Bench_mqr)

## Benchmark 2 ####
# as above with rolling update

test_start <- h2[kfold=="Test",min(issueTime)]
Bench_mqr <- copy(h2[,.(issueTime,targetTime_UK,dow,clock_hour)])


## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


for(test_week in 0){
  
  ## Analog based on past 52 weeks
  BenchPred <- h2[issueTime<test_start+(test_week-1)*3600*24*7 &
                    issueTime>test_start+(test_week-1)*3600*24*7 - 3600*24*7*52,
                  as.list(quantile(n_attendance,probs = 1:19/20)),by=c("dow","clock_hour")]
  setnames(BenchPred,paste0((1:19/20)*100,"%"),paste0("q",(1:19/20)*100))
  
  # Expectation and merge with quantiles
  BenchPred_exp <- h2[issueTime<test_start+(test_week-1)*3600*24*7 &
                        issueTime>test_start+(test_week-1)*3600*24*7 - 3600*24*7*52,
                      .(expectation=mean(n_attendance)),by=c("dow","clock_hour")]
  BenchPred <- merge(BenchPred,BenchPred_exp,by=c("dow","clock_hour"))
  
  
  
  Bench_mqr_temp <- merge(Bench_mqr[issueTime>=test_start+(test_week-1)*3600*24*7,.(issueTime,targetTime_UK,dow,clock_hour)],
                          BenchPred,by=c("dow","clock_hour"),all.x=T)
  
  Bench_mqr <- rbind(Bench_mqr[issueTime<test_start+(test_week-1)*3600*24*7,],
                     Bench_mqr_temp,fill=T)
  
  
}; rm(Bench_mqr_temp,BenchPred,BenchPred_exp)
Bench_mqr <- Bench_mqr[,-(3:4)]
setkey(Bench_mqr,issueTime,targetTime_UK)
class(Bench_mqr) <- c("MultiQR",class(Bench_mqr))



## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["Benchmark_2"]] <- time_temp
## %%%% Time for Ivan %%%%


## Fit Poisson-GAM and visualise model ####

## Version 1: All days, no holiday effects, no smoothed lags
# fold = "fold1"


## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


for(fold in "Test"){
  
  print(paste(fold,Sys.time()))
  
  gam1 <- bam(n_attendance ~
                # s(clock_hour,k=24,by=dow2) + t + I(t^2) +
                # ti(doy,clock_hour,k=c(6,6)),
                # te(doy,clock_hour,k=c(6,24),by=dow2) + t,
                dow + s(clock_hour,k=24,by=dow,bs = "cr") +
                s(doy,k=6,by=t,bs = "cr") + te(clock_hour,T2T),
              # family = ziP(link = "identity"), # error
              family = poisson(link = "log"), # log, sqrt
              # family = poisson(link = "identity"), # error
              data=h2[kfold!=fold & kfold!="Test",],discrete = T) 
  
  h2[kfold==fold,lambda:=predict(gam1,newdata =h2[kfold==fold,],type="response")]
  
}

h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
for(p in 1:19/20){
  h2_mqr[[paste0("q",p*100)]] <- qpois(p = p,lambda = h2[,lambda]) 
}
h2_mqr$expectation <- h2$lambda
class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["Poisson-GAM-te_v1"]] <- time_temp
## %%%% Time for Ivan %%%%





## Version 2: Grouped days, some holiday effects, no smooted lags ####

## School hols and public hol dummies

## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


for(fold in "Test"){
  
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

h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
for(p in 1:19/20){
  h2_mqr[[paste0("q",p*100)]] <- qpois(p = p,lambda = h2[,lambda]) 
}
h2_mqr$expectation <- h2$lambda
class(h2_mqr) <- c("MultiQR",class(h2_mqr))


## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["Poisson-GAM-te_v2"]] <- time_temp
## %%%% Time for Ivan %%%%




## GAMLSS ####

## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


h2_gamlss1_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])
for(fold in "Test"){
  
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
                       # family = PO(mu.link = "identity"),
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T)]
  
  temp_params <- predictAll(h2_gamlss1,newdata = test_data,
                            data = train_data)
  h2_gamlss1_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss1_params$mu) | is.na(h2_gamlss1_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNOtr(p = p,
                                                 mu=h2_gamlss1_params$mu[na_index],
                                                 sigma=h2_gamlss1_params$sigma[na_index])
  
}

h2_mqr$expectation <- NA
h2_mqr$expectation[na_index] <- mean_t_norm(mu=h2_gamlss1_params$mu[na_index],
                                            sigma=h2_gamlss1_params$sigma[na_index],
                                            a=0,b=Inf)

class(h2_mqr) <- c("MultiQR",class(h2_mqr))



## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["gamlss-NOtr_v1"]] <- time_temp
## %%%% Time for Ivan %%%%



## GAMLSS v2 ####
#
# NB: fit takes approx. 15 min per fold

h2_gamlss2_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])


## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


for(fold in "Test"){
  
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

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss2_params$mu) | is.na(h2_gamlss2_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNOtr(p = p,
                                                 mu=h2_gamlss2_params$mu[na_index],
                                                 sigma=h2_gamlss2_params$sigma[na_index])
  
}

h2_mqr$expectation <- NA
h2_mqr$expectation[na_index] <- mean_t_norm(mu=h2_gamlss1_params$mu[na_index],
                                            sigma=h2_gamlss1_params$sigma[na_index],
                                            a=0,b=Inf)

class(h2_mqr) <- c("MultiQR",class(h2_mqr))


## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["gamlss-NOtr_v2"]] <- time_temp
## %%%% Time for Ivan %%%%



## GAMLSS v3 ####
#
# NB: fit takes approx. 15 min per fold

h2_gamlss3_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])


## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


for(fold in "Test"){
  
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

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss3_params$mu) | is.na(h2_gamlss3_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qTF2tr(p = p,
                                                  mu=h2_gamlss3_params$mu[na_index],
                                                  sigma=h2_gamlss3_params$sigma[na_index],
                                                  nu=h2_gamlss3_params$nu[na_index])
  
}

h2_mqr$expectation <- NA
h2_mqr$expectation[na_index] <- mean_t_t(mu=h2_gamlss1_params$mu[na_index],
                                         sigma=h2_gamlss1_params$sigma[na_index],
                                         nu=h2_gamlss3_params$nu[na_index],
                                         a=0,b=Inf)

class(h2_mqr) <- c("MultiQR",class(h2_mqr))


## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["gamlss-TF2tr_v3"]] <- time_temp
## %%%% Time for Ivan %%%%


## GAMLSS v4 - Negative Binomial ####
#
# Approx 10 min per fold
#

h2_gamlss4_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])

## %%%% Time for Ivan %%%%
time_temp <- Sys.time() 
## %%%% Time for Ivan %%%%


for(fold in "Test"){
  
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
                       sigma.formula = ~ cs(clock_hour,df=12),
                       family =  NBI(mu.link = "identity"),
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T,school_holiday)]
  
  temp_params <- predictAll(h2_gamlss4,newdata = test_data,
                            data = train_data)
  h2_gamlss4_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)


## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss4_params$mu) | is.na(h2_gamlss4_params$sigma)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qNBI(p = p,
                                                mu=h2_gamlss4_params$mu[na_index],
                                                sigma=h2_gamlss4_params$sigma[na_index])
  
}

# Add expectation
h2_mqr$expectation[na_index] <- h2_gamlss4_params$mu[na_index]
class(h2_mqr) <- c("MultiQR",class(h2_mqr))


## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["gamlss-NBI_Ilink_v4"]] <- temp_time
## %%%% Time for Ivan %%%%


## GAMLSS PO - identity link ####
#
# ~1min per fold
#
h2_gamlss1_params <- copy(h2[,.(issueTime,targetTime_UK,kfold)])

## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%

for(fold in "Test"){
  
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
                       family = PO(mu.link = "identity"),
                       method=mixed(10,20),
                       control=gamlss.control(c.crit = 0.1))
  
  test_data <- h2[kfold==fold,.(issueTime,targetTime_UK,
                                n_attendance,dow,dow3,clock_hour,t,doy,T2T)]
  
  temp_params <- predictAll(h2_gamlss1,newdata = test_data,
                            data = train_data)
  h2_gamlss1_params[kfold==fold,names(temp_params) := temp_params] 
  
}; rm(temp_params,test_data)

## Form quantile forecasts
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
na_index <- which(!(is.na(h2_gamlss1_params$mu)))
for(p in 1:19/20){
  
  h2_mqr[[paste0("q",p*100)]] <- NA
  h2_mqr[[paste0("q",p*100)]][na_index] <- qPO(p = p,mu =h2_gamlss1_params$mu[na_index])
  
}

# Add expectation
h2_mqr$expectation[na_index] <- h2_gamlss1_params$mu[na_index]
class(h2_mqr) <- c("MultiQR",class(h2_mqr))

## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["gamlss-PO_Ilink_v1"]] <- temp_time
## %%%% Time for Ivan %%%%




## GBM... ####

# Maybe need to de-trend before GBM and replace after?
# Or lagged values/long term smooth?


## %%%% Time for Ivan %%%%
time_temp <- Sys.time()
## %%%% Time for Ivan %%%%


# h2_gbm_mqr <- MQR_gbm(data = h2,
#                       formula = n_attendance ~ clock_hour + dow3 + school_holiday + T2T + doy,
#                       quantiles = seq(0.05,0.95,by=0.05),
#                       gbm_params = list(n.tree=500,
#                                         interaction.depth=2,
#                                         shrinkage=0.1,
#                                         cv.folds=3),
#                       cores = detectCores()-1)

h2_gbm_mqr <- MQR_gbm(data = h2[kfold!="Test"],CVfolds = NULL,
                      formula = n_attendance ~ clock_hour + dow3 + school_holiday + T2T + doy,
                      quantiles = seq(0.05,0.95,by=0.05),
                      gbm_params = list(n.tree=500,
                                        interaction.depth=2,
                                        shrinkage=0.1,
                                        cv.folds=3),
                      cores = detectCores()-1)

expectation <- mean_from_qs(mqr = cbind(h2[kfold!="Test",.(issueTime,targetTime_UK)],h2_gbm_mqr))

## %%%% Time for Ivan %%%%
time_temp <- Sys.time() - time_temp
JB_results_time[["GBM"]] <- time_temp
## %%%% Time for Ivan %%%%


## qgam with mboost ####

for(Ver in 1:2){
  
  ## %%%% Time for Ivan %%%%
  time_temp <- Sys.time()
  ## %%%% Time for Ivan %%%%
  
  
  # h2_mboost_mqr <- qreg_mboost(data = h2[!is.na(T2T),],
  #                              formula = 
  #                                if(Ver==1){
  #                                  ## v1 above:
  #                                  n_attendance ~ bols(dow) + 
  #                                    bbs(clock_hour,knots=24,df=20) +
  #                                    bbs(clock_hour,knots=24, by=dow,df=30,center = T) +
  #                                    bbs(doy,knots=6,by=t,df=6) +
  #                                    bbs(clock_hour,T2T,knots = 6,df=6)
  #                                }else if(Ver==2){
  #                                  ## v2 here:
  #                                  n_attendance ~ bols(dow3) +
  #                                    bbs(clock_hour,knots=24,df=20) +
  #                                    bbs(clock_hour,knots=24, by=dow3,df=30,center = T) +
  #                                    bols(school_holiday) + bbs(clock_hour,knots=24,by=school_holiday,df=30,center = T) +
  #                                    bbs(doy,knots=6,by=t,df=6) +
  #                                    bbs(clock_hour,T2T,knots = 6,df=6)},
  #                              quantiles = seq(0.05,0.95,by=0.05),
  #                              cv_folds = "kfold",
  #                              cores = detectCores()-1,
  #                              pckgs = c("data.table"),
  #                              sort=T,
  #                              sort_limits = list(L=0,U=Inf),
  #                              control = mboost::boost_control(mstop = 500,nu=0.1))
  
  h2_mboost_mqr <- qreg_mboost(data = h2[!is.na(T2T) & kfold!="Test",],
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
                               cv_folds = NULL,
                               cores = detectCores()-1,
                               pckgs = c("data.table"),
                               sort=T,
                               sort_limits = list(L=0,U=Inf),
                               control = mboost::boost_control(mstop = 500,nu=0.1))
  
  
  
  expectation <- mean_from_qs(mqr = cbind(h2[kfold!="Test",.(issueTime,targetTime_UK)],h2_mboost_mqr$mqr_pred))
  
  ## %%%% Time for Ivan %%%%
  time_temp <- Sys.time() - time_temp
  JB_results_time[[paste0("qreg_boost_V",Ver)]] <- time_temp
  ## %%%% Time for Ivan %%%%
  

  
  
  
}





## Save Results ####

save(JB_results_time,file="../results/JethroResults_time.Rda")
