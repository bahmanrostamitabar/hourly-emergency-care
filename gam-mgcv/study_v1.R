
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019

require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("jbrowell/ProbCast")
require(ProbCast)
require(readxl)


setwd(dirname(getActiveDocumentContext()$path))

## Load and Prep Data ####
# load("../data/hw_hourly.rds")
h2 <- fread("../data/h2_hourly.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"

add_calendar_variables(h2,datetimecol = "targetTime_UK")

# Load Holidays
hols <- as.data.table(read_xlsx("../data/holiday_rugby.xlsx"))
hols[,Date:=as.Date(Date)]
h2[,Date:=as.Date(targetTime_UK)]
h2 <- merge(h2,hols,by="Date",all.x=T)
rm(hols); h2[,Date:=NULL]
h2[,school_holiday:=as.factor(school_holiday)]
h2[,holiday_festive_day:=as.factor(holiday_festive_day)]
h2[,is_rug_in_Cardiff:=is_rug_in_Cardiff==1]
h2[,is_rug_out_Cardiff:=is_rug_out_Cardiff==1]

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

## Forece issuetimes into LocalTime. Not ideal but also not significant either...
h2[,issueTime:=lubridate::force_tz(issueTime,tzone = "Europe/London")]
h2[,targetTime:=NULL]
#h2[!(h2[,issueTime]%in%h2[,targetTime_UK]),]
h2 <- h2[(targetTime_UK-issueTime)/3600<=48,]
setcolorder(h2, c("issueTime","targetTime_UK"))
setkey(h2,"issueTime","targetTime_UK")



## Set-up CV - Test Data: 1/3/2018 to 28/2/2019
h2[issueTime>="2018-03-01" & issueTime<"2019-03-01",kfold:="Test"]
h2[issueTime<"2018-03-01",kfold:=paste0("fold",rep(rep(1:2,each=24*7),length.out=.N))]


## Features

# Weekday groups
h2[dow%in%c("Mon","Tue","Wed","Thu","Fri"),dow2:="MonFri"]
h2[!dow%in%c("Mon","Tue","Wed","Thu","Fri"),dow2:="SatSun"]
h2[,dow2:=as.factor(dow2)]


## Fit Poisson-GAM and visualise model ####

for(fold in unique(h2$kfold)){
  
  gam1 <- bam(n_attendance ~
                # s(clock_hour,k=24,by=dow2) + t + I(t^2) +
                # ti(doy,clock_hour,k=c(6,6)),
                te(doy,clock_hour,k=c(6,24),by=dow2) + t,
              data=h2[kfold!=fold & kfold!="Test",],family = poisson())
  
  h2[kfold==fold,lambda:=predict(gam1,newdata =h2[kfold==fold,],type="response")]
  
}

## Families: 
# gaussian()
# poisson()
# nb() / negbin() is useful for overdispersed count data, but computation is slow.
# So far: poisson a little better than other two...

## In-sample RMSE
sqrt(mean((gam1$y-gam1$fitted.values)^2))

## Check fit - is the model using all DOF? Is so, consider increasing availability...
gam.check(gam1)
plot(gam1,pages = 2)
plot(gam1,select = 3,scheme = 1)


## GAM VIS
gam1 <- getViz(gam1,nsim = 200)

check1D(gam1,"clock_hour")
check1D(gam1,"doy")
check2D(gam1,"dow2","clock_hour")
check2D(gam1,"doy","clock_hour")

## Any holiday effects?
check2D(gam1,h2[kfold!="Test",school_holiday],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam1,h2[kfold!="Test",holiday_festive_day],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#check2D(gam1,h2[,is_rug_in_Cardiff],"clock_hour")

## Any weather effects?
check1D(gam1,h2[,`2T`])
check2D(gam1,h2[,`2T`],"clock_hour")
check1D(gam1,h2[,TP])
check1D(gam1,h2[,SSRD])
check1D(gam1,h2[,LCC+MCC+HCC])
check1D(gam1,h2[,wind10m])
check2D(gam1,h2[,wind10m],"clock_hour")
check2D(gam1,h2[,wind10m],h2[,TP])


## Quantiles and evaluation ####

# h2_mqr <- data.table(q5=qpois(p = 0.05,lambda = h2[,lambda]) )
h2_mqr <- copy(h2[,.(issueTime,targetTime_UK)])
for(p in 1:19/20){
  h2_mqr[[paste0("q",p*100)]] <- qpois(p = p,lambda = h2[,lambda]) 
}; class(h2_mqr) <- c("MultiQR",class(h2_mqr))


issue <- unique(h2$issueTime)[7]
plot(h2_mqr[issueTime==issue,-(1:2)],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_mqr[,-c(1:2)],h2$n_attendance)
reliability(h2_mqr[,-c(1:2)],h2$n_attendance,subsets = h2$clock_hour)

pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


# save(h2_mqr,file = "../data/example_forecast_format.R")


## GAMLSS ####

require(gamlss.add) # for ga()
# check out http://opisthokonta.net/?p=1157
# Double Poisson DPO
# Negative Binomial NBI
# 

h2_gamlss <- Para_gamlss(data = h2,
                         formula = n_attendance ~ 
                           pb(clock_hour) + pb(doy,df=6) + t,
                         # pvc(clock_hour,by=dow2,df=24) + pb(doy,by=clock_hour,df=6), # super slow...
                         sigma.formula = ~pb(clock_hour),
                         family =  NBI,#DPO, #NO,  #
                         method=mixed(20,10))

h2_gamlss_mqr <- PPD_2_MultiQR(data=h2,
                               models = h2_gamlss,
                               params = F)

issue <- unique(h2$issueTime)[9]
plot(h2_gamlss_mqr[h2[,which(issueTime==issue)],],
     xlab="Lead-time [hours]",ylab="Attendance",main=paste0("Origin: ",issue," (",format(issue,"%A"),")"),
     ylim=c(0,40),Legend = "topleft")


reliability(h2_gamlss_mqr,h2$n_attendance)
reliability(h2_gamlss_mqr,h2$n_attendance,subsets = h2$clock_hour)

pinball(h2_gamlss_mqr,h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2))


## All Eval ####


PB <- as.data.table(pinball(h2_mqr[,-c(1:2)],h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2)))
Rel <- as.data.table(reliability(h2_mqr[,-c(1:2)],h2$n_attendance))
PB[,Method:="Poisson"]
Rel[,Method:="Poisson"]

temp <- as.data.table(reliability(h2_gamlss_mqr,h2$n_attendance))
temp[,Method:="Negative Binomial"]
Rel <- rbind(Rel,temp); rm(temp)
temp <- as.data.table(pinball(h2_gamlss_mqr,h2$n_attendance,kfolds = h2$kfold,ylim=c(0.3,2)))
temp[,Method:="Negative Binomial"]
PB <- rbind(PB,temp); rm(temp)



ggplot(data = PB[kfold=="All_cv",],aes(y=`Loss`,x=Quantile,group=Method)) +
  geom_line(aes(linetype=Method, color=Method)) + 
  theme_bw() + theme(legend.position="bottom")

ggplot(data = Rel[kfold=="All",],aes(y=Empirical,x=Nominal,group=Method)) +
  geom_line(aes(linetype=Method, color=Method)) + 
  theme_bw() + theme(legend.position="bottom")





