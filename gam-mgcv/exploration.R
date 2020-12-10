
## Forecast every 12h (00 and 12 local time)
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019

require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)
require(ggplot2)
# library(devtools)
# install_github("jbrowell/ProbCast")
require(ProbCast)
require(readxl)

setwd(dirname(getActiveDocumentContext()$path))

## Load and Prep Data ####
# load("../data/hw_hourly.rds")
# h2 <- fread("../data/h2_hourly.csv")
# h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
# h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"
h2 <- fread("../data/h2_hourly_gb.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"

add_calendar_variables(h2,datetimecol = "targetTime_UK")

# Load Holidays
hols <- as.data.table(read_xlsx("../data/holiday_rugby_all.xlsx"))
hols[,Date:=as.Date(Date)]
h2[,Date:=as.Date(targetTime_UK)]
h2 <- merge(h2,hols,by="Date",all.x=T)
rm(hols); h2[,Date:=NULL]
h2[,school_holiday:=as.factor(school_holiday)]
h2[,holiday_festive_day:=as.factor(festive_day)]
h2[,is_rug_in_Cardiff:=is_rug_in_Cardiff==1]
h2[,is_rug_out_Cardiff:=is_rug_out_Cardiff==1]

#load weather data
weather_data <- data.table()
for(f in c(list.files(path="../data/",pattern = "weather",full.names = T))){
  load(f)
  weather_data <- rbind(weather_data,features)
}
h2 <- merge(weather_data,h2,by = "targetTime")
rm(f,features,weather_data)
h2[,wind10m:=sqrt(`10U`^2+`10V`^2)]


## Fit GAM and visualise model ####

gam1 <- bam(n_attendance ~ s(clock_hour,k=24,by=dow) +
              s(doy,k=26) +
              ti(doy,clock_hour,k=c(6,6)),
            data=h2,family = poisson())

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
plot(gam1,select = 8,scheme = 1)

gam1 <- getViz(gam1,nsim = 200)


check1D(gam1,"clock_hour")
check1D(gam1,"doy")
check2D(gam1,"dow","clock_hour")
check2D(gam1,"doy","clock_hour")

## Any holiday effects?
check2D(gam1,h2[,school_holiday],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam1,h2[,holiday_festive_day],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
## Check for leading + lagging days
h2_temp <- copy(h2)
h2_temp[,temp_T:=targetTime-24*60*60]
h2_temp[,holiday_festive_day_lag1:=holiday_festive_day]
h2 <- merge(h2,h2_temp[,.(temp_T,issueTime,holiday_festive_day_lag1)],by.x=c("targetTime","issueTime"),by.y = c("temp_T","issueTime"),all.x = T)
h2_temp <- copy(h2)
h2_temp[,temp_T:=targetTime+24*60*60]
h2_temp[,holiday_festive_day_lead1:=holiday_festive_day]
h2 <- merge(h2[,-c(24:25)],h2_temp[,.(temp_T,issueTime,holiday_festive_day_lead1)],by.x=c("targetTime","issueTime"),by.y = c("temp_T","issueTime"),all.x = T)
rm(h2_temp)

check2D(gam1,h2[,school_holiday],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam1,h2[,holiday_festive_day],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam1,h2[,holiday_festive_day_lead1],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
check2D(gam1,h2[,holiday_festive_day_lag1],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



check2D(gam1,h2[,holiday_festive_day_lag1],"clock_hour") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##


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
