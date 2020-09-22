
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019

require(rstudioapi)
require(data.table)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("jbrowell/ProbCast")
require(ProbCast)
setwd(dirname(getActiveDocumentContext()$path))



## Load A&E Arrival Data ####
h2 <- fread("../data/h2_hourly.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"


## Initialis Containers ####
test_start <- "2018-03-01"
PB <- data.table()
REL <- data.table()

## Jethro's Results ####

load("JethroResults_2020-09-22.Rda")


for(n in names(JB_results)){
  
  ## Get Actuals
  actuals <- merge(JB_results[[n]][targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                   by="targetTime_UK",all.x=T,no.dups = F)
  setkey(actuals,issueTime,targetTime_UK)
  
  ## Test Data Pinball & Reliability
  temp <- data.table(pinball(JB_results[[n]][targetTime_UK>=test_start,-c(1:2)],actuals[,n_attendance]))
  temp[,Method:=n]; temp[,kfold:="Test"]
  PB <- rbind(PB,temp); rm(temp)
  
  temp <- data.table(reliability(JB_results[[n]][targetTime_UK>=test_start,-c(1:2)],actuals[,n_attendance]))
  temp[,Method:=n]; temp[,kfold:="Test"]
  REL <- rbind(REL,temp); rm(temp)
}

rm(JB_results)

## Bahman's Results ####

Prophet <- data.table(readRDS("forecast_prophet.rds"))
setnames(Prophet,colnames(Prophet),c("issueTime","targetTime_UK",paste0("q",as.numeric(colnames(Prophet[,-c(1:2)]))*100)))
class(Prophet) <- c("MultiQR",class(Prophet))

## Get Actuals
actuals <- merge(Prophet[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                 by="targetTime_UK",all.x=T)
setkey(actuals,issueTime,targetTime_UK)

## Test Data Pinball & Reliability
temp <- data.table(pinball(Prophet[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="Prophet"]; temp[,kfold:="Test"]
PB <- rbind(PB,temp); rm(temp)

temp <- data.table(reliability(Prophet[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="Prophet"]; temp[,kfold:="Test"]
REL <- rbind(REL,temp); rm(temp)

rm(Prophet)

# % #

Fasster <- data.table(readRDS("forecast_fasster.rds"))

class(Fasster) <- c("MultiQR",class(Fasster))

## Get Actuals
actuals <- merge(Fasster[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                 by="targetTime_UK",all.x=T)
setkey(actuals,issueTime,targetTime_UK)

## Test Data Pinball & Reliability
temp <- data.table(pinball(Fasster[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="Fasster"]; temp[,kfold:="Test"]
PB <- rbind(PB,temp); rm(temp)

temp <- data.table(reliability(Fasster[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="Fasster"]; temp[,kfold:="Test"]
REL <- rbind(REL,temp); rm(temp)

rm(Fasster)


## Ivan's Results ####


load("IvanValues.RData")


for(n in names(quantileValuesIvan)){
  
  QR_data <- data.table(quantileValuesIvan[[n]])
  setkey(QR_data,issueTime,targetTime_UK)
  class(QR_data) <- c("MultiQR",class(QR_data))
  
  ## Get Actuals
  actuals <- merge(QR_data[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                   by="targetTime_UK",all.x=T,no.dups = F)
  setkey(actuals,issueTime,targetTime_UK)
  
  ## Test Data Pinball & Reliability
  temp <- data.table(pinball(QR_data[targetTime_UK>=test_start,-c(1:2)],actuals[,n_attendance]))
  temp[,Method:=n]; temp[,kfold:="Test"]
  PB <- rbind(PB,temp); rm(temp)
  
  temp <- data.table(reliability(QR_data[targetTime_UK>=test_start,-c(1:2)],actuals[,n_attendance]))
  temp[,Method:=n]; temp[,kfold:="Test"]
  REL <- rbind(REL,temp); rm(temp)
  
  rm(QR_data)
}

rm(quantileValuesIvan)



## Visualise all Results ####

## Pinball
ggplot(data=PB,aes(x=Quantile,y=Loss,group=Method,shape=Method,color=Method)) +
  geom_line() + geom_point() + ylab("Pinball Loss")


## Reliability


