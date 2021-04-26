rm(list=ls())
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019

## To do:
# add checks that exactly same data are being evaluated
# evaluate training data as well as test
# evaluate by lead-time and other factors
# test ceiling/floor options for non-integer forecasts
# report RMSE


## Link to use case:
# "Process 95% of patients in 4h"

require(rstudioapi)
require(data.table)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("jbrowell/ProbCast")
require(ProbCast)
setwd(dirname(getActiveDocumentContext()$path))



## Load A&E Arrival Data ####
h2 <- fread("../data/h2_hourly_gb.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"


## Initialis Containers ####
test_start <- "2018-03-01"
PB <- data.table()
REL <- data.table()

## Jethro's Results ####

load("JethroResults_2021-04-26.Rda")


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

tbats <- data.table(readRDS("tbats.rds"))
setnames(tbats,old = c("origin","target"),c("issueTime","targetTime_UK"))
## Get Actuals
actuals <- merge(tbats[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                 by="targetTime_UK",all.x=T)
setkey(actuals,issueTime,targetTime_UK)

## Test Data Pinball & Reliability
temp <- data.table(pinball(tbats[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="tbats"]; temp[,kfold:="Test"]
PB <- rbind(PB,temp); rm(temp)

temp <- data.table(reliability(tbats[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="tbats"]; temp[,kfold:="Test"]
REL <- rbind(REL,temp); rm(temp)

rm(tbats)


tbats <- data.table(readRDS("tbats_refit.rds"))
setnames(tbats,old = c("origin","target"),c("issueTime","targetTime_UK"))
## Get Actuals
actuals <- merge(tbats[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
                 by="targetTime_UK",all.x=T)
setkey(actuals,issueTime,targetTime_UK)

## Test Data Pinball & Reliability
temp <- data.table(pinball(tbats[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="tbats_refit"]; temp[,kfold:="Test"]
PB <- rbind(PB,temp); rm(temp)

temp <- data.table(reliability(tbats[,-c(1:2)],actuals[,n_attendance]))
temp[,Method:="tbats_refit"]; temp[,kfold:="Test"]
REL <- rbind(REL,temp); rm(temp)




# Prophet <- data.table(readRDS("forecast_prophet.rds"))
# setnames(Prophet,colnames(Prophet),c("issueTime","targetTime_UK",paste0("q",as.numeric(colnames(Prophet[,-c(1:2)]))*100)))
# class(Prophet) <- c("MultiQR",class(Prophet))
# 
# ## Get Actuals
# actuals <- merge(Prophet[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
#                  by="targetTime_UK",all.x=T)
# setkey(actuals,issueTime,targetTime_UK)
# 
# ## Test Data Pinball & Reliability
# temp <- data.table(pinball(Prophet[,-c(1:2)],actuals[,n_attendance]))
# temp[,Method:="Prophet"]; temp[,kfold:="Test"]
# PB <- rbind(PB,temp); rm(temp)
# 
# temp <- data.table(reliability(Prophet[,-c(1:2)],actuals[,n_attendance]))
# temp[,Method:="Prophet"]; temp[,kfold:="Test"]
# REL <- rbind(REL,temp); rm(temp)
# 
# rm(Prophet)

# % #

# Fasster <- data.table(readRDS("forecast_fasster.rds"))
# 
# class(Fasster) <- c("MultiQR",class(Fasster))
# 
# ## Get Actuals
# actuals <- merge(Fasster[targetTime_UK>=test_start,.(issueTime,targetTime_UK)],h2[,.(targetTime_UK,n_attendance)],
#                  by="targetTime_UK",all.x=T)
# setkey(actuals,issueTime,targetTime_UK)
# 
# ## Test Data Pinball & Reliability
# temp <- data.table(pinball(Fasster[,-c(1:2)],actuals[,n_attendance]))
# temp[,Method:="Fasster"]; temp[,kfold:="Test"]
# PB <- rbind(PB,temp); rm(temp)
# 
# temp <- data.table(reliability(Fasster[,-c(1:2)],actuals[,n_attendance]))
# temp[,Method:="Fasster"]; temp[,kfold:="Test"]
# REL <- rbind(REL,temp); rm(temp)
# 
# rm(Fasster)


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
  geom_line() + geom_point() + ylab("Pinball Loss") + 
  ggtitle("Pinball Loss") + theme_bw()
ggsave("Pinball.png")

ggplot(data=PB[Method!="Benchmark_1",],aes(x=Quantile,y=Loss,group=Method,shape=Method,color=Method)) +
  geom_line() + geom_point() + ylab("Pinball Loss") + 
  ggtitle("Pinball Loss") + theme_bw()
ggsave("Pinball_noBench.png")

## Reliability
REL_nom <- data.table(Nominal=seq(0,1,by=0.05),
                         Empirical=seq(0,1,by=0.05),
                      `Quantile Bias`= 0,
                         Method="Nominal")

ggplot(data=REL,aes(x=Nominal,y=Empirical,group=Method,color=Method)) +
  geom_line(data=REL_nom,aes(x=Nominal,y=Empirical), color="black",size=1.1,show.legend = F) +
  geom_line() + geom_point() +
  xlim(c(0,1)) + ylim(c(0,1)) + ggtitle("Reliability Diagram") +
  theme_bw() 
ggsave("Reliability.png")
  
## Quantile Bias

REL[,`Quantile Bias`:=Empirical-Nominal]

ggplot(data=REL,aes(x=Nominal,y=`Quantile Bias`,group=Method,color=Method)) +
  geom_line(data=REL_nom,aes(x=Nominal,y=`Quantile Bias`), color="black",size=1.1,show.legend = F) +
  geom_line() + geom_point() +
  xlim(c(0,1)) + ylim(c(-0.2,0.2)) + ggtitle("Quantile Bias") +
  theme_bw() 
ggsave("QuantileBias.png")
