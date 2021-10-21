# ADAM experiment
remotes::install_github("config-i1/greybox", upgrade="never", dependencies=FALSE)
remotes::install_github("config-i1/smooth", upgrade="never", dependencies=FALSE)

require(data.table)
require(readxl)

library(zoo)
library(greybox)
library(smooth)
library(foreach)
library(doMC)
library(ProbCast)

## Load and Prep Data ####
# load("../data/hw_hourly.rds")
h2 <- fread("data/h2_hourly_gb.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"

add_calendar_variables(h2,datetimecol = "targetTime_UK")

# Load Holidays
hols <- as.data.table(read_xlsx("data/holiday_rugby.xlsx"))
hols[,Date:=as.Date(Date)]
h2[,Date:=as.Date(targetTime_UK)]
h2 <- merge(h2,hols,by="Date",all.x=T)
rm(hols); h2[,Date:=NULL]
h2[,school_holiday:=as.factor(school_holiday)]
h2[,holiday_festive_day:=as.factor(holiday_festive_day)]
h2[,is_rug_in_Cardiff:=is_rug_in_Cardiff==1]
h2[,is_rug_out_Cardiff:=is_rug_out_Cardiff==1]

# Prepare explanatory variables
xreg <- as.character(h2$holiday_festive_day)
# Do leads and lags of the variables
xreg <- xregExpander(xreg, lags=c(-2:1)*24, gaps="NAs")
xreg <- as.data.table(xreg)
xreg$x[is.na(xreg$x)] <- "none"
xreg$xLag24[is.na(xreg$xLag24)] <- "none"
xreg$xLag48[is.na(xreg$xLag48)] <- "none"
xreg$xLead24[is.na(xreg$xLead24)] <- "none"
xreg$x <- factor(xreg$x)
xreg$xLag24 <- factor(xreg$xLag24)
xreg$xLag48 <- factor(xreg$xLag48)
xreg$xLead24 <- factor(xreg$xLead24)

# Create zoo objects
# The first observations is from 31st March
y <- zoo(h2$n_attendance, order.by=h2$targetTime)
# xreg <- zoo(xreg, order.by=h2$targetTime)
xregData <- data.table(y=y,xreg)

#### Expand the data and create the table with dummies 
# xregExpanded <- cbind(y,model.matrix(~xreg-1))
xregExpanded <- data.table(y=y,xreg)
# Include days of week and months of year
xregExpanded <- cbind(xregExpanded,
                      hourOfDay=as.factor(temporaldummy(y,type="hour",of="day",factors=TRUE)),
                      dayOfWeek=as.factor(temporaldummy(y,type="day",of="week",factors=TRUE)),
                      weekOfYear=as.factor(temporaldummy(y,type="week",of="year",factors=TRUE)))

#### !!! Include fourier for the hour of year in regression !!! ####

#### Expand the matrix, creating dummy variables from the factors
xregDummies <- cbind(xregExpanded[,1],
                     model.matrix(~x+xLag48+xLag24+xLead24,xregExpanded)[,-1],
                     xregExpanded[,6:8])
colnames(xregDummies) <- make.names(colnames(xregDummies), unique=TRUE)
# xregDummiesShort <- xregDummies

# Check, which variables are perfectly correlated
correlatedVariables <- vector("logical",ncol(xregDummiesShort)-1)
for(i in 2:ncol(xregDummiesShort)){
  correlatedVariables[i-1] <- any(apply(xregDummiesShort[,-c(1,i)]==xregDummiesShort[,i],2,all));
}
# Remove the ones that are perfectly correlated
while(any(correlatedVariables)){
  correlatedVariables <- vector("logical",ncol(xregDummiesShort)-1)
  for(i in 2:ncol(xregDummiesShort)){
    correlatedVariables[i-1] <- any(apply(xregDummiesShort[,-c(1,i)]==xregDummiesShort[,i],2,all));
  }
  # Kick off the last most correlated
  if(any(correlatedVariables)){
    xregDummiesShort <- xregDummiesShort[,-tail(which(correlatedVariables),1)-1];
  }
  cat(sum(correlatedVariables), "left; ");
}

stepwiseRegression <- stepwise(xregDummiesShort)

save(xreg, xregExpanded, xregDummies, xregDummiesShort, file="adam-model/xreg.Rdata")
# rm(xregDummies, xregDummiesShort)

xregDummiesShort <- model.matrix(~.,xregExpanded)[,-1]
colnames(xregDummiesShort) <- make.names(colnames(xregDummiesShort), unique=TRUE)


startTime <- Sys.time()
oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
esModel <- es(xregDummiesShort[,1], "ANA", h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b", xreg=xregDummiesShort[,-1])

adamModelFirst <- adam(xregExpanded[,-c(6:7)], "MNM", lags=c(1,24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",
                       occurrence=oesModel, regressors="adapt")
Sys.time() - startTime

# colnames(xregExpanded) <- make.names(colnames(xregExpanded), unique=TRUE)
# xregExpandedFourier <- cbind(xregExpanded,xFourier)

# Parameters for the experiment 
h <- 48
rohStep <- 12
testSet <- 365*24
# Ignore the first 23 hours?
obs <- length(time(xregExpanded[[1]]))
errorMeasures <- c("Actuals","Mean",paste0("quantile",c(1:19/20)))
modelsIvan <- c("iETSXSeasonal","ETS(XXX)","RegressionPoisson","LinearRegression")
modelsIvanNumber <- length(modelsIvan)
errorMeasuresValues <- array(0,c(modelsIvanNumber,length(errorMeasures),h),
                             dimnames=list(modelsIvan,errorMeasures,paste0("h",c(1:h))))

#### Preliminary parameters ####
# i <- 1
# oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
# adamModel <- adam(xregExpanded[,-c(6:7)], "MNM", lags=c(1,24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",
#                   occurrence=oesModel, regressors="use",distribution="dgamma", formula=y~x+xLag24+weekOfYear,
#                   maxeval=100000)
# adamParameters <- coef(adamModel)
# rm(oesModel,adamModel)
# almModel <- alm(formula=y~x+xLag24+weekOfYear+hourOfDay+dayOfWeek,xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),],
#                 distribution="dpois", ftol_rel=1e-8, print_level=41, maxeval=200000)
# almParameters <- coef(almModel)
# names(almParameters) <- make.names(names(almParameters),unique=T)
# almFormula <- as.formula(paste0("y~",paste(make.names(names(almParameters)[-1],unique=T),collapse="+")))
# rm(almModel)
# 
# #### The run ####
# registerDoMC(20)
timesIS <- vector("numeric",4)
# -36 gives the necessary 48 obs for the last step
timesISReturned <- foreach(i=1:1) %do% {
  #### First approach - Double-seasonal iETS with events
  j <- 1
  startTime <- Sys.time()
  oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
  adamModel <- adam(xregExpanded[,-c(6:7)], "MNM", lags=c(1,24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",
                    occurrence=oesModel, regressors="use",distribution="dgamma",
                    formula=y~x+xLag24+weekOfYear,B=adamParameters,maxeval=10000)
  testForecast <- forecast(adamModel, interval="semiparametric", h=h, level=c(1:19/20), side="u",
                           newdata=xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,])
  
  timesIS[j] <- Sys.time()-startTime

  # # Mean values
  # errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # # Pinball values
  # errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # 
  # # Actual values
  # errorMeasuresValues[,"Actuals",] <- matrix(adamModel$holdout[[1]][1:h],modelsIvanNumber,h,byrow=TRUE)
  # # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  # gc(verbose=FALSE)
  
  #### Second approach - just ETS
  j <- 2
  startTime <- Sys.time()
  etsModel <- adam(xregExpanded[[1]],"ANA",lags=24, h=testSet-(i-1)*rohStep, holdout=TRUE, initial="o")
  testForecast <- forecast(etsModel, interval="pred", h=h, level=c(1:19/20), side="upper")
  timesIS[j] <- Sys.time()-startTime
  # Mean values
  # errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # # Pinball values
  # errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(etsModel, testForecast)
  # gc(verbose=FALSE)
  
  #### Third approach - Regression with Poisson
  j <- 3
  startTime <- Sys.time()
  # regressionModel <- stepwise(xregDummies[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dpois",
  #                             ftol_rel=1e-8)
  regressionModel <- alm(formula=almFormula, xregDummies[1:(obs-(testSet-(i-1)*rohStep)),],
                         distribution="dpois", ftol_rel=1e-8, B=almParameters, fast=T, maxeval=10000)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregDummies[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  timesIS[j] <- Sys.time()-startTime
  # Mean values
  # errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # # Pinball values
  # errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # # Remove objects to preserve memory
  # rm(regressionModel, testForecast)
  gc(verbose=FALSE)
  
  #### Fourth approach - Stepwise Regression with Poisson
  j <- 4
  startTime <- Sys.time()
  regressionModel <- stepwise(xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dnorm",
                              ftol_rel=1e-10)
  # regressionModel <- alm(y~.,xregDummies[1:(obs-(testSet-(i-1)*rohStep)),-c(2:109)], distribution="dnorm", ar=1,
  #                             ftol_rel=1e-8)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  timesIS[j] <- Sys.time()-startTime
  # Mean values
  # errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # # Pinball values
  # errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # # Set all negative values to zero
  # errorMeasuresValues[j,2+1:19,] <- max(errorMeasuresValues[j,2+1:19,],0)
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  gc(verbose=FALSE)
# Sys.time() - startTime
  
  return(timesIS)
}
IS_results_time <- unlist(timesISReturned)
names(IS_results_time) <- modelsIvan

save(IS_results_time,file="results/IS_results_time.Rdata")
