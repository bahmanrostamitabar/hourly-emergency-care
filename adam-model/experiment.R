# ADAM experiment
devtools::install_github("config-i1/gnorm", upgrade="never", dependencies=FALSE)
devtools::install_github("config-i1/greybox", upgrade="never", dependencies=FALSE)
devtools::install_github("config-i1/smooth", upgrade="never", dependencies=FALSE)

require(data.table)
require(readxl)
library(greybox)
library(smooth)
library(zoo)
library(forecast)
library(foreach)
library(doMC)

## Load and Prep Data ####
# load("../data/hw_hourly.rds")
h2 <- fread("data/h2_hourly.csv")
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
xreg[is.na(h2$holiday_festive_day)] <- "none"
xreg <- factor(xreg)

# Create zoo objects
x <- zoo(h2$n_attendance, order.by=h2$targetTime)
xreg <- zoo(xreg, order.by=h2$targetTime)
xregData <- data.frame(x=x,xreg=xreg)
xFourier <- fourier(msts(as.vector(x),seasonal.periods=c(24,24*7,24*365.25)), K=c(10,10,10))
xregDataFourier <- cbind(xregData,xFourier)
xregExpanded <- cbind(x,model.matrix(~xreg-1))
# colnames(xregExpanded) <- make.names(colnames(xregExpanded), unique=TRUE)
xregExpandedFourier <- cbind(xregExpanded,xFourier)

# #### Models for the experiment - tryout ####
# # First approach - Double Seasonal iETS
# oesModel1 <- oes(as.vector(x), "MNN", h=testSet, holdout=TRUE, occurrence="direct")
# adamModel1 <- adam(as.vector(x), "MNM", lags=c(24,24*7), h=testSet, holdout=TRUE, initial="b", occurrence=oesModel1)
# forecast(adamModel1, interval="simulated", h=h)
# 
# # Second approach - Fourier iETS
# oesModel2 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct",xreg=xFourier)
# adamModel2 <- adam(as.vector(x), "MNN", h=h, holdout=TRUE, initial="b", occurrence=oesModel2, xreg=xFourier)
# forecast(adamModel2, interval="simulated", h=h)
# 
# # Third approach - Double Seasonal iETSX
# oesModel3 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct", xreg=model.matrix(~xreg))
# adamModel3 <- adam(xregData, "MNM", lags=c(24,24*7), h=h, holdout=TRUE, initial="b", occurrence=oesModel3)
# forecast(adamModel3, interval="simulated", h=h)
# 
# # Fourth approach - Fourier iETSX
# oesModel4 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct",xreg=cbind(xFourier,model.matrix(~xreg)))
# adamModel4 <- adam(cbind(xregData,xFourier), "MNN", h=h, holdout=TRUE, initial="b",occurrence=oesModel4)
# forecast(adamModel4, interval="simulated", h=h)
# 
# # Fifth approach - ETS
# etsModel <- adam(as.vector(x),"XXX",lags=24, h=h, holdout=TRUE, initial="b")
# forecast(etsModel, interval="simulated", h=h)
# 
# # Sixth approach - sink regression
# regressionModel1 <- alm(x~.,xregData,subset=1:(obs-h))
# predict(regressionModel1, xregData[-c(1:(obs-h)),],interval="parametric")
# 
# # Seventh approach - sink regression with Fourier
# regressionModel2 <- alm(x~.,xregDataFourier,subset=1:(obs-h))
# predict(regressionModel2, xregDataFourier[-c(1:(obs-h)),],interval="parametric")
# 
# # Eight approach - stepwise regression
# regressionModel3 <- stepwise(xregExpanded[1:(obs-h),])
# predict(regressionModel3, xregExpanded[-c(1:(obs-h)),],interval="parametric")
# 
# # Nineth approach - stepwise regression with Fourier
# regressionModel4 <- stepwise(xregExpandedFourier,subset=1:(obs-h))
# predict(regressionModel4, xregExpandedFourier[-c(1:(obs-h)),],interval="parametric")


# Parameters for the experiment 
h <- 48
rohStep <- 12
testSet <- 365*24
obs <- length(x) - 23
errorMeasures <- c("Actuals","Mean",paste0("quantile",c(1:19/20)),"Time")
modelsIvan <- c("iETSDoubleSeasonal","iETSFourier","iETSXDoubleSeasonal","iETSXFourier","ETS(XXX)",
                "RegressionSink","RegressionSinkFourier","RegressionStepwise","RegressionStepwiseFourier")
modelsIvanNumber <- length(modelsIvan)

experimentResultsIvan <- array(NA, c((testSet-36)/rohStep,modelsIvanNumber,length(errorMeasures),h),
                               dimnames=list(paste0("ro",1:((testSet-36)/rohStep)),modelsIvan,errorMeasures,paste0("h",c(1:h))))

#### The run ####
registerDoMC(16)
experimentResultsTestIvan <- foreach(i=1:((testSet-36)/rohStep)) %dopar% {
  errorMeasuresValues <- array(0,c(modelsIvanNumber,length(errorMeasures),h),
                                dimnames=list(modelsIvan,errorMeasures,paste0("h",c(1:h))))
  
  #### First approach - Double Seasonal iETS
  j <- 1
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
  adamModel <- adam(as.vector(x), "MNM", lags=c(24,24*7),h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b", occurrence=oesModel)
  testForecast <- forecast(adamModel, interval="simulated", h=h, level=c(1:19/10), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- testForecast$upper[,1:19]
  
  # Actual values
  errorMeasuresValues[,"Actuals",] <- matrix(adamModel$holdout[1:h],modelsIvanNumber,h,byrow=TRUE)
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  
  #### Second approach - Fourier iETS
  j <- 2
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct",xreg=xFourier)
  adamModel <- adam(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b", occurrence=oesModel, xreg=xFourier)
  testForecast <- forecast(adamModel, interval="simulated", h=h, level=c(1:19/10), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- testForecast$upper[,1:19]
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  
  #### Third approach - Double Seasonal iETSX
  j <- 3
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct", xreg=model.matrix(~xreg))
  adamModel <- adam(xregData, "MNM", lags=c(24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b", occurrence=oesModel)
  testForecast <- forecast(adamModel, interval="simulated", h=h, level=c(1:19/10), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- testForecast$upper[,1:19]
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  
  #### Fourth approach - Fourier iETSX
  j <- 4
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct",xreg=cbind(xFourier,model.matrix(~xreg)))
  adamModel <- adam(xregDataFourier, "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",occurrence=oesModel)
  testForecast <- forecast(adamModel, interval="simulated", h=h, level=c(1:19/10), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- testForecast$upper[,1:19]
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  
  #### Fifth approach - ETS
  j <- 5
  etsModel <- adam(as.vector(x),"XXX",lags=24, h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b")
  testForecast <- forecast(etsModel, interval="simulated", h=h, level=c(1:19/10), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- testForecast$upper[,1:19]
  # Remove objects to preserve memory
  rm(etsModel, testForecast)
  
  
  #### Sixth approach - sink regression
  j <- 6
  regressionModel <- alm(x~.,xregData[1:(obs-(testSet-(i-1)*rohStep)),])
  testForecast <- predict(regressionModel, xregData[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="parametric", level=0.5, side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Median
  errorMeasuresValues[j,12,] <- testForecast$upper
  # Pinball values
  for(k in 1:9){
    testForecast <- predict(regressionModel, xregData[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                            interval="parametric", level=k/10)
    errorMeasuresValues[j,k+2,] <- testForecast$lower
    errorMeasuresValues[j,k+12,] <- testForecast$upper
  }
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  
  #### Seventh approach - sink regression with Fourier
  j <- 7
  regressionModel <- alm(x~.,xregDataFourier[1:(obs-(testSet-(i-1)*rohStep)),])
  testForecast <- predict(regressionModel, xregDataFourier[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="parametric", level=0.5, side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Median
  errorMeasuresValues[j,12,] <- testForecast$upper
  # Pinball values
  for(k in 1:9){
    testForecast <- predict(regressionModel, xregDataFourier[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                            interval="parametric", level=k/10)
    errorMeasuresValues[j,k+2,] <- testForecast$lower
    errorMeasuresValues[j,k+12,] <- testForecast$upper
  }
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  
  #### Eight approach - stepwise regression
  j <- 8
  regressionModel <- stepwise(xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),])
  testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="parametric", level=0.5, side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Median
  errorMeasuresValues[j,12,] <- testForecast$upper
  # Pinball values
  for(k in 1:9){
    testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                            interval="parametric", level=k/10)
    errorMeasuresValues[j,k+2,] <- testForecast$lower
    errorMeasuresValues[j,k+12,] <- testForecast$upper
  }
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  
  #### Nineth approach - stepwise regression with Fourier
  j <- 9
  regressionModel <- stepwise(xregExpandedFourier[1:(obs-(testSet-(i-1)*rohStep)),])
  testForecast <- predict(regressionModel, xregExpandedFourier[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="parametric", level=0.5, side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Median
  errorMeasuresValues[j,12,] <- testForecast$upper
  # Pinball values
  for(k in 1:9){
    testForecast <- predict(regressionModel, xregExpandedFourier[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                            interval="parametric", level=k/10)
    errorMeasuresValues[j,k+2,] <- testForecast$lower
    errorMeasuresValues[j,k+12,] <- testForecast$upper
  }
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  return(errorMeasuresValues)
}
save(experimentResultsTestIvan, file="adam-model/experimentResultsTestIvan.Rdata")

# Create an array based on the list
for(i in 1:((testSet-36)/rohStep)){
  experimentResultsIvan[i,,,] <- experimentResultsTestIvan[[i]]
}
save(experimentResultsIvan, file="adam-model/experimentResultsIvan.Rdata")
rm(experimentResultsTestIvan)
rm(x,xreg,xregData,xFourier,xregDataFourier,xregExpanded,xregExpandedFourier)

# RMSE matrix
RMSEValuesADAM <- matrix(NA,(testSet-36)/rohStep*h,modelsIvanNumber,
                         dimnames=list(dimnames(experimentResultsIvan)[[1]],dimnames(experimentResultsIvan)[[2]]))

# Prepare the quantiles in Jethro's format
quantileMatrix <- matrix(NA,nrow=(testSet-36)/rohStep*h,ncol=21,
                         dimnames=list(NULL,c("issueTime","targetTime_UK",paste0("q",c(1:19)*5))))

quantileValuesIvan <- replicate(modelsIvanNumber,quantileMatrix)
names(quantileValuesIvan) <- modelsIvan

for(j in 1:modelsIvanNumber){
  for(i in 1:((testSet-36)/rohStep)){
    quantileValuesIvan[[j]][,1] <- time(x)[(obs-(testSet-(i-1)*rohStep))]
    quantileValuesIvan[[j]][,2] <- time(x)[-c(1:(obs-(testSet-(i-1)*rohStep)))][1:h]
    quantileValuesIvan[[j]][,3:21] <- t(experimentResultsIvan[i,1,3:21,])
    RMSEValuesIvan[i,j] <- sqrt(MSE(experimentResultsIvan[i,1,"Actuals",],experimentResultsIvan[i,1,"Mean",]))
  }
}

save(RMSEValuesIvan,quantileValuesIvan,file="results/IvanValues.RData")