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
library(ProbCast)

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
# The first observations is from 31st March
x <- zoo(h2$n_attendance, order.by=h2$targetTime)
xreg <- zoo(xreg, order.by=h2$targetTime)
xregData <- data.frame(x=x,xreg=xreg)
# xFourier <- fourier(msts(as.vector(x),seasonal.periods=c(24,24*7,24*365.25)), K=c(10,10,10))
# xregDataFourier <- cbind(xregData,xFourier)

#### Expand the data and create the table with dummies 
xregExpanded <- cbind(x,model.matrix(~xreg-1))
# Include days of week and months of year
xregExpanded <- cbind(xregExpanded,
                      temporaldummy(x,type="hour",of="day")[,-1],
                      temporaldummy(x,type="day",of="week")[,-1],
                      temporaldummy(x,type="month",of="year")[,-1])
# colnames(xregExpanded) <- make.names(colnames(xregExpanded), unique=TRUE)
# xregExpandedFourier <- cbind(xregExpanded,xFourier)

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
testSet <- 364*24
# Ignore the first 23 hours?
obs <- length(x) - 23
errorMeasures <- c("Actuals","Mean",paste0("quantile",c(1:19/20)),"Time")
modelsIvan <- c("iETSX","iETSXSeasonal","ETS(XXX)",
                "RegressionPoisson","RegressionPoissonAR(1)")
modelsIvanNumber <- length(modelsIvan)


#### The run ####
registerDoMC(8)
# -36 gives the necessary 48 obs for the last step
experimentResultsTestIvan <- foreach(i=1:((testSet-36)/rohStep)) %dopar% {
  errorMeasuresValues <- array(0,c(modelsIvanNumber,length(errorMeasures),h),
                                dimnames=list(modelsIvan,errorMeasures,paste0("h",c(1:h))))
  
  #### First approach - Non-seasonal iETS with dummies
  j <- 1
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
  adamModel <- adam(xregExpanded, "MNN", lags=1,h=testSet-(i-1)*rohStep, holdout=TRUE, initial="o",
                    occurrence=oesModel)
  testForecast <- forecast(adamModel, interval="prediction", h=h, level=c(1:19/20), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  
  # Actual values
  errorMeasuresValues[,"Actuals",] <- matrix(adamModel$holdout[1:h],modelsIvanNumber,h,byrow=TRUE)
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  #### Second approach - Seasonal iETSX with m=24
  j <- 2
  oesModel <- oes(as.vector(x), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct", xreg=model.matrix(~xreg))
  adamModel <- adam(xregExpanded[,-c(3:25)], "MNM", lags=c(24), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="o",
                    occurrence=oesModel)
  testForecast <- forecast(adamModel, interval="pred", h=h, level=c(1:19/20), side="u")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  
  #### Third approach - ETS
  j <- 3
  etsModel <- adam(as.vector(x),"XXX",lags=24, h=testSet-(i-1)*rohStep, holdout=TRUE, initial="o")
  testForecast <- forecast(etsModel, interval="pred", h=h, level=c(1:19/20), side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(etsModel, testForecast)
  
  #### Fourth approach - Mixture regression with LGnorm
  j <- 4
  regressionModel <- alm(x~.,xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dpois",
                         maxeval=1000, ftol_rel=1e-8)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  #### Fifth approach - Regression with Poisson
  j <- 5
  regressionModel <- alm(x~.,xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dpois", ar=1,
                         maxeval=10000, ftol_rel=1e-10)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  
  return(errorMeasuresValues)
}
save(experimentResultsTestIvan, file="adam-model/experimentResultsTestIvan.Rdata")

# The array with the results
experimentResultsIvan <- array(NA, c((testSet-36)/rohStep,modelsIvanNumber,length(errorMeasures),h),
                               dimnames=list(paste0("ro",1:((testSet-36)/rohStep)),modelsIvan,errorMeasures,paste0("h",c(1:h))))
# Create an array based on the list
for(i in 1:((testSet-36)/rohStep)){
  experimentResultsIvan[i,,,] <- experimentResultsTestIvan[[i]]
}
save(experimentResultsIvan, file="adam-model/experimentResultsIvan.Rdata")
rm(experimentResultsTestIvan)
rm(xreg,xregData,xFourier,xregDataFourier,xregExpanded,xregExpandedFourier)

# RMSE matrix
RMSEValuesIvan <- matrix(NA,((testSet-36)/rohStep),modelsIvanNumber,
                         dimnames=list(dimnames(experimentResultsIvan)[[1]],
                                       dimnames(experimentResultsIvan)[[2]]))

# Prepare the quantiles in Jethro's format
quantileMatrix <- as.data.frame(matrix(NA,nrow=(testSet-36)/rohStep*h,ncol=21,
                                       dimnames=list(paste0(rep(dimnames(experimentResultsIvan)[[1]],each=h),paste0("h",c(1:h))),
                                                     c("issueTime","targetTime_UK",paste0("q",c(1:19)*5)))))
quantileMatrix$issueTime <- as.POSIXct(quantileMatrix$issueTime)
quantileMatrix$targetTime_UK <- as.POSIXct(quantileMatrix$targetTime_UK)

quantileValuesIvan <- replicate(modelsIvanNumber,quantileMatrix,simplify=FALSE)
names(quantileValuesIvan) <- modelsIvan

for(j in 1:modelsIvanNumber){
  for(i in 1:((testSet-36)/rohStep)){
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),1] <- time(x)[(obs-(testSet-(i-1)*rohStep))]
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),2] <- time(x)[-c(1:(obs-(testSet-(i-1)*rohStep)))][1:h]
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),3:21] <- t(experimentResultsIvan[i,j,3:21,])
    RMSEValuesIvan[i,j] <- sqrt(MSE(experimentResultsIvan[i,1,"Actuals",],experimentResultsIvan[i,j,"Mean",]))
  }
}

save(RMSEValuesIvan,quantileValuesIvan,file="results/IvanValues.RData")
