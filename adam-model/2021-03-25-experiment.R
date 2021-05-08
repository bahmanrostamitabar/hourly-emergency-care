# ADAM experiment
devtools::install_github("config-i1/greybox", upgrade="never", dependencies=FALSE)
devtools::install_github("config-i1/smooth", upgrade="never", dependencies=FALSE)

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
rm(xregDummies, xregDummiesShort)

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
i <- 1
oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
adamModel <- adam(xregExpanded[,-c(6:7)], "MNM", lags=c(1,24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",
                  occurrence=oesModel, regressors="use",distribution="dgamma", formula=y~x+xLag24+weekOfYear,
                  maxeval=100000)
adamParameters <- coef(adamModel)
rm(oesModel,adamModel)
almModel <- alm(formula=y~x+xLag24+weekOfYear+hourOfDay+dayOfWeek,xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),],
                distribution="dpois", ftol_rel=1e-8, print_level=41, maxeval=200000)
almParameters <- coef(almModel)
names(almParameters) <- make.names(names(almParameters),unique=T)
almFormula <- as.formula(paste0("y~",paste(make.names(names(almParameters)[-1],unique=T),collapse="+")))
rm(almModel)

#### The run ####
registerDoMC(20)
# -36 gives the necessary 48 obs for the last step
experimentResultsTestIvan <- foreach(i=1:((testSet-36)/rohStep)) %dopar% {

# startTime <- Sys.time()
  #### First approach - Double-seasonal iETS with events
  j <- 1
  oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=testSet-(i-1)*rohStep, holdout=TRUE, occurrence="direct")
  adamModel <- adam(xregExpanded[,-c(6:7)], "MNM", lags=c(1,24,24*7), h=testSet-(i-1)*rohStep, holdout=TRUE, initial="b",
                    occurrence=oesModel, regressors="use",distribution="dgamma",
                    formula=y~x+xLag24+weekOfYear,B=adamParameters,maxeval=10000)
  testForecast <- forecast(adamModel, interval="semiparametric", h=h, level=c(1:19/20), side="u",
                           newdata=xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,])
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  
  # Actual values
  errorMeasuresValues[,"Actuals",] <- matrix(adamModel$holdout[[1]][1:h],modelsIvanNumber,h,byrow=TRUE)
  # Remove objects to preserve memory
  rm(oesModel, adamModel, testForecast)
  gc(verbose=FALSE)
  
  #### Second approach - just ETS
  j <- 2
  etsModel <- adam(xregExpanded[[1]],"ANA",lags=24, h=testSet-(i-1)*rohStep, holdout=TRUE, initial="o")
  testForecast <- forecast(etsModel, interval="pred", h=h, level=c(1:19/20), side="upper")
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(etsModel, testForecast)
  gc(verbose=FALSE)
  
  #### Third approach - Regression with Poisson
  j <- 3
  # regressionModel <- stepwise(xregDummies[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dpois",
  #                             ftol_rel=1e-8)
  regressionModel <- alm(formula=almFormula, xregDummies[1:(obs-(testSet-(i-1)*rohStep)),],
                         distribution="dpois", ftol_rel=1e-8, B=almParameters, fast=T, maxeval=10000)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregDummies[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  gc(verbose=FALSE)
  
  #### Fourth approach - Stepwise Regression with Poisson
  j <- 4
  regressionModel <- stepwise(xregExpanded[1:(obs-(testSet-(i-1)*rohStep)),], distribution="dnorm",
                              ftol_rel=1e-10)
  # regressionModel <- alm(y~.,xregDummies[1:(obs-(testSet-(i-1)*rohStep)),-c(2:109)], distribution="dnorm", ar=1,
  #                             ftol_rel=1e-8)
  # nsim is needed just in case, if everything fails and bootstrap is used
  testForecast <- predict(regressionModel, xregExpanded[-c(1:(obs-(testSet-(i-1)*rohStep))),][1:h,],
                          interval="prediction", level=c(1:19/20), side="upper", nsim=100)
  # Mean values
  errorMeasuresValues[j,"Mean",] <- testForecast$mean
  # Pinball values
  errorMeasuresValues[j,2+1:19,] <- t(testForecast$upper[,1:19])
  # Set all negative values to zero
  errorMeasuresValues[j,2+1:19,] <- max(errorMeasuresValues[j,2+1:19,],0)
  # Remove objects to preserve memory
  rm(regressionModel, testForecast)
  gc(verbose=FALSE)
# Sys.time() - startTime
  
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
# rm(xreg,xregData,xFourier,xregDataFourier,xregExpanded,xregExpandedFourier)

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

quantileValuesIvan <- replicate(modelsIvanNumber+1,quantileMatrix,simplify=FALSE)
names(quantileValuesIvan) <- c(modelsIvan,"iETSCeiling")

for(j in 1:modelsIvanNumber){
  for(i in 1:((testSet-36)/rohStep)){
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),1] <- time(xregExpanded[[1]])[(obs-(testSet-(i-1)*rohStep))]
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),2] <- time(xregExpanded[[1]])[-c(1:(obs-(testSet-(i-1)*rohStep)))][1:h]
    quantileValuesIvan[[j]][(i-1)*h+c(1:h),3:21] <- t(experimentResultsIvan[i,j,3:21,])
    RMSEValuesIvan[i,j] <- sqrt(MSE(experimentResultsIvan[i,1,"Actuals",],experimentResultsIvan[i,j,"Mean",]))
  }
}
# Quantiles for rounded up values
j <- modelsIvanNumber+1
for(i in 1:((testSet-36)/rohStep)){
  quantileValuesIvan[[j]][(i-1)*h+c(1:h),1] <- time(xregExpanded[[1]])[(obs-(testSet-(i-1)*rohStep))]
  quantileValuesIvan[[j]][(i-1)*h+c(1:h),2] <- time(xregExpanded[[1]])[-c(1:(obs-(testSet-(i-1)*rohStep)))][1:h]
  quantileValuesIvan[[j]][(i-1)*h+c(1:h),3:21] <- t(ceiling(experimentResultsIvan[i,1,3:21,]))
}

save(RMSEValuesIvan,quantileValuesIvan,file="results/IvanValues.RData")
