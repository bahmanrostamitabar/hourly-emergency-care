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

# Parameters for the experiment 
h <- 48
rohStep <- 12
testSet <- 365*24
obs <- length(x)


#### Models for the experiment ####
# First approach - Double Seasonal iETS
oesModel1 <- oes(as.vector(x), "MNN", h=testSet, holdout=TRUE, occurrence="direct")
adamModel1 <- adam(as.vector(x), "MNM", lags=c(24,24*7), h=testSet, holdout=TRUE, initial="b", occurrence=oesModel)
forecast(adamModel1, interval="simulated", h=h)

# Second approach - Fourier iETS
oesModel2 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct",xreg=xFourier)
adamModel2 <- adam(as.vector(x), "MNN", h=h, holdout=TRUE, initial="b", occurrence=oesModel2, xreg=xFourier)
forecast(adamModel2, interval="simulated", h=h)

# Third approach - Double Seasonal iETSX
oesModel3 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct", xreg=model.matrix(~xreg))
adamModel3 <- adam(xregData, "MNM", lags=c(24,24*7), h=h, holdout=TRUE, initial="b", occurrence=oesModel3)
forecast(adamModel3, interval="simulated", h=h)

# Fourth approach - Fourier iETSX
oesModel4 <- oes(as.vector(x), "MNN", h=h, holdout=TRUE, occurrence="direct",xreg=cbind(xFourier,model.matrix(~xreg)))
adamModel4 <- adam(cbind(xregData,xFourier), "MNN", h=h, holdout=TRUE, initial="b",occurrence=oesModel4)
forecast(adamModel4, interval="simulated", h=h)

# Fifth approach - ETS
etsModel <- adam(as.vector(x),"ZZZ",lags=24, h=h, holdout=TRUE, initial="b")
forecast(etsModel, interval="simulated", h=h)

# Sixth approach - sink regression
regressionModel1 <- alm(x~.,xregData,subset=1:(obs-h))
predict(regressionModel1, xregData[-c(1:(obs-h)),],interval="parametric")

# Seventh approach - sink regression with Fourier
regressionModel2 <- alm(x~.,xregDataFourier,subset=1:(obs-h))
predict(regressionModel2, xregDataFourier[-c(1:(obs-h)),],interval="parametric")

# Eight approach - stepwise regression
regressionModel3 <- stepwise(xregExpanded[1:(obs-h),])
predict(regressionModel3, xregExpanded[-c(1:(obs-h)),],interval="parametric")

# Nineth approach - stepwise regression with Fourier
regressionModel4 <- stepwise(xregExpandedFourier,subset=1:(obs-h))
predict(regressionModel4, xregExpandedFourier[-c(1:(obs-h)),],interval="parametric")

