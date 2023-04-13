devtools::install_github("config-i1/greybox", upgrade="never", dependencies=FALSE)
devtools::install_github("config-i1/smooth", upgrade="never", dependencies=FALSE)
devtools::install_github("config-i1/gnorm", upgrade="never", dependencies=FALSE)

require(data.table)
require(readxl)
library(greybox)
library(smooth)
library(zoo)
library(statmod)
library(gnorm)

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

# Fit the occurrence model
oesModel <- oes(x, "MNN", h=24*7, holdout=TRUE, occurrence="direct")
# Fit the demand sizes model
adamModel <- adam(x, "MNM", lags=c(24,24*7), h=24*7, holdout=TRUE, initial="b", occurrence=oesModel)
adamModel <- adam(x, "MNM", lags=c(24,24*7), h=24*7, holdout=TRUE, initial="b", occurrence=oesModel, orders=list(ar=1))
# Do visual diagnostics
par(mfcol=c(3,4))
plot(adamModel,c(1:11))


# adamModelX <- auto.adam(xregData, "MNM", lags=c(24,24*7), h=24*7, holdout=TRUE, initial="b", occurrence=oesModel, parallel=TRUE)
adamModelX <- adam(xregData, "MNM", lags=c(24,24*7), h=24*7, holdout=TRUE, initial="b", occurrence=oesModel, distribution="dgnorm")
plot(adamModelX,7)
adamModelX
par(mfcol=c(3,4))
plot(adamModelX,c(1:11))
