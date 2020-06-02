

require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)
# library(devtools)
# install_github("jbrowell/ProbCast")
require(ProbCast)

setwd(dirname(getActiveDocumentContext()$path))

## Load and Prep Data ####
# load("../data/hw_hourly.rds")
h2 <- fread("../data/h2_hourly.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"


add_calendar_variables(h2,datetimecol = "targetTime_UK")

## Fit GAM and visualise model ####
gam1 <- bam(n_attendance ~ s(clock_hour,k=24,by=dow) + dow + s(doy,k=20,bs="ad"),
              data=h2)

gam.check(gam1)
plot(gam1,pages = 1)

gam1 <- getViz(gam1,nsim = 200)

check1D(gam1,"clock_hour")
check1D(gam1,"doy")
check2D(gam1,"dow","clock_hour")
