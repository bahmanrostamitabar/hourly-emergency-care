

require(rstudioapi)
require(data.table)
require(mgcv)
require(mgcViz)

setwd(dirname(getActiveDocumentContext()$path))

load("../data/hw_hourly.rds")
