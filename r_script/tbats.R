library(lubridate)
library(fable)
library(tidyverse)
library(tsibble)
library(fabletools)
library(forecast)


#admission
ae_original <- readr::read_csv("data/h2_hourly.csv")

#holidays 
holidays_ae <- readxl::read_xlsx("data/holiday_rugby_all.xlsx")

holidays_ae %>%
  mutate(xmas=if_else((mday(Date)==25 & month(Date)==12),1,0),
         new_year=if_else((mday(Date)==1 & month(Date)==1),1,0)) %>% 
  select(Date,is_public_holiday,is_school_holiday,xmas,new_year) %>%
  slice(rep(1:n(), each=24)) %>% select(-Date)->holiday_wales

#join holiday ans ae data

bind_cols(ae_original,
          holiday_wales) %>% as_tsibble()->data_for_forecast
data_for_forecast %>% mutate(is_public_holiday=as_factor(is_public_holiday), 
                             is_school_holiday=as_factor(is_school_holiday),
                             xmas=as_factor(xmas),
                             new_year=as_factor(new_year)) %>% force_tz(arrival_1h,tzone = "UTC")->data_for_forecast
train <- data_for_forecast %>% filter(arrival_1h < lubridate::ymd_hms("2019-02-27 00:00:00"))
test <- data_for_forecast %>% filter(arrival_1h >= lubridate::ymd_hms("2019-02-27 00:00:00"))

# model 
x <- train$n_attendance
startTime <- Sys.time()
msts <- msts(x,seasonal.periods = c(24,24*7, 24*365))

tbats_fit <- forecast::tbats(msts, 
                             use.arma.errors=TRUE,
                             use.trend = TRUE,
                             use.damped.trend = FALSE)
tbats_fc <- forecast::forecast(tbats_fit,h=48)
BRT_results_time[[2]] <- Sys.time()-startTime

names(BRT_results_time) <- c("prophet","tbats")

save(BRT_results_time,file="results/BRT_results_time.Rdata")
