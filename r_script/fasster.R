library(lubridate)
library(patchwork)
library(fable)
library(tidyverse)
library(tsibble)
library(fabletools)
library(fasster)

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
fit_fasster <- train %>%
  model(
    fass=FASSTER(sqrt(n_attendance) ~ fourier(period = "day", K = 10) +
                   fourier(period = "week", K = 5) +
                   fourier(period = "year", K = 3)+
                   is_public_holiday+ is_school_holiday+xmas+new_year+
                   trend(2))
  )
ae_fc <- fit_fasster %>% forecast(new_data=test)