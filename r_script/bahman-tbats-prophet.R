library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(tidyverse)
library(tsibble)
library(fable.prophet)
library(fabletools)
library(fasster)
library(forecast)
ae_original <- readr::read_csv("data/h2_hourly.csv")
ae_original %>%  
mutate(arrival_1h=lubridate::force_tz(arrival_1h,tz="GB")) -> ae_original_tz
ae_original_tz$n_attendance[is.na(ae_original_tz$arrival_1h)] <- NA 
ae_original_tz1 <- ae_original_tz %>% fill(arrival_1h)#fill n_attendance of the missing value with previous value
# which(is.na(ae_original_tz), arr.ind=TRUE)

 ae_original_tz %>% 
   mutate(arrival_1h=lubridate::force_tz(arrival_1h,tz="UTC")) -> ae_original_tz_test

 ae_original_tz_test1 <- ae_original_tz_test %>% fill(arrival_1h)
 ae_original_tz_test1$arrival_1h[is.na(ae_original_tz_test1$n_attendance)] <-  (ae_original_tz_test1$arrival_1h[is.na(ae_original_tz_test1$n_attendance)])+dhours(1)
 ae_original_tz_test1 %>% filter(row_number()>8685)
ae_data <- ae_original_tz_test1 %>% fill(n_attendance)


#holidays 
holidays_ae <- readxl::read_xlsx("data/holiday_rugby.xlsx")

holidays_ae %>% mutate(Date=lubridate::force_tz(Date,tz="Europe/London")) -> holidays_ae_tz

holidays_ae_tz$holiday_festive_day[is.na(holidays_ae_tz$holiday_festive_day)] <- FALSE

holidays_ae_tz %>% mutate(holiday=if_else(holiday_festive_day=="FALSE", "FALSE", "TRUE")) %>% 
  select(Date,holiday) %>%
  slice(rep(1:n(), each=24)) ->holiday_wales

holidays_ae$holiday_festive_day[is.na(holidays_ae$holiday_festive_day)] <- FALSE
holidays_ae %>% mutate(holiday=if_else(holiday_festive_day=="FALSE", "FALSE", "TRUE")) %>% 
  select(Date,holiday) %>%
  slice(rep(1:n(), each=24)) ->holiday_wales

holiday_wales %>% filter(row_number()>8685)
#join holiuday ans ae data

bind_cols(ae_original,holiday=holiday_wales$holiday) %>% as_tsibble()->data_for_forecast
anyNA(ae_data)

# ae_original %>% filter(arrival_1h>ymd_hms("2015-03-28 21:00:00"),arrival_1h<ymd_hms("2015-03-29 5:00:00"))->orig_utc
# orig_utc %>% mutate(arrival_1h=force_tz(arrival_1h,tz="Europe/London"))
# tbl <- ae_original %>%
#   mutate(arrival_1h = force_tz(ae_original, tz="Europe/London"))
# 
# test1 %>% filter(year(arrival_1h)=="2015") %>% View()
# tibble(force_tz(ae_original, tz="GB"))->test1
# test1 %>% filter(is.na(arrival_1h))
# test1%>% filter(arrival_1h>ymd_hms("2015-03-28 21:00:00"))
# 
# tbl2 <- ae_original %>%
#   mutate(arrival_1h = with_tz(arrival_1h, "Europe/London"))
# lubridate::tz(tbl2$arrival_1h)
# tbl2 %>% filter(year(arrival_1h)=="2015") %>% View()
# bind_cols(tbl,tbl2) %>% filter(year(arrival_1h...1)=="2015") %>% View()
# 
# identical(tbl,tbl2)
# names(bind_cols(tbl,tbl2))
# tbl1 <- ae_original %>%
#   mutate(arrival_1h = force_tz(arrival_1h, "GB"))
# lubridate::tz(tbl1$arrival_1h)
# 
# 
# 
# # ae_original <- ae_original %>% mutate(arrival_1h=with_tz(arrival_1h,tz="GB"))
# ae_subdaily <- ae_original %>%
#   as_tsibble(index = arrival_1h)
# ae_subdaily %>% filter_index("2015") %>% View()
# 
# aaa_lon <- ae_subdaily %>% as_tibble()%>% transmute(London=with_tz(arrival_1h,tz="GB"),n_attendance=n_attendance)
# all <- (bind_cols(ae_subdaily,aaa_lon))
# all %>% filter(year(arrival_1h)==2015) %>% View()
# 
# ae_original <- ae_original %>% mutate(arrival_1h=with_tz(arrival_1h,tz="GB"))
# ae_subdaily <- ae_original %>%
#   as_tsibble(index = arrival_1h)
# ae_subdaily %>% filter_index("2015") %>% View()
# aaa_lon <- ae_subdaily %>% as_tibble()%>% transmute(arrival_1h=with_tz(arrival_1h,tz="UTC"),n_attendance=n_attendance)
# all1 <- (bind_cols(ae_subdaily,aaa_lon))
# diffdf::diffdf(all,all1)
# all %>% filter(year(arrival_1h)==2015) %>% View()
# 
# 
# aaa_lon_ts <- aaa_lon %>% as_tsibble(index = London)
# View(aaa_lon_ts)
# all <- all %>% mutate(ds=identical(arrival_1h, London))
# 
# aaa1 <- all %>% filter_index("2015")
# View(aaa1)
# aaa1 <- ae_subdaily %>% filter_index("2018-10-25 23:00:00" ~"2018-10-28 23:00:00")
# aaa_GB %>% filter(year(time_gb)==2015) %>% View()
# aaa_GB <- ae_subdaily %>% as_tibble()%>% transmute(time_gb=ymd_hms(arrival_1h,tz="GB"),n_attendance_gb=n_attendance)
# aaa_UTC <- ae_subdaily %>% as_tibble()%>% transmute(time_utc=arrival_1h,n_attendance_utc=n_attendance)
# 
# all <- bind_cols(aaa_GB,aaa_UTC)
# all %>% filter(year(time_gb)=="2015") %>% View()
# 
# aaa1 <- full_join(aaa_GB,aaa_UTC, by="time") %>%  filter(time> ymd_hms("2015-10-25 00:00:00") & time< ymd_hms("2015-10-25 03:00:00"))
# aaa1 %>% pull(arrival_1h) %>% unique()


# split data

ae_original_tz1
holiday_rug_dummy <- holidays_ae %>%
  transmute(
    date = date(Date), 
    holiday = if_else(!is.na(holiday_festive_day), 1,0),
    school_holiday = if_else(!is.na(school_holiday), 1,0),
    rugby=is_rug_in_Cardiff+is_rug_out_Cardiff
  )


f_horizon <- 48
start <- last(data_for_forecast$arrival_1h)-years(1)
n_init <- nrow(filter(data_for_forecast, arrival_1h <= lubridate::ymd_hms("2018-02-28 23:00:00")))
data_for_forecast %>% mutate_if(is.character, as.factor)->data_for_forecast
ae_tscv <- data_for_forecast %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = n_init, .step = 12 )
ae_tscv %>% pull(.id) %>% unique() %>% length()
ae_test <- filter(data_for_forecast, arrival_1h > lubridate::ymd_hms("2018-02-28 23:00:00")) %>% 
  slide_tsibble(.size = f_horizon, .step = 12)
ae_tscv_nonzero <- ae_tscv %>% mutate(n_attendance=n_attendance+1)
nrow(ae_test)
#readr::write_csv(as_tibble(test_data) , "actual.csv")
ae_tscv %>% filter(.id==2) %>% nrow()

#---tbats--------
# ts <- ae_tscv %>% filter(.id==1) %>% pull(n_attendance)
# msts_atttendance<-msts( ts,seasonal.periods = c(24, 24*7))
# tbats_mod <- msts_atttendance %>%
#   forecast::tbats(use.box.cox = FALSE, 
#         use.trend = TRUE) %>% forecast(h=24)
#https://github.com/robjhyndman/quantile_ensembles
#https://robjhyndman.com/hyndsight/electrictsibbles/
#https://blog.rsquaredacademy.com/handling-date-and-time-in-r/#timezones
#https://blog.methodsconsultants.com/posts/timezone-troubles-in-r/
#https://garthtarr.github.io/meatR/datetime.html#time_zones
#fable---------------
scaled_logit <- new_transformation(
  transformation = function(x, lower=0, upper=1){
    log((x-lower)/(upper-x))
  },
  inverse = function(x, lower=0, upper=1){
    (upper-lower)*exp(x)/(1+exp(x)) + lower
  }
)

#Which K?

ae_fit_fass_best <- data_for_forecast %>% model(
  fass_1_1=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=1)+holiday+trend(1)),
  fass_1_2=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=2)+holiday+trend(1)),
  fass_1_3=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=3)+holiday+trend(1)),
  fass_1_4=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=4)+holiday+trend(1)),
  fass_1_5=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=5)+holiday+trend(1)),
  fass_1_6=FASSTER(n_attendance ~ fourier("day", K=1)+fourier("week",K=6)+holiday+trend(1)),
  fass_2_1=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=1)+holiday+trend(1)),
  fass_2_2=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=2)+holiday+trend(1)),
  fass_2_3=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=3)+holiday+trend(1)),
  fass_2_4=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=4)+holiday+trend(1)),
  fass_2_5=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=5)+holiday+trend(1)),
  fass_2_6=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=6)+holiday+trend(1)),
  fass_3_1=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=1)+holiday+trend(1)),
  fass_3_2=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=2)+holiday+trend(1)),
  fass_3_3=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=3)+holiday+trend(1)),
  fass_3_4=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=4)+holiday+trend(1)),
  fass_3_5=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=5)+holiday+trend(1)),
  fass_3_6=FASSTER(n_attendance ~ fourier("day", K=3)+fourier("week",K=6)+holiday+trend(1)),
  fass_4_1=FASSTER(n_attendance ~ fourier("day", K=4)+fourier("week",K=1)+holiday+trend(1)),
  fass_4_2=FASSTER(n_attendance ~ fourier("day", K=4)+fourier("week",K=2)+holiday+trend(1)),
  fass_4_3=FASSTER(n_attendance ~ fourier("day", K=4)+fourier("week",K=3)+holiday+trend(1)),
  fass_4_4=FASSTER(n_attendance ~ fourier("day", K=4)+fourier("week",K=4)+holiday+trend(1)),
  fass_4_5=FASSTER(n_attendance ~ fourier("day", K=4)+fourier("week",K=5)+holiday+trend(1))
  )

ae_fit_fass_best %>% glance()

s <- Sys.time()
ae_fit <- ae_tscv %>% model(
  prophet=prophet(n_attendance ~ season("day")+season("week")+holiday+ growth("linear")),
  prophet_log=prophet(log(n_attendance) ~ season("day")+season("week")+holiday+ growth("linear")))

#fasster= fasster::FASSTER(n_attendance ~ fourier("day", 10)+fourier("week",6)+holiday+ trend())
ae_fc <- ae_fit %>% forecast(new_data=ae_test)
accuracy <- ae_fc %>% accuracy(data_for_forecast)
e <- Sys.time()
s-e
s <- Sys.time()
ae_fit_fass <- ae_tscv %>% model(
  fass=FASSTER(n_attendance ~ fourier("day", K=2)+fourier("week",K=3)+holiday+trend(1)))
e <- Sys.time()
s-e
ae_fc_fasster <- ae_fit_fass %>% forecast(new_data=ae_test)
s <- Sys.time()
ae_fit_fass_10_5 <- ae_tscv %>% model(
  fass=FASSTER(n_attendance ~ fourier("day", K=8)+fourier("week",K=4)+holiday+trend(1)))
e <- Sys.time()
s-e
accuracy <- ae_fc %>% accuracy(data_for_forecast)


# extract qualtiles according to Jethro's format
ae_fc_fasster %>% 
  as_tibble() %>%
  group_by(.id,arrival_1h) %>%
  summarise(
    qs = quantile(n_attendance, seq(.05,.95,.05)), prob = seq(.05,.95,.05)
  ) %>% ungroup()->q_prophet

q_prophet %>% pivot_wider(names_from = prob, values_from=qs)->q_wider
q_wider %>% View()

q_wider[q_wider<0] <- 0

ae_tscv %>% group_by(.id) %>% slice(n()) %>% as_tibble() %>% 
  select(arrival_1h) %>% 
 slice(rep(1:n(), each = 48)) %>% mutate(issueTime=arrival_1h) %>% select(-arrival_1h) %>% ungroup()-> origin_date
q_wider_target <- q_wider %>% rename(targetTime_UK=arrival_1h) %>% select(-.id)
bind_cols(q_wider_target,origin_date) %>% select(issueTime,targetTime_UK, everything())-> forecast_fasster

forecast_fasster <- forecast_fasster %>% rename_at(vars(names(forecast_fasster)[3:21]),~paste0("q",seq(5,95,5)))
(names(forecast_fasster))[3:21] <-  paste0("q",seq(5,95,5))

names(forecast_fasster)

write_rds(forecast_fasster,"results/forecast_fasster.rds")


## using tbats

#TBATS
ae_original <- ae_original %>% as_tsibble(index = arrival_1h)
f_horizon <- 48
start <- last(ae_original$arrival_1h)-years(1)
n_init <- nrow(filter(ae_original, arrival_1h <= lubridate::ymd_hms("2018-02-28 23:00:00")))
ae_tscv <- ae_original %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = n_init, .step = 12 )
tseries <- list()
for (i in 1:length(unique(ae_tscv$.id))) {
  tseries[[i]] <- ae_tscv %>% as_tibble() %>% filter(.id==i) %>% select(n_attendance) %>% unlist() %>% as.vector()
}
mt_list <- split(ae_tscv, f = ae_tscv$.id)
s1 <- 24
s2 <- 24*7
fh <- 48

fable_tbats <- function(x) {
  msts <- msts(x,seasonal.periods = c(24,24*7))
  tbats_fit <- forecast::tbats(msts)
  tbats_fc <- forecast::forecast(tbats_fit,h=48)
  as_fable(tbats_fc)
}

tseriestest <- tseries[c(1,2,3)]
res <- map_dfr(tseriestest,fable_tbats)

tbst_fcst <- bind_cols(select(test_tscv, date,.id), res) %>% select(-index) %>% as_tsibble(index = date)
as_fable(tbst_fcst, response = "value", distribution = value)->tbats_fable
yy <- tseries[c(1,2,3)]
fTBATS<- function(yy){
  library(forecast)
  yy.msts <- msts(yy,seasonal.periods = c(24,24*7))
  model<-tbats(yy.msts, use.box.cox=FALSE)
  replicate(5000,simulate(model, future=TRUE, nsim=48))
}


TBATS_FCT <- function(x) {
  msts <- msts(x,seasonal.periods = c(24,24*7))
  tbats_fit <- forecast::tbats(msts, use.box.cox=FALSE, use.arma.errors=FALSE, use.damped.trend = FALSE)
  tbats_fc <- forecast::forecast(tbats_fit,h=48)
  qf <- matrix(0, nrow=19, ncol=48)
  m <- tbats_fc$mean
  s <- (tbats_fc$upper-tbats_fc$lower)/1.96/2
  for(h in 1:48)
    qf[,h] <- qnorm(seq(0.05,.95,.05), m[h], s[h])
  qf
}

tbast_fct <- list()
for (i in (1:length(tseries))) {
  tbast_fct[[i]] <- TBATS_FCT(tseries[[i]])
}

tbast_fctt <- list()
for (i in (1:length(tbast_fct))) {
  tbast_fctt[[i]] <- t(tbast_fct[[i]])
}

tbast_tbl <- tibb

for (i in (1:length(tbast_fct))) {
  tbast_fctt[[i]] <- t(tbast_fct[[i]])
}

res = NULL
for(i in (1:length(tbast_fctt))){
  fct <- as_tibble(tbast_fctt[[i]])
  res = bind_rows(res,fct)
}
colnames(res) <- paste0("q",seq(5,95,5))
res[res<0] <- 0

dates <- read_rds("results/forecast_prophet.rds") %>% select(origin,target)


write_rds(bind_cols(dates,res),"results/tbats.rds")
