library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(tidyverse)
library(tsibble)
library(fable.prophet)
library(fabletools)
library(fasster)

ae_original <- readr::read_csv("data/h2_hourly.csv")
ae_subdaily <- ae_original %>%
  as_tsibble(index = arrival_1h)
# split data


f_horizon <- 24
start <- last(ae_subdaily$arrival_1h)-years(1)
n_init <- nrow(filter(ae_subdaily, arrival_1h <= lubridate::ymd_hms("2018-02-28 23:00:00")))

ae_tscv <- ae_subdaily %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = n_init, .step = 24 )

ae_test <- filter(ae_subdaily, arrival_1h > lubridate::ymd_hms("2018-02-28 23:00:00")) %>% 
  slide_tsibble(.size = f_horizon, .step = 24)

test_data <- ae_test %>% pull(n_attendance) %>% matrix(ncol = 24,byrow = TRUE)
nrow(test_data)
#readr::write_csv(as_tibble(test_data) , "actual.csv")
ae_tscv %>% filter(.id==2) %>% nrow()

#---tbats--------
ts <- ae_tscv %>% filter(.id==1) %>% pull(n_attendance)
msts_atttendance<-msts( ts,seasonal.periods = c(24, 24*7))
tbats_mod <- msts_atttendance %>%
  forecast::tbats(use.box.cox = FALSE, 
        use.trend = TRUE) %>% forecast(h=24)

#fable---------------
s <- Sys.time()
ae_fit <- ae_tscv %>% model(
  prophet=prophet(n_attendance ~ season("day")+season("week")),
  fasster= FASSTER(n_attendance ~ fourier("day",K=7) + fourier("week", K = 3)+ poly(2))
  )
ae_fc <- ae_fit %>% forecast(h=24)

e <- Sys.time()
s-e              
accuracy <- ae_fc %>% accuracy(ae_subdaily)

##as_fable.forecast function
as_fable.forecast <- function(x, ..., point_forecast = list(.mean = mean)){
  if(is.null(x$upper)){
    # Without intervals, the best guess is the point forecast
    dist <- distributional::dist_degenerate(x$mean)
  } else {
    if(!is.null(x$lambda)){
      x$upper <- box_cox(x$upper, x$lambda)
      x$lower <- box_cox(x$lower, x$lambda)
    }
    warn("Assuming intervals are computed from a normal distribution.")
    level <- colnames(x$upper)[1]
    level <- as.numeric(gsub("^[^0-9]+|%", "", level))/100
    mid <- (x$upper[,1] - x$lower[,1])/2
    mu <- x$lower[,1] + mid
    sigma <- mid/(qnorm((1+level)/2))
    dist <- distributional::dist_normal(mu = as.numeric(mu), sigma = as.numeric(sigma))
    if(!is.null(x$lambda)){
      dist <- distributional::dist_transformed(
        dist, 
        transform = rlang::new_function(exprs(x = ), expr(inv_box_cox(x, !!x$lambda)), env = rlang::pkg_env("fabletools")), 
        inverse = rlang::new_function(exprs(x = ), expr(inv_box_cox(x, !!x$lambda)), env = rlang::pkg_env("fabletools"))
      )
    }
  }
  out <- as_tsibble(x$mean)
  dimnames(dist) <- "value"
  out[["value"]] <- dist
  
  point_fc <- compute_point_forecasts(dist, point_forecast)
  out[names(point_fc)] <- point_fc
  
  build_fable(
    out,
    response = "value",
    distribution = "value"
  )
}
