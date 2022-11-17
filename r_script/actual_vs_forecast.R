#create actual vs. forecast in out of sample
#add also school holidays, new_years day and xmas day, and the rest as is_public_holiday

library(tidyverse)

#------- using tbats-------


ae_original <- ae_original %>% as_tsibble(index = arrival_1h)
f_horizon <- 48
start <- last(ae_original$arrival_1h)-years(1)
n_init <- nrow(filter(ae_original, arrival_1h <= lubridate::ymd_hms("2018-02-28 23:00:00")))
ae_test <- filter(ae_original, arrival_1h > lubridate::ymd_hms("2018-02-28 23:00:00")) %>%
  slide_tsibble(.size = f_horizon, .step = 12)
ivan <- load("results/IvanValues.RData")
fc <- read_rds("results/forecast_prophet.rds")
fc1 <- quantileValuesIvan[[1]]
as_tibble(fc1) %>% tail()
as_tibble(fc) %>% tail()
fcj<- JB_results[[1]]
as_tibble(fcj) %>% tail()
res_plot <- bind_cols(as_tibble(fc),ae_test)

data_plot <- res_plot %>% 
  mutate(hour=lubridate::hour(target),dow=lubridate::wday(target, label = TRUE)) %>% 
  mutate(hour=as_factor(hour),dow=as_factor(dow))
ggplot(data_plot, 
       mapping = aes(x=n_attendance, y =`0.95`, color = hour))+
  geom_jitter(size=0.5)

data_plot <- res_plot %>% 
  mutate(hour=lubridate::hour(target),dow=lubridate::wday(target, label = TRUE)) %>% 
  mutate(hour=as_factor(hour),dow=as_factor(dow))
ggplot(data_plot, 
       mapping = aes(x=n_attendance, y =`0.5`, color = dow))+
  geom_jitter(size=0.5)+
  geom_abline(slope = 1, colour="red", intercept = 0)+
  ggthemes::scale_color_colorblind()+
  expand_limits(y=c(0,50))+
  ggthemes::theme_few()+
  labs(x="actual", y="forecast", color="Day of Week")


ae_original <- ae_original %>% as_tsibble(index = arrival_1h)
f_horizon <- 2

ae <- ae_original %>% filter_index(. ~ "2014-04-01 07:00:00")
ae_tscv <- ae %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = 4, .step = 2 )

ae_test <- ae %>% slice((n()-f_horizon):n())
  slide_tsibble(.size = f_horizon, .step = 2)
