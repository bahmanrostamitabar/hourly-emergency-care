library(lubridate)
library(tidyverse)
x <- "2018-01-01 12:00:00"
as.POSIXct(x)
my_t <- seq(ymd_hms("2015-03-28 21:00:00"),ymd_hms("2015-10-27 21:00:00"),by="hour")
tz(my_t)

my_t1 <- with_tz(my_t,tz="Europe/London")
tz(my_t1)

tibble(date=my_t, att=rep(20,length(my_t)))->exam
tibble(date=with_tz(my_t,tz="Europe/London"))->test1
tibble(date=force_tz(my_t,tz="Europe/London"))->test2
tibble(date=with_tz(my_t,tz="Europe/London"), n=rnorm(200,5,n=length(my_t)))->test3#this is my AE data with with_tz

union(test2,test1) ->com_date

left_join(com_date,test3) -> data_for_forecast

data_for_forecast %>% fill(n) %>% drop_na()->aaa#forecast after removing the day with 23 hours
aaa <- aaa %>% mutate(n_f=n)
left_join(test3,aaa)

test1%>% filter(date>ymd_hms("2015-10-24 21:00:00"))

left_join(test3,aaa)%>% filter(date>ymd_hms("2015-10-24 21:00:00"))
my_t2 <- force_tz(my_t,tz="Europe/London")
tz(my_t2)

bind_cols(my_t,my_t1,my_t2) %>% View()

my_t_m <- seq(ymd_hms("2015-03-2 21:00:00"),ymd_hms("2015-03-29 08:00:00"),by="hour")
my_t_m <- seq(ymd_hms("2015-10-24 21:00:00"),ymd_hms("2015-10-25 05:00:00"),by="hour")
tz(my_t_m)

my_t_m1 <- with_tz(my_t_m,tz="Europe/London")
tz(my_t_m1)

my_t_m2 <- force_tz(my_t_m,tz="Europe/London")
tz(my_t_m2)

bind_cols(my_t_m,my_t_m1,my_t_m2) %>% View()
#importa the data and convert the tz using with_tz()
# use forece_tz(), this will create a NA for the clock change in March and remove the aditional hour in the clock change in October
#replace NA with teh date and take the average of the adjacet periods to replace the number of attendance
#tune the model and forecast
#now we need to go through the reverse process to reome the date in March adn add an exyra hour in October to corresp
#onf to the original data to calculate the accuracy

#he suggested procedure -- toss out the extra hour, fill in the missing 
#hour by averaging nearby values -- strikes me as quite reasonable. 
#The affected hours are in the middle of the night when the system is drawing its base load. 
#There is much less volatility of base loads than of peak loads. 
#Therefore, the forecasting results are unlikely to be sensitive to the details: 
#any reasonable way of accounting for the change-over should yield about the same result in terms of 
#calibrating a forecasting model.

#https://stats.stackexchange.com/questions/45046/daylight-saving-time-in-time-series-modelling-e-g-load-data
