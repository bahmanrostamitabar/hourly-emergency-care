install.packages(c("lubridate","tsutils"))

library(lubridate)

# Get the 2018 data
test <- h2[year(h2$arrival_1h)==2018,]
# Get to March
testMarch <- test[month(test$arrival_1h)==3,]
# Season plot of 24 - 27 of March (25th March, 1am)
tsutils::seasplot(ts(testMarch$n_attendance[1:(24*4)+552],frequency=24))
# Get to October
testOctober <- test[month(test$arrival_1h)==10,]
# Season plot of 26 - 29 of October (28th October)
tsutils::seasplot(ts(testOctober$n_attendance[1:(24*4)+600],frequency=24))
