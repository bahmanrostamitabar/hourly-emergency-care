## ---- hourly-plot -----------
h2_hourly <- readr::read_csv("../data/h2_hourly.csv")
h2_hourly <- h2_hourly %>% filter(arrival_1h >="2015-01-01 00:00:00"&arrival_1h <"2019-01-01 00:00:00")
subseriesplot <- h2_hourly %>% as_tibble() %>% mutate(day=factor(yday(arrival_1h)),year=factor(year(arrival_1h)),hour=hour(arrival_1h)) %>% 
  select(-arrival_1h)
time <-  format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 hour"),
                 "%H:%M", tz="GMT")
time <- time[-length(time)]
p1 <- ggplot(data=subseriesplot,mapping = aes(x=hour ,y=n_attendance, colour=day))+
  geom_line(aes(group=interaction(day,year)))+
  geom_point()+
  theme_bw()+
  theme(legend.position = "")+
  labs(y="Number of visits in ED", x="")+
  scale_x_continuous(breaks=0:23, labels = time)+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(size = 10))

p2 <- subseriesplot %>% 
  ggplot(mapping = aes(x=factor(hour) ,y=n_attendance))+
  geom_boxplot(aes(fill=year))+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(legend.position = "bottom", legend.box = "horizontal")+
  labs(fill="Year",y="Number of visits in ED", x="Hour of day")+
  scale_x_discrete(breaks=0:23, labels = time)+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(size = 10))


h <- tibble(h=rep(1:24,1461 ))
ae_h_hour <- bind_cols(h2_hourly,h)
ae_h_hour <- ae_h_hour %>% as_tibble() %>% mutate(date=date(arrival_1h)) %>% select(h,date,n_attendance)
p <- ggplot(ae_h_hour, mapping = aes(x=factor(h), y=n_attendance))+
  geom_boxplot(outlier.colour = "red")+
  stat_summary(fun = quantile, geom="line",fun.args = list(probs = 0.05) , group= 1, aes(color= "#0072B2"), size = .3)+
  stat_summary(fun = quantile, geom="line",fun.args = list(probs = 0.25) , group= 1, aes(color= "#009E73"), size = .3)+
  stat_summary(fun = median, geom="line", group= 1, aes(color= "#CC79A7"), size = .5)+
  stat_summary(fun = quantile, geom="line",fun.args = list(probs = 0.75) , group= 1, aes(color= "#E69F00"), size = .3)+
  stat_summary(fun = quantile, geom="line",fun.args = list(probs = 0.95) , group= 1, aes(color= "#56B4E9"), size = .3)+
  stat_summary(fun = mean, geom="line", group= 1, aes(color= "#F0E442"), size = .5)

time <-  format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 hour"),
                 "%H:%M", tz="GMT")
time <- time[-length(time)]

p_boxplot <- p+ theme_light()+
  labs(x="Hour of day", y="Hospital admission", color="")+ # Set line color to blue
  scale_color_identity(
    breaks = c("#0072B2","#009E73","#CC79A7","#F0E442","#E69F00","#56B4E9"),
    labels = c("5% quantile", "25% quantile","Median", "Mean","75% quantile","95% quantile"),
    guide = "legend")+
  theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=1,byrow=TRUE))+
  scale_x_discrete(breaks=1:24,labels=time)+
  theme(axis.text.x = element_text(angle = 90))

#p1/p2/p_boxplot

p_boxplot

## ---- 24hour ---------
hour.labs <- time
names(hour.labs) <- c(0:23)
h2_hourly %>% as_tibble() %>% mutate(Date=date(arrival_1h),Hour=hour(arrival_1h)) %>%
  select(-arrival_1h) %>% ggplot(mapping=aes(x=n_attendance))+
  geom_histogram(mapping=aes(y=stat(density)), binwidth = 1,fill="white",color = 'black')+
  geom_density(color="blue")+
  scale_y_continuous(labels = scales::percent_format())+
  facet_wrap(~Hour, scales = "free", ncol = 3,labeller = labeller(Hour = hour.labs))+
  theme_bw()+
  labs(x="Number of visits in ED", y="Percentage")+
  theme(axis.text = element_text(size = 7))
