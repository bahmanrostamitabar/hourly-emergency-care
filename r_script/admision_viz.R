## ---- hourly-plot-ridge -----------
h2_hourly <- readr::read_csv("../data/h2_hourly.csv")
h2_hourly1 <- h2_hourly %>% 
  filter(arrival_1h >="2015-01-01 00:00:00"&arrival_1h <"2019-01-01 00:00:00")

ride_data <- h2_hourly1 %>% 
  mutate(hour=lubridate::hour(arrival_1h), 
         dow=lubridate::wday(arrival_1h, label = TRUE))

ride_data_ampm <- ride_data %>% 
  mutate(hour_ampm = case_when(
    hour == 0 ~ "Minnight",
    hour == 1 ~ "1 am",
    hour == 2 ~ "2 am",
    hour == 3 ~ "3 am",
    hour == 4 ~ "4 am",
    hour == 5 ~ "5 am",
    hour == 6 ~ "6 am",
    hour == 7 ~ "7 am",
    hour == 8 ~ "8 am",
    hour == 9 ~ "9 am",
    hour == 10 ~ "10 am",
    hour == 11 ~ "11 am",
    hour == 12 ~ "Midday",
    hour == 13 ~ "1 pm",
    hour == 14 ~ "2 pm",
    hour == 15 ~ "3 pm",
    hour == 16 ~ "4 pm",
    hour == 17 ~ "5 pm",
    hour == 18 ~ "6 pm",
    hour == 19 ~ "7 pm",
    hour == 20 ~ "8 pm",
    hour == 21 ~ "9 pm",
    hour == 22 ~ "10 pm",
    hour == 23 ~ "11 pm"
  )) %>% 
  mutate(hour_ampm=factor(hour_ampm))
  

l <- c("Minnight", "1 am",  "2 am",     "3 am",    "4 am",    "5 am",    "6 am",   
"7 am",     "8 am",     "9 am",     "10 am",     "11 am",     "Midday",    
"1 pm",     "2 pm",     "3 pm",     "4 pm",     "5 pm",     "6 pm",    
"7 pm" ,    "8 pm"  ,   "9 pm" ,    "10 pm" ,    "11 pm")

ride_data_ampm <- ride_data_ampm %>% 
  mutate(hour_ampm= hour_ampm %>% fct_relevel(l),
         hour_ampm=fct_rev(hour_ampm))


#plot
my_colorblind <- c("#D55E00",
                   "#56B4E9",
                   "#E69F00",
                   "#009E73",
                   "#CC79A7",
                    "#F0E442",
                    "#999999"
                   )

p <- ggplot(ride_data_ampm , aes(y=hour_ampm,
                           x=n_attendance, 
                           fill = dow))+
  geom_density_ridges(alpha=.9,
                      colour= "white",
                      bandwidth=.5,
                      scale = 2) +
  scale_x_continuous(breaks = seq(0,50,5), 
                     expand = c(0, 0))+
  theme_ipsum_rc(grid=FALSE)+
  scale_fill_manual(values = my_colorblind)+
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "black", 
                                 size = .5, linetype = "solid"),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.text.y = element_text(siz=12),
        axis.text.x = element_text(angle = 90,siz=12),
        legend.title = element_text(face = "bold", siz=12),
        axis.title.x = element_text(siz=12),
        axis.title.y = element_text(siz=12))+
  guides(fill = guide_legend(nrow = 1))+
  labs(fill=NULL, 
       y = "Hour of Day\n", 
       x = "\n ED arrivals")

p

## ---- dayofweek
daily_density <- ride_data %>% 
  group_by(arrival_1h,dow) %>% summarise(n_attendance=sum(n_attendance), .groups = "drop")
#after_stat(density),
ggplot(ride_data, aes(n_attendance, colour = dow)) +
  geom_freqpoly(binwidth = 1)+
  theme_ipsum_rc(grid=FALSE)+
  labs(colour=NULL, 
       y = "Count\n", 
       x = "\n ED arrivals")

## ---- seasonplot-dofw
ride_data <- h2_hourly %>% 
  mutate(hour=lubridate::hour(arrival_1h), 
         dow=lubridate::wday(arrival_1h, label = TRUE))
ride_data_tsbl <- ride_data %>% 
  mutate(date=as_date(arrival_1h)) %>% 
  group_by(date) %>% 
  summarise(n_attendance=sum(n_attendance)) %>%  as_tsibble()

ride_data_tsbl %>% 
  gg_season(n_attendance, period = "week")+
  geom_point(size=1)+
  theme_bw()+
  labs(
       y = "ED arrivals\n", 
       x = "\n Day of week")

## ---- seasonplot-weekofyear
ride_data_tsbl <- ride_data %>% 
  mutate(date=as_date(arrival_1h),
         year=year(date),
         week=week(date)) %>% 
  group_by(year,date,week) %>% 
  summarise(n_attendance=sum(n_attendance),
            .groups = "drop")

ggplot(ride_data_tsbl,
       aes(x = week, y = n_attendance))+
  geom_boxplot(aes(group=week))+
   scale_x_continuous(limits =  c(0,54),
                breaks = seq(1,53,2))+
  theme_bw()+
  labs(x="Week", y="ED arrivals")

# ride_data_tsbl %>% 
#   gg_season(n_attendance, period = "year")+
#   geom_point(size=1)+
#   theme_bw()+
#   labs(colour=NULL, 
#        y = "ED arrivals\n", 
#        x = "\n Week of year")+
#   scale_x_date(expand = c(0,0),
#                date_breaks = "2 week", 
#                date_minor_breaks = "1 week",
#                date_labels = "%W")+
#   theme(axis.text.x = element_text(angle = 90))

## ---- date-plot
daily_graph <-  h2_hourly %>% 
  mutate(hour=lubridate::hour(arrival_1h), 
         dow=lubridate::wday(arrival_1h, label = TRUE),
         date=as_date(arrival_1h)) %>% 
  group_by(date,dow) %>% 
  summarise(n_attendance=sum(n_attendance), .groups = "drop")
d_label <- daily_graph %>% filter(n_attendance<250|n_attendance>480|year(date)<2016&n_attendance>450)
p_date <- ggplot(data=daily_graph,mapping = aes(x=date,y=n_attendance))+
  ggrepel::geom_label_repel(data =d_label , aes(label=as.character(date)))+
  geom_point(mapping = aes(shape=dow))+
  geom_smooth(se=FALSE)+
  scale_shape_manual(
    values = seq(0,6))+
  labs(shape = NULL, x="Date", y="ED arrivals")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(shape=guide_legend(nrow=1,byrow=TRUE))
p_date
