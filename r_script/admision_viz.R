## ---- hourly-plot-ridge -----------
h2_hourly <- readr::read_csv("../data/h2_hourly.csv")
h2_hourly <- h2_hourly %>% filter(arrival_1h >="2015-01-01 00:00:00"&arrival_1h <"2019-01-01 00:00:00")

ride_data <- h2_hourly %>% 
  mutate(hour=hour(arrival_1h), dow=wday(arrival_1h, label = TRUE)) 

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
       x = "\nAdmission")

p

# ggsave(filename="admission.pdf",
#        plot=p, 
#        width = 11,
#        height=14,
#        dpi = 360, 
#        units = "in")