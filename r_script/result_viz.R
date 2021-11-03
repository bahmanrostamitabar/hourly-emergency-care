## ---- Pinball
my_colorblind <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black")
PB <- read_rds("../results/PB.rds")
REL <- read_rds("../results/REL.rds")
plotdata <- read_rds("../results/plotdata.rds")

selected_method <- c("ADAM-iETSX", # ETS Example
                     "GBM-2",      # ML/non-parametric method
                     "NOtr-2",     # GAMLSS - good
                     "tbats","Benchmark-2", # Benchmarks
                     "NBI-2", "prophet") # GAMLSS - not good
PB_selected <- PB %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)
ggplot(data=PB_selected %>% filter(Horizon=="All" & Issue == "All"),
       aes(x=Quantile,
           y=Loss,
           color=Method)) +
  geom_line() + geom_point(size=1.5) + 
  ylab("Pinball Loss") + 
  scale_x_continuous(breaks = seq(.05,.95,.05))+
  scale_y_continuous(breaks = seq(0,2,.2))+
  scale_color_manual(
    values = my_colorblind)+
  theme_few()

## ---- Reliability
REL_nom <- data.table::data.table(Nominal=seq(0,1,by=0.05),
                      Empirical=seq(0,1,by=0.05),
                      `Quantile Bias`= 0,
                      Method="Nominal")
REL_selected <- REL %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)
ggplot(data=REL_selected %>% filter(Horizon=="All" & Issue == "All"),
       aes(x=Nominal,
           y=Empirical,
           color=Method)) +
  geom_line(data=REL_nom,
            aes(x=Nominal,y=Empirical), 
            color="red",size=1,show.legend = F,linetype=2) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(0,1,.1))+
  scale_y_continuous(breaks = seq(0,1,.1))+
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 

## ---- quantile-bias
REL1 <- REL[,`Quantile Bias`:=Empirical-Nominal]
REL1_selected <- REL1 %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(data=REL1_selected %>% filter(Horizon=="All" & Issue == "All")
       ,aes(x=Nominal,
            y=`Quantile Bias`,
            color=Method)) +
  geom_line(data=REL_nom,aes(x=Nominal,
                             y=`Quantile Bias`), 
            color="red",size=1,linetype=2,show.legend = F) +
  geom_point() + geom_line() +
  xlim(c(0,1)) + ylim(c(-0.2,0.2)) + #ggtitle("Quantile Bias") +
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 

## ---- lead-time-pb

lt_pb <- PB[Horizon!="All" & Method != "faster",
            .(Loss=mean(Loss)),
            by=c("Horizon","Method","Issue")]
lt_pb_selected <- lt_pb %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)
ggplot(lt_pb_selected,
       aes(x=as.numeric(Horizon),
           y=Loss,color=Method)) + 
  facet_wrap(facets = "Issue") +
  geom_line() + 
  geom_point()+
  expand_limits(x=c(1,48))+
  xlab("Lead-time [h]") + 
  ylab("Pinball Loss") + 
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 

## ---- lead-time-rel
rel_pb <- REL %>% filter(Horizon!="All") %>%
  group_by(Horizon,Method,Issue) %>% 
  summarise(Loss=mean(abs(Nominal-Empirical))) %>% ungroup()

rel_pb_selected <- rel_pb %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)
ggplot(rel_pb_selected,
       aes(x=as.numeric(Horizon),
           y=Loss,
           color=Method)) + 
  facet_wrap(facets = "Issue") +
  geom_line() + 
  geom_point() +
  xlab("Lead-time [h]") + 
  ylab("Mean Absolute Quantile Bias")+
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 

## ---- rmse
RMSE <- read_rds("../results/RMSE.rds")
rmse <- RMSE %>% filter(Horizon=="All") %>%
  group_by(Method) %>% summarise(RMSE=mean(RMSE))
rmse_selected <- rmse %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(rmse_selected,
       aes(x=fct_reorder(Method,RMSE),
           y=RMSE))+
  geom_point(colour="#fcab27", size=3) + 
geom_segment(color="grey", lty=3, aes(x=Method, 
                                      xend=Method, 
                                      y=0, yend=RMSE))+
  scale_y_continuous(breaks = seq(0,2,.2))+
  coord_flip()+
  theme_few()+
  labs(x="RMSE", y = "Method")

## ---- lead-time-rmse
rmse_h <- RMSE[Horizon!="All" & Method != "faster",
             .(RMSE=mean(RMSE)),
             by=c("Horizon","Method","Issue")]
rmse_selected_h <- rmse_h %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(rmse_selected_h,
       aes(x=as.numeric(Horizon),
           y=RMSE,color=Method)) + 
  facet_wrap(facets = "Issue") +
  geom_line() + xlab("Lead-time [h]") + 
  ylab("Pinball Loss") +
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 


## ---- Skill-rel2bench
plotdata_plot <- plotdata %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(plotdata_plot, 
       aes(x=reorder(Method, -`Skill Score`), y=`Skill Score`, fill=Qbias)) + 
  ylab("Pinball Skill Score [%]") +
  geom_boxplot() + theme_few() +
  # ggtitle("Pinball Skill Score Relative to Benchmark 2") +
  theme(axis.text.x = element_text(angle = -80)) + #scale_fill_manual(values=cbPalette) +
  # scale_x_discrete(labels= paste0(substring(NAMES, 1,4),".")) +
  geom_hline(yintercept=0, linetype="dashed",size=0.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = "RdYlGn")),
                       limits=c(0,0.11))+
  labs(fill = "Quantile Bias",x="Method") +
  theme_few() 


## ---- Skill-rel2bench-reduced
Skill_rel2bench_data <- plotdata %>% filter(`Skill Score`>-2)
ggplot(Skill_rel2bench_data, aes(x=reorder(Method, -`Skill Score`), y=`Skill Score`, fill=Qbias)) + 
  ylab("Pinball Skill Score [%]") +
  geom_boxplot() + theme_few() +
  # ggtitle("Pinball Skill Score Relative to Benchmark 2") +
  theme(axis.text.x = element_text(angle = -80)) + #scale_fill_manual(values=cbPalette) +
  # scale_x_discrete(labels= paste0(substring(NAMES, 1,4),".")) +
  geom_hline(yintercept=0, linetype="dashed",size=0.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = "RdYlGn")),
                       limits=c(0,0.11))+
  labs(fill = "Quantile Bias",x="Method") +
  theme_few() 

## ---- Pinaball-vs-Qbias
ggplot(data=merge(PB[kfold=="Test" & Horizon=="All",.(Pinball=mean(Loss)),by=c("Method")],
      REL[kfold=="Test" & Horizon=="All",.(`Quantile Bias`=mean(abs(`Quantile Bias`))),by=c("Method")],
      by="Method")[Method%in%selected_method],
      aes(x=Pinball,y=`Quantile Bias`,color=Method,shape=Method)) +
        geom_point(size=3) + theme_few() + ylim(c(0,0.09)) + xlim(c(1.2,1.6))


## ---- time
results_table <- read_rds("results_table.rds")
time <- results_table %>% as_tibble() %>% select(Method,Time)
time_selected <- time %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(time_selected,
       aes(x=fct_reorder(Method,Time),
           y=Time))+
  geom_point(colour="#fcab27", size=3) + 
  geom_segment(color="grey", lty=3, aes(x=Method, 
                                        xend=Method, 
                                        y=0, yend=Time))+
  coord_flip()+
  theme_few()+
  labs(x="Method", y = "Running time in second")


## ---- time-accuracy
results_table <- read_rds("results_table.rds")
time <- results_table %>% as_tibble() %>% select(Method,Time)
time_selected <- time %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

results_table_nona <- results_table %>% filter(across(
  .cols = everything(),
  .fns = ~ !is.na(.)
))

ggplot(results_table_nona)+
aes(x=Time,
    y=Pinball)+
  geom_jitter()+
  geom_label(aes(label = Method))+
  theme_few()
  