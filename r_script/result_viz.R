## ---- Pinball
my_colorblind <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black")
PB <- read_rds("results/PB.rds")
selected_method <- c("iETSXSeasonal","GBM",
                     "NOtr-2","RegressionPoisson",
                     "tbats","ETS(XXX)","Benchmark-1",
                     "Poisson-1","NBI-2-I")
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
REL <- read_rds("results/REL.rds")
REL_nom <- data.table(Nominal=seq(0,1,by=0.05),
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
            color="red",size=1.1,show.legend = F) +
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
            color="red",size=1.1,show.legend = F) +
  geom_line() + geom_point() +
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
  expand_limits(x=c(1,48))+
  xlab("Lead-time [h]") + 
  ylab("Pinball Loss") + 
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 

## ---- lead-time-rel
rel_pb <- REL[Horizon!="All",
              .(Loss=mean(abs(Nominal-Empirical))),
              by=c("Horizon","Method","Issue")]
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


## ---- lead-time-rmse
RMSE <- read_rds("results/RMSE.rds")
rmse <- RMSE[Horizon!="All" & Method != "faster",
             .(RMSE=mean(RMSE)),
             by=c("Horizon","Method","Issue")]
rmse_selected <- rmse %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(rmse_selected,
       aes(x=as.numeric(Horizon),
           y=RMSE,color=Method)) + 
  facet_wrap(facets = "Issue") +
  geom_line() + xlab("Lead-time [h]") + 
  ylab("Pinball Loss") +
  scale_color_manual(
    values = my_colorblind)+
  theme_few() 



## ---- Skill_rel2bench

plotdata <- read_rds("plotdata.rds")
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


## ---- Skill_rel2bench_reduced
Skill_rel2bench_data <- plotdata[`Skill Score`>-2,]
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







