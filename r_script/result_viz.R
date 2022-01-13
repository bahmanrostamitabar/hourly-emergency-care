## ---- Pinball
my_colorblind <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black")
PB <- read_rds("../results/PB.rds")
REL <- read_rds("../results/REL.rds")
plotdata <- read_rds("../results/plotdata.rds")
RMSE <- read_rds("../results/RMSE.rds")

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
           shape=Method)) +
  geom_line(size=.05) + geom_point(size=2) + 
  ylab("Pinball Loss") + 
  scale_x_continuous(breaks = seq(.05,.95,.05))+
  scale_y_continuous(breaks = seq(0,2,.2))+
  scale_shape_manual(
    values = c(0,1,2,3,4,5,8))+
  theme_few()+
  theme(legend.position = "bottom", 
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))

## ---- Reliability
REL_nom <- data.table::data.table(Nominal=seq(0,1,by=0.05),
                      Empirical=seq(0,1,by=0.05),
                      `Quantile Bias`= 0,
                      Method="Nominal")
REL_selected <- REL %>% as_tibble() %>% 
  filter(Method %in% selected_method)
REL_selected$Method <- factor(REL_selected$Method, levels = unique(REL_selected$Method))

ggplot(data=REL_selected %>% filter(Horizon=="All" & Issue == "All"),
       aes(x=Nominal,
           y=Empirical,
           shape=Method)) +
  geom_line(size=.05) + geom_point(size=2) +
  geom_abline(color="red",size=1,linetype=2)+
  scale_x_continuous(limits=c(0,1),breaks = seq(0,1,.1))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,.1))+
  scale_shape_manual(values = c(0,1,2,3,4,5,8))+
  theme_few()+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

## ---- quantile-bias
REL1 <- REL[,`Quantile Bias`:=Empirical-Nominal]
REL1_selected <- REL1 %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(data=REL1_selected %>% filter(Horizon=="All" & Issue == "All")
       ,aes(x=Nominal,
            y=`Quantile Bias`,
            shape=Method)) +
  geom_line(size=.05) + geom_point(size=2) +
  geom_hline(yintercept = 0, color="red",size=1,linetype=2)+
  xlim(c(0,1)) + ylim(c(-0.2,0.2)) + #ggtitle("Quantile Bias") +
  scale_shape_manual(values = c(0,1,2,3,4,5,8))+
  theme_few()+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

## ---- lead-time-pb

lt_pb <- PB[Horizon!="All" & Method != "faster",
            .(Loss=mean(Loss)),
            by=c("Horizon","Method","Issue")]
lt_pb_selected <- lt_pb %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method) %>% filter(Issue==0) %>% select(-Issue)
ggplot(lt_pb_selected,
       aes(x=as.numeric(Horizon),
           y=Loss,shape=Method)) + 
  geom_line(size=.05) + 
  geom_point(size=2)+
  expand_limits(x=c(1,48))+
  xlab("Lead-time [h]") + 
  ylab("Pinball Loss") + 
  scale_shape_manual(values = c(0,1,2,3,4,5,8))+
  theme_few() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

## ---- lead-time-rel
rel_pb <- REL %>% filter(Horizon!="All") %>%
  group_by(Horizon,Method,Issue) %>% 
  summarise(Loss=mean(abs(Nominal-Empirical))) %>% ungroup()

rel_pb_selected <- rel_pb %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method) %>% filter(Issue==0) %>% select(-Issue)
ggplot(rel_pb_selected,
       aes(x=as.numeric(Horizon),
           y=Loss,
           shape=Method)) + 
  geom_line(size=.05) + 
  geom_point(size=2)+
  geom_point() +
  xlab("Lead-time [h]") + 
  ylab("Mean Absolute Quantile Bias")+
  scale_shape_manual(values = c(0,1,2,3,4,5,8))+
  theme_few() 

## ---- rmse
rmse <- RMSE %>% filter(Horizon=="All") %>%
  group_by(Method) %>% summarise(RMSE=mean(RMSE))
rmse_selected <- rmse %>% as_tibble() %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(rmse_selected,
       aes(x=fct_reorder(Method,RMSE),
           y=RMSE))+
  geom_point(colour="black", size=3) + 
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
  filter(Method %in% selected_method)%>% filter(Issue==0) %>% select(-Issue)

ggplot(rmse_selected_h,
       aes(x=as.numeric(Horizon),
           y=RMSE,shape=Method)) + 
  geom_line(size=.05) + 
  geom_point(size=2)+
  xlab("Lead-time [h]") + 
  ylab("RMSE") +
  scale_shape_manual(values = c(0,1,2,3,4,5,8))+
  theme_few() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

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
  theme_few() +
  theme(
        legend.title = element_text(face = "bold"))



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
  theme_few()+
  theme(
        legend.title = element_text(face = "bold"))

## ---- Pinaball-vs-Qbias
ggplot(data=merge(PB[kfold=="Test" & Horizon=="All",.(Pinball=mean(Loss)),by=c("Method")],
      REL[kfold=="Test" & Horizon=="All",.(`Quantile Bias`=mean(abs(`Quantile Bias`))),by=c("Method")],
      by="Method")[Method%in%selected_method],
      aes(x=Pinball,y=`Quantile Bias`,shape=Method)) +
        geom_point(size=3) + theme_few() + ylim(c(0,0.065)) + 
  xlim(c(1.2,1.5))+
  theme_few()+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

## ---- time
results_table <- read_rds("results_table.rds")
time <- results_table %>% as_tibble() %>% select(Method,Time)
time_selected <- time %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

ggplot(time_selected,
       aes(x=fct_reorder(Method,Time),
           y=Time))+
  geom_point(colour="black", size=3) + 
  geom_segment(color="grey", lty=3, aes(x=Method, 
                                        xend=Method, 
                                        y=0, yend=Time))+
  coord_flip()+
  theme_few()+
  labs(x="Method", y = "Running time in second")

## ---- time-accuracy
results_table <- read_rds("results_table.rds")
time <- results_table %>% as_tibble()
time_selected <- time %>% 
  mutate(Method=factor(Method)) %>% 
  filter(Method %in% selected_method)

results_table_nona <- time_selected %>% filter(across(
  .cols = everything(),
  .fns = ~ !is.na(.)
)) %>% select(Method,Time, everything())

time_long <- results_table_nona %>% 
  pivot_longer(cols = 3:5,names_to = "Measure",values_to = "Accuracy")

ggplot(time_long,aes(x=Time,
                     y=Accuracy))+
  geom_point(aes(shape=Method))+
  facet_wrap(vars(Measure), ncol = 1, scales = "free")+
  theme_few()+
  labs(x="Time (in seconds)")+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

# ggplot(time_long,aes(x=Time,
#                               y=Accuracy))+
#   ggrepel::geom_text_repel(aes(label = Method), size=3)+
#   facet_wrap(vars(Measure), ncol = 1, scales = "free")+
#   theme_few()+
#   labs(x="Time (in seconds)")

