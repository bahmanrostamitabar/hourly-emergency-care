rm(list=ls())
## Forecast every 12h (00 and 12 local time) <<< Circulate template!!!
## Lead times from 0 to 48h
## Quantiles from 0.05 to 0.95, Pinball loss
## Test Data: 1/3/2018 to 28/2/2019

## To do:
# add checks that exactly same data are being evaluated
# evaluate training data as well as test
# test ceiling/floor options for non-integer forecasts ! << Easy now...
# report RMSE


## Link to use case:
# "Process 95% of patients in 4h"

require(rstudioapi)
require(data.table)
require(ggplot2)
# library(devtools); Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("jbrowell/ProbCast")
require(ProbCast)
setwd(dirname(getActiveDocumentContext()$path))



## Load A&E Arrival Data ####
h2 <- fread("../data/h2_hourly_gb.csv")
h2[,targetTime:=as.POSIXct(arrival_1h,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")]
h2[,targetTime_UK:=targetTime]; attributes(h2$targetTime_UK)$tzone <- "Europe/London"


## Initialis Containers ####
test_start <- "2018-03-01"
last_issue <- "2019-02-26 00:00:00" #### <<<<< Impliment!!! Make test for data matching
PB <- data.table()
REL <- data.table()


## Eval function
big_eval_function <- function(forecast_DT,h2_actuals,method_name){
  
  forecast_DT <- forecast_DT[targetTime_UK>=test_start & issueTime<=last_issue,]
  
  ## Get Actuals
  setkey(forecast_DT,issueTime,targetTime_UK)
  actuals <- merge(forecast_DT[,.(issueTime,targetTime_UK)],h2_actuals[,.(targetTime_UK,n_attendance)],
                   by="targetTime_UK",all.x=T,no.dups = F)
  setkey(actuals,issueTime,targetTime_UK)
  
  ## All
  temp <- data.table(pinball(forecast_DT[,-c(1:2)],actuals[,n_attendance],plot.it = F))
  temp[,Method:=method_name]; temp[,kfold:="Test"]; temp[,Horizon:="All"]; temp[,Issue:="All"]
  PB <<- rbind(PB,temp); rm(temp)
  
  temp <- data.table(reliability(forecast_DT[,-c(1:2)],actuals[,n_attendance],plot.it = F))
  temp[,Method:=method_name]; temp[,kfold:="Test"]; temp[,Horizon:="All"]; temp[,Issue:="All"]
  REL <<- rbind(REL,temp); rm(temp)
  
  ## By horizon and issue time
  for(issue in c(0,12)){
    for(lt in 0:48){
      temp <- data.table(pinball(forecast_DT[
        hour(issueTime)==issue & (targetTime_UK-issueTime)/3600 == lt,-c(1:2)],
        actuals[hour(issueTime)==issue & (targetTime_UK-issueTime)/3600 == lt,n_attendance],plot.it = F))
      temp[,Method:=method_name]; temp[,kfold:="Test"]; temp[,Horizon:=lt]; temp[,Issue:=issue]
      PB <<- rbind(PB,temp); rm(temp)
      
      temp <- data.table(reliability(forecast_DT[
        hour(issueTime)==issue & (targetTime_UK-issueTime)/3600 == lt,-c(1:2)],
        actuals[hour(issueTime)==issue & (targetTime_UK-issueTime)/3600 == lt,n_attendance],plot.it = F))
      temp[,Method:=method_name]; temp[,kfold:="Test"]; temp[,Horizon:=lt]; temp[,Issue:=issue]
      REL <<- rbind(REL,temp); rm(temp)
      
    }
  }  
  
  ## Pinball timeseries
  pb_temp <- copy(forecast_DT[,.(issueTime,targetTime_UK)])
  for(q in 1:19/20){
    pb_temp[,(paste0("q",100 * q)):=(actuals[,n_attendance] - forecast_DT[[paste0("q",100 * q)]]) * q * (actuals[,n_attendance] >= forecast_DT[[paste0("q",100 * q)]]) +
              (actuals[,n_attendance] - forecast_DT[[paste0("q",100 * q)]]) * (q - 1) * (actuals[,n_attendance] < forecast_DT[[paste0("q", 100 * q)]])]
  }
  
  pb_temp[,(method_name):=rowMeans(pb_temp[,-c(1:2)])]
  
  PB_ts <<- merge(PB_ts,pb_temp[,.(issueTime,targetTime_UK,temp_name=get(method_name))],
                  by=c("issueTime","targetTime_UK"),all.x=T)
  setnames(PB_ts,"temp_name",method_name)
}




## Jethro's Results ####

load("JethroResults_2021-05-06.Rda")

for(n in names(JB_results)){
  
  if(!exists("PB_ts")){
    PB_ts <- copy(JB_results[[n]][issueTime<=last_issue,.(issueTime,targetTime_UK)])
  }
  
  big_eval_function(forecast_DT = JB_results[[n]],h2_actuals = h2,method_name = n)
}

rm(JB_results)

## Bahman's Results ####

tbats <- data.table(readRDS("tbats.rds"))
setnames(tbats,old = c("origin","target"),c("issueTime","targetTime_UK"))
tbats[,issueTime := issueTime+3600]

big_eval_function(forecast_DT = tbats,h2_actuals = h2,method_name = "tbats")

rm(tbats)

tbats <- data.table(readRDS("tbats_refit.rds"))
setnames(tbats,old = c("origin","target"),c("issueTime","targetTime_UK"))
tbats[,issueTime := issueTime+3600]

big_eval_function(forecast_DT = tbats,h2_actuals = h2,method_name = "tbats_refit")

rm(tbats)


## Ivan's Results ####

load("IvanValues.RData")

for(n in names(quantileValuesIvan)){
  
  big_eval_function(forecast_DT = data.table(quantileValuesIvan[[n]]),
                    h2_actuals = h2,method_name = n)
}

rm(quantileValuesIvan)



## Visualise all Results ####

## Pinball
ggplot(data=PB[Horizon=="All" & Issue == "All",],aes(x=Quantile,y=Loss,group=Method,shape=Method,color=Method)) +
  geom_line() + geom_point() + ylab("Pinball Loss") + 
  ggtitle("Pinball Loss") + theme_bw()
ggsave("Pinball.png")

## Reliability
REL_nom <- data.table(Nominal=seq(0,1,by=0.05),
                      Empirical=seq(0,1,by=0.05),
                      `Quantile Bias`= 0,
                      Method="Nominal")

ggplot(data=REL[Horizon=="All" & Issue == "All",],aes(x=Nominal,y=Empirical,group=Method,color=Method)) +
  geom_line(data=REL_nom,aes(x=Nominal,y=Empirical), color="black",size=1.1,show.legend = F) +
  geom_line() + geom_point() +
  xlim(c(0,1)) + ylim(c(0,1)) + ggtitle("Reliability Diagram") +
  theme_bw() 
ggsave("Reliability.png")

## Quantile Bias

REL[,`Quantile Bias`:=Empirical-Nominal]

ggplot(data=REL[Horizon=="All" & Issue == "All",],aes(x=Nominal,y=`Quantile Bias`,group=Method,color=Method)) +
  geom_line(data=REL_nom,aes(x=Nominal,y=`Quantile Bias`), color="black",size=1.1,show.legend = F) +
  geom_line() + geom_point() +
  xlim(c(0,1)) + ylim(c(-0.2,0.2)) + ggtitle("Quantile Bias") +
  theme_bw() 
ggsave("QuantileBias.png")

## Tables
Res_sum <- merge(
  REL[kfold=="Test" & Horizon=="All" & Issue == "All",.(Qbias=mean(abs(Nominal-Empirical))),by="Method"],
  PB[kfold=="Test"  & Horizon=="All" & Issue == "All",.(PBLoss=mean(Loss)),by="Method"],
  by="Method"
)[order(Qbias),]
write.csv(Res_sum,row.names = F,file = "Results_Summary.csv")

## Performance by lead time

ggplot(PB[Horizon!="All",.(Loss=mean(Loss)),by=c("Horizon","Method","Issue")],
       aes(x=as.numeric(Horizon),y=Loss,color=Method)) + facet_wrap(facets = "Issue") +
  geom_line() + xlab("Lead-time [h]") + ylab("Pinball Loss")
ggsave("Pinball_LeadTime.png")

ggplot(REL[Horizon!="All",.(Loss=mean(abs(Nominal-Empirical))),by=c("Horizon","Method","Issue")],
       aes(x=as.numeric(Horizon),y=Loss,color=Method)) + facet_wrap(facets = "Issue") +
  geom_line() + xlab("Lead-time [h]") + ylab("Mean Absolute Quantile Bias")
ggsave("Qbias_LeadTime.png")



## significance testing ####

## Bootstraped skill scores
##
## Would like to set colMeans(..., na.rm=F)...
##
nboot <- 250
NAMES <- colnames(PB_ts[,-c(1:2)])
REF <- "Benchmark_2"
NAMES <- NAMES[which(!NAMES %in% REF)]

## Block bootstrap if >1
# acf(FC_data_e[,abs(`de Vilmarest-Joseph`)-abs(`Ziel-Florian`)],lag.max = 48)
Block <- 24

bootdata <- data.table(Sample=1:nboot)
for(i in 1:nboot){
  # bootdata[i,c(NAMES,REF):=as.list(colMeans(abs(FC_data_e[sample(1:.N,.N,replace = T),mget(c(NAMES,REF))])))]  
  bootdata[i,c(NAMES,REF):=as.list(colMeans(abs(PB_ts[issueTime>=test_start & issueTime<=last_issue,][
    rep(sample(1:(.N-Block+1),.N-Block+1,replace = T),each=Block)+rep(0:(Block-1),.N-Block+1),
    mget(c(NAMES,REF))]),na.rm = T))]  
}


plotdata <- melt(bootdata[,100*(get(REF)-.SD)/get(REF),.SDcols=NAMES],
                 measure.vars =  1:length(NAMES),variable.name = "Method",
                 value.name = "Skill Score")

## Merge Reliability for colouring...
plotdata <- merge(plotdata,REL[kfold=="Test" & Horizon=="All" & Issue == "All",.(Qbias=mean(abs(Nominal-Empirical))),by="Method"],
                  by="Method",all.x = T)

require(RColorBrewer)
ggplot(plotdata, aes(x=reorder(Method, -`Skill Score`), y=`Skill Score`, fill=Qbias)) + 
  ylab("Pinball Skill Score [%]") +
  geom_boxplot() + theme_classic() +
  ggtitle("Pinball Skill Score Relative to Benchmark 2") +
  theme(axis.text.x = element_text(angle = -80)) + #scale_fill_manual(values=cbPalette) +
  # scale_x_discrete(labels= paste0(substring(NAMES, 1,4),".")) +
  geom_hline(yintercept=0, linetype="dashed",size=0.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = "RdYlGn")),
                       limits=c(0,0.11))+
  labs(fill = "Quantile Bias",x="Method")  
ggsave("Skill_rel2bench.png")



ggplot(plotdata[`Skill Score`>-2,], aes(x=reorder(Method, -`Skill Score`), y=`Skill Score`, fill=Qbias)) + 
  ylab("Pinball Skill Score [%]") +
  geom_boxplot() + theme_classic() +
  ggtitle("Pinball Skill Score Relative to Benchmark 2") +
  theme(axis.text.x = element_text(angle = -80)) + #scale_fill_manual(values=cbPalette) +
  # scale_x_discrete(labels= paste0(substring(NAMES, 1,4),".")) +
  geom_hline(yintercept=0, linetype="dashed",size=0.5)+
  scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = "RdYlGn")),
                       limits=c(0,0.11))+
  labs(fill = "Quantile Bias",x="Method")  
ggsave("Skill_rel2bench_reduced.png")








