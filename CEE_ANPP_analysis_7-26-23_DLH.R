#CEE2 analysis
#
#
#DLH 6-22-19
####################################

#Libraries
library(tidyverse)
library(scales)
library(car)
library(lme4)
library(nlme)
library(stats)
library(multcomp)
library(emmeans)
library(lmerTest)
library(MuMIn)
library(piecewiseSEM)
library(broom)
library(lubridate)
library(multcompView)

####################################
#load data
anpp.1<-read.csv("H:/Collaborations/CEE2/analysis/data/ANPP_2012-2017_plotaverages_v2.csv")
trt<-read.csv(("H:/Collaborations/CEE2/analysis/data/CEE_treatments_2023.csv"))

####################################
anpp.1<-left_join(anpp.1, trt)

#resit, resil, recover
  anpp.2<-anpp.1%>%
    mutate(period=ifelse(year==2013, "Before", ifelse(year==2015, "During", ifelse(year == 2016, "After", "drop"))))%>%
    filter(period != "drop")
  
  baci.tot.1<-anpp.2%>%
    group_by(drt, period, Plot, block, halfblock)%>%
    summarise(tot.mean = mean(biomass, na.rm=T))%>%
    pivot_wider(names_from = period, values_from = tot.mean)%>%
    mutate(Resistance=(During-Before)/Before,
           Resilience=(After-Before)/Before,
           Recovery=(After-During)/During)
  
  baci.tot.2<-baci.tot.1%>%
    pivot_longer(cols= After:Recovery, names_to = "variable", values_to = "val")
    
  


#means
  baci.tot.3<-baci.tot.2%>%
    group_by(drt, variable)%>%
    summarise(mean= mean(val, na.rm=T))
   

#models
  ggplot(data=baci.tot.2, aes(x=val))+
    geom_histogram()+
    facet_wrap(~variable, scales = "free")
  
  m.resistance<-lmer(val~drt+(1|block/halfblock), data=subset(baci.tot.2, 
                                                         variable == "Resistance"))
    
    anova(m.resistance) 
  
    emmeans(m.resistance, pairwise~drt, adjust = "tukey")
  
    
  m.recovery<-lmer(val~drt+(1|block/halfblock), data=subset(baci.tot.2, 
                                                      variable == "Recovery"))
    anova(m.recovery)  
    
    emmeans(m.recovery, pairwise~drt, adjust = "tukey")
    
    
  m.resilience<-lmer(val~drt+(1|block/halfblock), data=subset(baci.tot.2, 
                                                              variable == "Resilience"))
    anova(m.resilience)  
    
    emmeans(m.resilience, pairwise~drt, adjust = "tukey")
  
  ggplot(data=subset(baci.tot.2, variable == "Resistance"), aes(x=drt, y = val, color=as.factor(block)))+
    geom_point()
    
  ggplot(data=subset(baci.tot.2, variable == "Resistance"), aes(x=drt, y = val))+
    geom_boxplot()
  
  ggplot(data=subset(baci.tot.2, variable == "Resilience"), aes(x=drt, y = val))+
    geom_boxplot()
