library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Soil respiration//")

theme_set(theme_bw(12))

trt<-read.csv("CEE_treatments_2018.csv")

sdat<-read.csv("CEE_SoilCO2Flux_PlotAverages_2012,14-17.csv")

ggplot(data=sdat, aes(x=soil.CO2.flux))+
  geom_histogram()

sdat2<-sdat%>%
  left_join(trt)%>%
  filter(drt!=".")%>%
  select(-heat1, -drt1, -drt2, -drt3)

trtaves<-sdat2%>%
  group_by(year, drt)%>%
  summarize(mean=mean(soil.CO2.flux), sd=sd(soil.CO2.flux), n=length(soil.CO2.flux))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(group=ifelse(year==2012, 1, 2),
         grp=paste(group, drt, sep=""))

soilresp<-ggplot(data=trtaves, aes(x=year, y=mean, color=drt))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line(aes(group=grp))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.075)+
  xlab("Year")+
  ylab(bquote('Soil Respiration ('*mu~ 'mol' ~CO[2]~ s^-1~m^-2*')')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.15,0.75))+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  annotate(x=2014, y=11, "text", label="*", size=8)+
  annotate(x=2015, y=11, "text", label="*", size=8)

##doing stats on total

ms<-lmer(soil.CO2.flux~drt*as.factor(year)+(1|block/plot)
         ,data=sdat2)
summary(ms)
anova(ms, ddf="Kenward-Roger") #use this ddf for repeated measures.

#doing contrasts 
emmeans(ms, pairwise~drt|year, adjust="holm")
