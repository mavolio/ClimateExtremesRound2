library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


setwd('C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\')

NPP<-read.csv("BNPP\\NPP_2015-2016.csv")
resist<-read.csv('BNPP\\ResistRecov.csv')
stems2015_2016<-read.csv("Stem Data//All_live_springStems_11-18.csv") %>%  
  mutate(TotalStems=Andro+Sorg+Grass+Solidago+Forb+Wood)%>%
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(Year>2014&Year<2017) %>% 
  select(Year, Plot, TotalStems) %>% 
  rename(year=Year, plot=Plot)

wp<-read.csv('water_potential\\CEE_WP_allyears.csv') %>% 
  group_by(Plot, year) %>% 
  summarize(wp=mean(wp)) %>% 
  filter(year>2014&year<2017) %>% 
  rename(plot=Plot)

#mechanisms for NPP
mechNPP<-NPP %>% 
  left_join(stems2015_2016) %>% 
  left_join(wp)

#2015
summary(lm(npp~TotalStems, data=subset(mechNPP, year==2015))) #not sig
summary(lm(npp~wp, data=subset(mechNPP, year==2015)))#sig

#2016
summary(lm(npp~TotalStems, data=subset(mechNPP, year==2016))) #not sig
summary(lm(npp~wp, data=subset(mechNPP, year==2016)))#not sig

ggplot(data=mechNPP, aes(x=TotalStems, y=npp, color=drt))+
  geom_point()+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  facet_wrap(~year, scales='free')+
  ylab('NPP')+
  xlab('Total Stems')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
ggplot(data=mechNPP, aes(x=TotalStems, y=wp, color=drt))+
  geom_point()+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  facet_wrap(~year, scales='free')+
  ylab('NPP')+
  xlab('Water Potential')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(data=subset(mechNPP, year==2015), method='lm', se=T, color="black")



#mechanisms for recovery resistance
stem2<-stems2015_2016 %>% 
  pivot_wider(names_from = year, values_from = TotalStems, names_prefix="y")

mechRecov<-resist %>% 
  left_join(stem2)

#ANPP
summary(lm(ANPP.Resist~y2015, data=mechRecov))#not sig
summary(lm(ANPP.Recov~y2016, data=mechRecov))#not sig

#I am not sure what to correlate BNPP with

