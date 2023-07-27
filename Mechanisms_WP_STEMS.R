library(tidyverse)
#library(lme4)
#library(lmerTest)
#library(emmeans)
#library(car)
library(gridExtra)
library(relaimpo)
library(codyn)

theme_set(theme_bw(12))

setwd('C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\')

NPP<-read.csv("BNPP\\NPP_2015-2016.csv")

Androbiomasschange<-read.csv('ANPP\\ANPP_2012-2017_combinedrawdata.csv') %>% 
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  select(Plot, Rep, Andro, year) %>% 
  filter(year==2013|year==2015|year==2016) %>% 
  group_by(year, Plot) %>% 
  summarise(Androbiomass=mean(Andro)) %>% 
  pivot_wider(names_from = year, values_from=Androbiomass, names_prefix = "y") %>% 
  mutate(AndroRecoverBiomass=y2016-y2015,
         AndroResistBiomass=y2015-y2013) %>% 
  rename(plot=Plot) %>% 
  select(-y2015, -y2016, -y2013)

resist<-read.csv('BNPP\\ResistRecov.csv')

androstems<-read.csv("Stem Data//All_live_springStems_11-18.csv") %>%  
  select(Year, Plot, Andro) %>% 
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(Year==2013|Year==2015|Year==2016) %>%
  rename(year=Year, plot=Plot) %>% 
  pivot_wider(names_from = year, values_from = Andro, names_prefix = "y") %>% 
  mutate(AndroRecoverStems=y2016-y2015,
         AndroResistStems=y2015-y2013) %>% 
  select(-y2015,-y2016, -y2013)

wp<-read.csv('water_potential\\CEE_WP_allyears.csv') %>% 
  group_by(Plot, year) %>% 
  summarize(wp=mean(wp)) %>% 
  filter(year>2014&year<2017) %>% 
  rename(plot=Plot) %>% 
  pivot_wider(names_from=year, names_prefix = "y", values_from=wp) %>% 
  rename(wp2015=y2015, wp2016=y2016)

spdat<-read.csv("Sppcomp//Entered//spcomp_cee_2010_2017.csv")%>%
  filter(year==2013|year==2015|year==2016)

#getting richness
rich<-community_structure(spdat, time.var='year', abundance.var = 'cov1', replicate.var = 'plot') %>% 
  select(-Evar) %>% 
  pivot_wider(names_from = year, values_from = richness, names_prefix = 'y') %>%
  mutate(RichRecover=y2016-y2015, 
         RichResist=y2015-y2013) %>% 
  select(-y2013, -y2015, -y2016)


#Mechanisms 
MechData<-resist %>% 
  left_join(androstems) %>% 
  left_join(Androbiomasschange) %>% 
  left_join(wp) %>% 
  left_join(rich)

mresist<-lm(ANPP.Resist~wp2015+AndroResistStems+AndroResistBiomass+RichResist, data=MechData)
summary(mresist)
calc.relimp(mresist)

mrecov<-lm(ANPP.Resist~wp2016+AndroRecoverStems+AndroRecoverBiomass+RichRecover, data=MechData)
summary(mrecov)
calc.relimp(mrecov)

wp2015<-
  ggplot(data=MechData, aes(x=wp2015, y=ANPP.Resist, color=drt))+
  geom_point()+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Resistance')+
  xlab('Water Potential')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")+
  annotate("text", x=-2.8, y=0.8, label=expression(paste("Partial R"^"2","=0.29; p=0.0006")))

AndrobiomassResist<-
  ggplot(data=MechData, aes(x=AndroResistBiomass, y=ANPP.Resist, color=drt))+
  geom_point()+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Resistance')+
  xlab('Change in A. gerardii Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")+
  annotate("text", x=-8, y=1, label=expression(paste("Partial R"^"2","=0.30; p=0.0004")))

AndrobiomassRecover<-
  ggplot(data=MechData, aes(x=AndroRecoverBiomass , y=ANPP.Recov, color=drt))+
  geom_point()+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Recovery')+
  xlab('Change in A. gerardii Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")+
  annotate("text", x=-3, y=1.5, label=expression(paste("Partial R"^"2","=0.33; p=0.001")))

grid.arrange(wp2015, AndrobiomassResist, AndrobiomassRecover)

