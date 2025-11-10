library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
#library(relaimpo)
library(ggpubr)
library(codyn)
library(gridExtra)
library(cowplot)

theme_set(theme_bw(12))
#read in data
setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//")

#read in data
ANPP<-read.csv("ANPP//ANPP_2013_2016plotaverages.csv") %>% 
  filter(type=='Total') %>% 
  rename(plot=Plot, anpp=biomass) %>%
  select(-type)

BNPP<-read.csv("BNPP//CEE_IGCs_2015-16_R.csv") %>% 
  select(year, plot, bnpp)

trt<-read.csv("ANPP//CEE_treatments_2023.csv")

NPP<-ANPP %>% 
  filter(year %in% c(2015, 2016)) %>% 
  left_join(BNPP) %>% 
  mutate(npp=anpp+bnpp)

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

sorgbiomasschange<-read.csv('ANPP\\ANPP_2012-2017_combinedrawdata.csv') %>% 
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  dplyr::select(Plot, Rep, Sorg, year) %>% 
  filter(year==2013|year==2015|year==2016) %>% 
  group_by(year, Plot) %>% 
  summarise(Sorgbiomass=mean(Sorg)) %>% 
  pivot_wider(names_from = year, values_from=Sorgbiomass, names_prefix = "y") %>% 
  mutate(SorgRecoverBiomass=y2016-y2015,
         SorgResistBiomass=y2015-y2013) %>% 
  rename(plot=Plot) %>% 
  dplyr::select(-y2015, -y2016, -y2013)

sorgstems<-read.csv("Stem Data//All_live_springStems_11-18.csv") %>%  
  select(Year, Plot, Sorg) %>% 
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(Year==2013|Year==2015|Year==2016) %>%
  rename(year=Year, plot=Plot) %>% 
  pivot_wider(names_from = year, values_from = Sorg, names_prefix = "y") %>% 
  mutate(SorgRecoverStems=y2016-y2015,
         SorgResistStems=y2015-y2013) %>% 
  select(-y2015,-y2016, -y2013)

wp<-read.csv('water_potential\\CEE_WP_allyears.csv') %>% 
  group_by(Plot, year) %>% 
  summarize(wp=mean(wp)) %>% 
  filter(year>2014&year<2017) %>% 
  rename(plot=Plot) %>% 
  pivot_wider(names_from=year, names_prefix = "y", values_from=wp) %>% 
  rename(wp2015=y2015, wp2016=y2016)

otherbiomasschange<-read.csv('ANPP\\ANPP_2012-2017_combinedrawdata.csv') %>% 
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  group_by(Plot, year, Rep) %>% 
  mutate(Other=Grass+Solidago+Forbs+Woody) %>% 
dplyr::select(Plot, Rep, Other, year) %>% 
  filter(year==2013|year==2015|year==2016) %>% 
  group_by(year, Plot) %>% 
  summarise(Otherbiomass=mean(Other)) %>% 
  pivot_wider(names_from = year, values_from=Otherbiomass, names_prefix = "y") %>% 
  mutate(OtherRecoverBiomass=y2016-y2015,
         OtherResistBiomass=y2015-y2013) %>% 
  rename(plot=Plot) %>% 
  dplyr::select(-y2015, -y2016, -y2013)

otherstems<-read.csv("Stem Data//All_live_springStems_11-18.csv") %>%  
  mutate(Other=Solidago+Grass+Forb+Wood) %>% 
  dplyr::select(Year, Plot, Other) %>% 
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(Year==2013|Year==2015|Year==2016) %>%
  rename(year=Year, plot=Plot) %>% 
  pivot_wider(names_from = year, values_from = Other, names_prefix = "y") %>% 
  mutate(OtherRecoverStems=y2016-y2015,
         OtherResistStems=y2015-y2013) %>% 
  dplyr::select(-y2015,-y2016, -y2013)

spdat<-read.csv("Sppcomp//Entered//spcomp_cee_2010_2017.csv")%>%
  filter(year==2013|year==2015|year==2016)

rich<-community_structure(spdat, time.var='year', abundance.var = 'cov1', replicate.var = 'plot') %>% 
  select(-Evar) %>% 
  pivot_wider(names_from = year, values_from = richness, names_prefix = 'y') %>%
  mutate(RichRecover=y2016-y2015, 
         RichResist=y2015-y2013) %>% 
  select(-y2013, -y2015, -y2016)


#calculate recovery and resistence
anpp.2<-ANPP%>%
  mutate(period=ifelse(year==2013, "Before", ifelse(year==2015, "During", ifelse(year == 2016, "After", "drop"))))%>%
  filter(period != "drop")

baci.tot.1<-anpp.2%>%
  select(-year) %>% 
  pivot_wider(names_from = period, values_from = anpp)%>%
  mutate(Resistance=(During-Before)/Before,
         Resilience=(After-Before)/Before,
         Recovery=(After-During)/During)

baci.tot.2<-baci.tot.1%>%
  pivot_longer(cols= After:Recovery, names_to = "variable", values_to = "val")

#write NPP and resist_recovery for SAS analyses
#write.csv(baci.tot.2, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Analyses in SAS//ResitResil.csv', row.names =F )

#write.csv(NPP, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Analyses in SAS//NPP_2015_2016.csv', row.names=F)

#Mechanisms 
MechData<-baci.tot.1 %>% 
  left_join(androstems) %>% 
  left_join(Androbiomasschange) %>% 
  left_join(wp) %>% 
  left_join(rich) %>% 
  left_join(sorgbiomasschange) %>% 
  left_join(sorgstems) %>% 
  left_join(otherbiomasschange) %>% 
  left_join(otherstems)

mresist<-lm(Resistance~wp2015+AndroResistStems+AndroResistBiomass+RichResist+SorgResistBiomass+SorgResistStems+OtherResistBiomass+OtherResistStems, data=MechData)
summary(mresist)
calc.relimp(mresist)

mrecov<-lm(Recovery~wp2016+AndroRecoverStems+AndroRecoverBiomass+RichRecover+SorgRecoverBiomass+SorgRecoverStems+OtherRecoverBiomass+OtherRecoverStems, data=MechData)
summary(mrecov)
calc.relimp(mrecov)


#NPP Figures
NPPmeans<-NPP %>% 
  group_by(drt, year) %>% 
  summarise(manpp=mean(anpp, na.rm=T), mbnpp=mean(bnpp, na.rm=T),
            sdanpp=sd(anpp,na.rm=T), sdbnpp=sd(bnpp,na.rm=T), n=length(bnpp)) %>% 
  mutate(seanpp=sdanpp/sqrt(n), sebnpp=sdbnpp/sqrt(n))

npp2015<-ggplot(subset(NPPmeans, year==2015), aes(x=drt, fill = drt)) + 
  geom_col(aes(y=manpp), width = .5, color = 'black')+ 
  geom_col(aes(y=-mbnpp), width = .5, color = 'black')+
  geom_errorbar(aes(ymin=manpp-seanpp, ymax=manpp+seanpp), width=.1) +
  geom_errorbar(aes(ymin=-mbnpp+sebnpp, ymax=-mbnpp-sebnpp), width=.1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"))+
  scale_y_continuous(breaks=seq(-450, 900, 200))+
  ylab(expression(BNPP~(g~m^-2)~~~~~~~~~~~~~~~ANPP~(g~m^-2)))+
  scale_fill_manual(values=c('blue', 'dodgerblue','orange', 'red'), limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D")) +
  xlab("")+
  theme_bw(12) +
  theme(panel.grid = element_blank())+
  annotate("text", x=1, y=870, label="A", size=4)+
  annotate("text", x=2, y=850, label="A", size=4)+
  annotate("text", x=3, y=640, label="B", size=4)+
  annotate("text", x=4, y=430, label="C", size=4)+
  annotate("text", x=1, y=-330, label="A", size=4)+
  annotate("text", x=2, y=-270, label="B", size=4)+
  annotate("text", x=3, y=-270, label="B", size=4)+
  annotate("text", x=4, y=-150, label="C", size=4)+
  annotate("text", x=1, y=50, label="A", size=4, fontface=2)+
  annotate("text", x=2, y=50, label="A", size=4, fontface=2)+
  annotate("text", x=3, y=50, label="B", size=4, fontface=2)+
  annotate("text", x=4, y=50, label="C", size=4, fontface=2)+
  labs(fill="Drought Treatment")
npp2015

#2016 plot

npp2016<-ggplot(subset(NPPmeans, year==2016), aes(x=drt, fill = drt)) + 
  geom_col(aes(y=manpp), width = .5, color = 'black')+ 
  geom_col(aes(y=-mbnpp), width = .5, color = 'black')+
  geom_errorbar(aes(ymin=manpp-seanpp, ymax=manpp+seanpp), width=.1) +
  geom_errorbar(aes(ymin=-mbnpp+sebnpp, ymax=-mbnpp-sebnpp), width=.1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"))+
  scale_y_continuous(breaks=seq(-450, 900, 200))+
  ylab(expression(BNPP~(g~m^-2)~~~~~~~~~~~~~~~ANPP~(g~m^-2)))+
  scale_fill_manual(values=c('blue', 'dodgerblue','orange', 'red'), limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D")) +
  xlab("")+
  theme_bw(12) +
  theme(panel.grid = element_blank())+
  labs(fill="Drought Treatment")
npp2016

###Resistance and Recovery BACI design figures
baci.tot.3<-baci.tot.2%>%
  filter(variable=="Resistance"|variable=="Recovery") %>% 
  group_by(drt, variable)%>%
  summarise(mean= mean(val, na.rm=T), sdev=sd(val, na.rm=T), n=length(val)) %>% 
  mutate(se=sdev/sqrt(8))

#resistance
anpp.resist<-ggplot(data=subset(baci.tot.3, variable == "Resistance"), aes(x=drt, y = mean, fill=drt))+
  geom_col(aes(y=mean), width = .5, color = 'black')+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"))+
  ylab("ANPP Resistance")+
  scale_fill_manual(values=c('blue', 'dodgerblue','orange', 'red'), limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D")) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=0.45, label="A", size=4)+
  annotate("text", x=2, y=0.35, label="AB", size=4)+
  annotate("text", x=3, y=0.2, label="BC", size=4)+
  annotate("text", x=4, y=-0.45, label="C", size=4)
anpp.resist

anpp.recov<-ggplot(data=subset(baci.tot.3, variable == "Recovery"), aes(x=drt, y = mean, fill=drt))+
  geom_col(aes(y=mean), width = .5, color = 'black')+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"))+
  ylab("ANPP Recovery")+
  scale_fill_manual(values=c('blue', 'dodgerblue','orange', 'red'), limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D")) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=0.25, label="A", size=4)+
  annotate("text", x=2, y=0.4, label="AB", size=4)+
  annotate("text", x=3, y=0.8, label="B", size=4)+
  annotate("text", x=4, y=1.4, label="C", size=4)
anpp.recov

#regression figures
wp2015<-
  ggplot(data=MechData, aes(x=wp2015, y=Resistance, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Resistance')+
  xlab('Water Potential')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")+
  labs(color="")
wp2015


wp2016<-
  ggplot(data=MechData, aes(x=wp2016, y=Recovery, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Recovery')+
  xlab('Water Potential')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black", linetype=3)
wp2016

AndrobiomassResist<-
  ggplot(data=MechData, aes(x=AndroResistBiomass, y=Resistance, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Resistance')+
  xlab('Change in A. gerardii Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")
AndrobiomassResist

AndrobiomassRecover<-
  ggplot(data=MechData, aes(x=AndroRecoverBiomass , y=Recovery, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Recovery')+
  xlab('Change in A. gerardii Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")
AndrobiomassRecover

SorgbiomassResist<-
  ggplot(data=MechData, aes(x=SorgResistBiomass, y=Resistance, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Resistance')+
  xlab('Change in S. nutans Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")
SorgbiomassResist

SorgbiomassRecover<-
  ggplot(data=MechData, aes(x=SorgRecoverBiomass , y=Recovery, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"), values=c('blue', 'dodgerblue','orange', 'red'))+
  ylab('ANPP Recovery')+
  xlab('Change in S. nutans Biomass')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth( method='lm', se=T, color="black")
SorgbiomassRecover

Fig3<-ggarrange(npp2015, anpp.resist, SorgbiomassResist,AndrobiomassResist, nrow=2, ncol=2, common.legend = T, legend='bottom')+
  draw_plot_label(label = c("A)", "C)", "B)", 'D)'), size = 12,
                  x = c(0, 0, 0.5, 0.5), y = c(1, 0.5, 1, 0.5))
Fig3

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig3_Nov3.jpeg', Fig3, width=8, height=8, units='in')

Fig4<-ggarrange(npp2016, anpp.recov, SorgbiomassRecover,AndrobiomassRecover, nrow=2, ncol=2, common.legend = T, legend='bottom')+
  draw_plot_label(label = c("A)", "C)", "B)", 'D)'), size = 12,
                  x = c(0, 0, 0.5, 0.5), y = c(1, 0.5, 1, 0.5))
Fig4

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig4_Nov3.jpeg', Fig4, width=8, height=8, units='in')
