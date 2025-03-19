library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

theme_set(theme_bw(12))



setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//")

#read in data
ANPP<-read.csv("ANPP//ANPP_2012-2017_plotaverages.csv") %>% 
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

wp<-read.csv('water_potential\\CEE_WP_allyears.csv') %>% 
  group_by(Plot, year) %>% 
  summarize(wp=mean(wp)) %>% 
  filter(year>2014&year<2017) %>% 
  rename(plot=Plot) %>% 
  pivot_wider(names_from=year, names_prefix = "y", values_from=wp) %>% 
  rename(wp2015=y2015, wp2016=y2016)

spdat<-read.csv("Sppcomp//Entered//spcomp_cee_2010_2017.csv")%>%
  filter(year==2013|year==2015|year==2016)


###NPP Stats
#stats
m2015.anpp<-lmer(anpp~drt+(1|block), data=subset(NPP, year==2015))
anova(m2015.anpp)
emmeans(m2015.anpp, pairwise~drt, adjust="holm")

m2015.bnpp<-lmer(bnpp~drt+(1|block), data=subset(NPP, year==2015))
anova(m2015.bnpp)
emmeans(m2015.bnpp, pairwise~drt, adjust="holm")

m2015.npp<-lmer(npp~drt+(1|block), data=subset(NPP, year==2015))
anova(m2015.npp)
emmeans(m2015.npp, pairwise~drt, adjust="holm")

m2016.anpp<-lmer(anpp~drt+(1|block), data=subset(NPP, year==2016))
anova(m2016.anpp)
emmeans(m2016.anpp, pairwise~drt, adjust="holm")

m2016.bnpp<-lmer(bnpp~drt+(1|block), data=subset(NPP, year==2016))
anova(m2016.bnpp)
emmeans(m2016.bnpp, pairwise~drt, adjust="holm")

m2016.npp<-lmer(npp~drt+(1|block), data=subset(NPP, year==2016))
anova(m2016.npp)
emmeans(m2016.npp, pairwise~drt, adjust="holm")

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
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  scale_y_continuous(breaks=seq(-450, 900, 200))+
  ylab(expression(BNPP~(g~m^-2)~~~~~~~~~~~~~~~~~ANPP~(g~m^-2)))+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=870, label="A", size=4)+
  annotate("text", x=2, y=850, label="AB", size=4)+
  annotate("text", x=3, y=640, label="BC", size=4)+
  annotate("text", x=4, y=430, label="C", size=4)+
  annotate("text", x=1, y=-330, label="A", size=4)+
  annotate("text", x=2, y=-300, label="B", size=4)+
  annotate("text", x=3, y=-300, label="B", size=4)+
  annotate("text", x=4, y=-150, label="C", size=4)+
  annotate("text", x=1, y=50, label="A", size=4, fontface=2)+
  annotate("text", x=2, y=50, label="A", size=4, fontface=2)+
  annotate("text", x=3, y=50, label="B", size=4, fontface=2)+
  annotate("text", x=4, y=50, label="C", size=4, fontface=2)

#2016 plot

npp2016<-ggplot(subset(NPPmeans, year==2016), aes(x=drt, fill = drt)) + 
  geom_col(aes(y=manpp), width = .5, color = 'black')+ 
  geom_col(aes(y=-mbnpp), width = .5, color = 'black')+
  geom_errorbar(aes(ymin=manpp-seanpp, ymax=manpp+seanpp), width=.1) +
  geom_errorbar(aes(ymin=-mbnpp+sebnpp, ymax=-mbnpp-sebnpp), width=.1) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  scale_y_continuous(breaks=seq(-450, 900, 200))+
  ylab(expression(BNPP~(g~m^-2)~~~~~~~~~~~~~~~~~ANPP~(g~m^-2)))+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = "none")


###Resistance and Recovery BACI design
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
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  ylab("ANPP Resistance")+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")

anpp.recov<-ggplot(data=subset(baci.tot.3, variable == "Recovery"), aes(x=drt, y = mean, fill=drt))+
  geom_col(aes(y=mean), width = .5, color = 'black')+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  ylab("ANPP Recovery")+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=0.25, label="A", size=4)+
  annotate("text", x=2, y=0.4, label="A", size=4)+
  annotate("text", x=3, y=0.8, label="AB", size=4)+
  annotate("text", x=4, y=1.4, label="B", size=4)

