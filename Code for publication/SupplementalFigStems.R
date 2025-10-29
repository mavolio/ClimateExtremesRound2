library(tidyverse)
library(gridExtra)
library(lme4)
library(lmerTest)
library(emmeans)

theme_set(theme_bw(12))

setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//")

trt<-read.csv("ANPP//CEE_treatments_2018.csv")%>%
  rename(Plot=plot)

stems<-read.csv("Stem Data//All_live_springStems_11-18.csv")%>%
  left_join(trt)%>%
  filter(drt!=".") %>% 
  mutate(TotGrass=Andro+Sorg+Grass,
       TotForb=Solidago+Forb,
       Total=Andro+Sorg+Grass+Solidago+Forb+Wood,
       Totnowood=Andro+Sorg+Grass+Solidago+Forb,
       Other=Sorg+Grass+Solidago+Forb+Wood,
       Othernowood=Sorg+Grass+Solidago+Forb)%>%
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(Year>2012&Year<2017)


stems2<-read.csv("Stem Data//All_live_springStems_11-18.csv")%>%
  left_join(trt)%>%
  filter(drt!=".") %>% 
  pivot_longer(Andro:Wood, names_to = 'sp', values_to = 'stems') %>% 
  filter(Year>2012&Year<2017) %>% 
  group_by(Year, drt, sp) %>% 
  summarise(m=mean(stems), sd=sd(stems), n=length(stems)) %>% 
  mutate(se=sd/sqrt(n))

ggplot(data=stems2, aes(x=Year, y=m, color=sp))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+
  ylab("Number of Stems")+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  facet_wrap(~drt)

ggplot(data=subset(stems2, sp %in% c('Andro', 'Sorg')), aes(x=Year, y=m, color=sp))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+
  ylab("Number of Stems")+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  facet_wrap(~drt)


stems3<-read.csv("Stem Data//All_live_springStems_11-18.csv")%>%
  left_join(trt)%>%
  filter(drt!=".") %>% 
  pivot_longer(Andro:Wood, names_to = 'sp', values_to = 'stems') %>% 
  filter(Year>2012&Year<2017)


st2013<-stems3 %>% 
  filter(Year==2013) %>% 
  select(plotid, stems, sp) %>% 
  unique() %>% 
  rename(st13=stems)

stems4<-stems3 %>% 
  left_join(st2013) %>% 
  filter(Year!=2013) %>% 
  mutate(pd=st13-stems) %>% 
  group_by(Year, drt, sp) %>% 
  summarise(m=mean(pd), sd=sd(pd), n=length(pd)) %>% 
  mutate(se=sd/sqrt(n))

mst<-lmer(stems~drt*sp*as.factor(Year)+(1|block/Plot), data=subset(stems3, sp %in% c('Andro', 'Sorg')))
summary(mst)
anova(mst) #use this ddf for repeated measures.
emmeans(mst, pairwise~drt|Year|sp)

ggplot(data=subset(stems2, sp %in% c('Andro', 'Sorg')), aes(x=Year, y=m, shape=sp, color=drt))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Treatment", breaks=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  xlab("Year")+
  ylab("Number of Stems")+
  facet_wrap(~sp)
 


