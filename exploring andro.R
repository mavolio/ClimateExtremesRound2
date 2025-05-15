library(gridExtra)
library(tidyverse)

theme_set(theme_bw(12))

setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//ANPP//")

trt<-read.csv("CEE_treatments_2023.csv")%>%
  rename(Plot=plot)

#only use 2013-2016
dat<-read.csv("ANPP_2012-2017_combinedrawdata.csv")%>%
  select(-X)%>%
  mutate(TotGrass=Andro+Sorg+Grass,
         TotForb=Solidago+Forbs,
         Total=Andro+Sorg+Grass+Solidago+Forbs+Woody,
         Totnowood=Andro+Sorg+Grass+Solidago+Forbs,
         Other=Sorg+Grass+Solidago+Forbs+Woody,
         Othernowood=Sorg+Grass+Solidago+Forbs)%>%
  filter(Total!=0)%>%
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1) 

alldat<-read.csv('C://Users//mavolio2//Dropbox//Grants//CEE_GRANT//Rcode//CEE_anpp_2010_2018.csv') %>% 
  left_join(trt) %>% 
  filter(drt3!=".") %>% 
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1)

means<-alldat %>% 
  group_by(Year, drt) %>%
  summarise(mandro=mean(Andro), mtot=mean(Total), msorg=mean(Sorg))

#C-C vs D-D 2011 andro biomass diff is 48% lower
#C-C vs D-C 2011 andro biomass is 46% lower
#C-C vs D-D 2014 andro biomass diff is 35% lower, so even less responsive to the drought......
#C-C vs C-D in 2014 andro is 1% more. ANDRO is not responding in these plots

#getting PD over time relative to C-C
c_c<-means %>% 
  filter(drt=='C-C') %>% 
  rename(ccandro=mandro,
         cctotal=mtot,
         ccsorg=msorg) %>% 
  select(-drt)

pdiff<-means %>% 
  filter(drt!='C-C') %>% 
  left_join(c_c) %>% 
  mutate(pdAndro=(mandro-ccandro)/ccandro,
         pdSorg=(msorg-ccsorg)/ccsorg,
         pdTot=(mtot-cctotal)/cctotal) %>% 
  pivot_longer(pdAndro:pdTot, names_to='Type', values_to = 'PDiff') %>% 
  filter(Year<2017)

pd<-ggplot(data=pdiff, aes(x=Year, y=PDiff, color=drt))+
  geom_point(size=3)+
  geom_line()+
  facet_wrap(~Type)+
  geom_hline(yintercept = 0)+
  scale_color_manual(name="Treatment", breaks=c('PD-C', "C-D", "PD-D"), labels=c("D->C", "C->D", "D->D"), values=c("dodgerblue", "orange", "red"))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  annotate("rect", xmin=2010, xmax=2011.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  theme(legend.position = 'none')

control<-c_c %>% 
  pivot_longer(ccandro:ccsorg, names_to='Type', values_to = 'Controls') %>% 
  filter(Year<2017)

contro<-ggplot(data=control, aes(x=Year, y=Controls))+
  geom_point()+
  geom_line()+  facet_wrap(~Type)

grid.arrange(pd,contro)


meanslong<-means %>% 
  pivot_longer(mandro:msorg, names_to='Type', values_to = 'biomass') %>% 
  filter(Year<2017)

biomass<-ggplot(data=meanslong, aes(x=Year, y=biomass, color=drt))+
  geom_point(size=3)+
  geom_line()+  
  facet_wrap(~Type)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  annotate("rect", xmin=2010, xmax=2011.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  theme(legend.position = 'top')
biomass


grid.arrange(biomass, pd)

cee_ppt <- read.csv('C://Users/mavolio2/Dropbox//Konza Research//CEE_Part2/Precipitation/CEE_ppt_2010_2018_v3.csv') %>% 
  rename(Year=Yr) %>% 
  select(-C, -D) %>% 
  pivot_longer(C_D:D_D, names_to = 'drt1', values_to = 'ppt') %>% 
  mutate(drt=ifelse(drt1=='C_D', 'C-D', ifelse(drt1=='D_C', 'PD-C', ifelse(drt1=='C_C', 'C-C', 'PD-D'))))

RUE<-meanslong %>% 
  left_join(cee_ppt) %>% 
  mutate(rue=biomass/ppt)

ruegraph<-ggplot(data=RUE, aes(x=Year, y=rue, color=drt))+
  geom_point(size=3)+
  geom_line()+  
  facet_wrap(~Type)+
  scale_color_manual(name="Treatment", breaks=c('C-C','PD-C','C-D','PD-D'), labels=c('C-C', 'D-C','C-D','D-D'), values=c('blue', 'dodgerblue','orange', 'red'))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  annotate("rect", xmin=2010, xmax=2011.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  theme(legend.position = 'none')

grid.arrange(biomass, pd, ruegraph)
