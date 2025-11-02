library(tidyverse)
library(gridExtra)
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
  filter(Year>2012&Year<2017) %>% 
  select(Year, Plot, Andro, Sorg, plotid,block, drt) %>% 
  pivot_longer(Andro:Sorg, names_to = 'Species', values_to = 'Stems')

#write csv for SAS
write.csv(stems, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Analyses in SAS//stems.csv', row.names=F)


#to plot
stems2<-read.csv("Stem Data//All_live_springStems_11-18.csv")%>%
  left_join(trt)%>%
  filter(drt!=".") %>% 
  mutate(drop=ifelse(Plot==210&Year==2015|Plot==102&Year==2014, 1, 0))%>%
  filter(drop!=1)%>% 
  pivot_longer(Andro:Wood, names_to = 'sp', values_to = 'stems') %>% 
  filter(Year>2012&Year<2017) %>% 
  group_by(drt, sp) %>% 
  summarise(m=mean(stems), sd=sd(stems), n=length(stems)) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  filter(sp %in% c('Andro', 'Sorg')) %>% 
  mutate(sig=case_when(
    sp=='Andro' & drt=='PD-C' ~'A', 
    sp=='Andro' & drt=='PD-D' ~'B',
    sp=='Andro' & drt=='C-D' ~ 'BC',
    sp=='Andro' & drt=='C-C' ~ 'C', 
    sp=='Sorg' & drt =='C-D' ~ 'DE',
    sp=='Sorg' & drt =='PD-C' ~ 'E', 
    .default = 'D'
  ))

stems<-
ggplot(data=stems2, aes(x=drt, y=m, fill=sp, label=sig))+
  geom_bar(stat='identity', position=position_dodge(0.9))+
  scale_fill_manual(name='Species', labels=c('A. gerardii', 'S. nutans'), values=c('black', 'gray'))+
  scale_x_discrete(limits=c('C-C', 'PD-C', 'C-D', 'PD-D'))+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), position=position_dodge(0.9), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Drought Treatment")+
  ylab("Number of Stems")+
  geom_text(position=position_dodge(0.9), aes(y=m+se*2))

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig_Stems.jpeg', stems, width=8, height=8, units='in')

