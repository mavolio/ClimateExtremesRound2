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
  select(Year, Plot, Andro, Sorg, Total, plotid,block, drt) %>% 
  pivot_longer(Andro:Total, names_to = 'Species', values_to = 'Stems')


#write csv for SAS
write.csv(stems, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Analyses in SAS//stems.csv', row.names=F)


#to plot Andro and Sorg
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

stemsfig<-
  ggplot(data=stems2, aes(x=drt, y=m, fill=sp, label=sig))+
  geom_bar(stat='identity', position=position_dodge(0.9))+
  scale_fill_manual(name='Species', labels=c('A. gerardii', 'S. nutans'), values=c('black', 'gray'))+
  scale_x_discrete(limits=c('C-C', 'PD-C', 'C-D', 'PD-D'), labels=c("C->C", "D->C", "C->D", "D->D"))+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), position=position_dodge(0.9), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Drought Treatment")+
  ylab("Number of Stems")+
  geom_text(position=position_dodge(0.9), aes(y=m+se*2))
stemsfig

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig_Stems.jpeg', stemsfig, width=5, height=4, units='in')



#to plot total
stemstotal<-stems%>% 
  filter(Species=='Total') %>% 
  group_by(drt, Year) %>% 
  summarise(m=mean(Stems), sd=sd(Stems), n=length(Stems)) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  mutate(sig=case_when(
    Year==2016 & drt=='PD-C' ~'A', 
    Year==2016 & drt=='PD-D' ~'B',
    Year==2016 & drt=='C-D' ~ 'B',
    Year==2016 & drt=='C-C' ~ 'AB', 
    .default = ''
  ))

stemstotalfig<-
  ggplot(data=stemstotal, aes(x=Year, y=m, color=drt, label=sig))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), values = c("blue",  "dodgerblue","orange", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
  geom_line()+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+
  ylab("Total Number of Stems")+
  geom_text(color="black", nudge_x = 0.15, size=3)+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")
stemstotalfig
ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig_TotalStems.jpeg', stemstotalfig, width=5, height=3, units='in')
