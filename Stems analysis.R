library(tidyverse)
library(gridExtra)

setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//")

trt<-read.csv("ANPP//CEE_treatments_2018.csv")%>%
  rename(Plot=plot)

stems<-read.csv("Stem Data//All_live_springStems_11-18.csv")%>%
  left_join(trt)%>%
  filter(drt!=".") %>% 
  mutate(TotGrass=Andro+Sorg+Grass,
       TotForb=Solidago+Forbs,
       Total=Andro+Sorg+Grass+Solidago+Forbs+Woody,
       Totnowood=Andro+Sorg+Grass+Solidago+Forbs,
       Other=Sorg+Grass+Solidago+Forbs+Woody,
       Othernowood=Sorg+Grass+Solidago+Forbs)%>%
  filter(Total!=0)%>%
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1) %>% 
  filter(year>2012&year<2017)




stems2016<-stems%>%
  filter(Year==2016)

#stats on andro 2016 stems
mstems<-lmer(Andro~drt+(1|block)
         ,data=stems2016)
summary(mstems)
anova(mstems, ddf="Kenward-Roger") #use this ddf for repeated measures.
#no sig diff in number of stems

mstems<-stems2016%>%
  group_by(drt)%>%
  summarize(mean=mean(Andro), sd=sd(Andro), n=length(Andro))%>%
  mutate(se=sd/sqrt(n))

stemsfig<-ggplot(data=mstems, aes(x=drt, y=mean, fill=drt))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_fill_manual(name="Treatment", breaks=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  xlab("Treatment")+
  ylab("Number of A. gerardii Stems")+
  scale_x_discrete(limits=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"))


#water potential
wp2016<-wp%>%
  filter(year==2016)%>%
  group_by(Plot, drt, block)%>%
  summarize(wp=mean(wp))

#stats on andro 2016 wp
mwp<-lmer(wp~drt+(1|block)
             ,data=wp2016)
summary(mwp)
anova(mwp, ddf="Kenward-Roger") #use this ddf for repeated measures.
#no sig diff in number of stems

mwp<-wp2016%>%
  group_by(drt)%>%
  summarize(mean=mean(wp), sd=sd(wp), n=length(wp))%>%
  mutate(se=sd/sqrt(n))

wpfig<-ggplot(data=mwp, aes(x=drt, y=mean, fill=drt))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_fill_manual(name="Treatment", breaks=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  xlab("Treatment")+
  ylab("Mid-day leaf water potential (Mpa)")+
  scale_x_discrete(limits=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"))

#have to run code in ANPP data stats to make difffig
fig4<-grid.arrange(difffig, arrangeGrob(stemsfig, wpfig, ncol=2), nrow = 2)

ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\Fig4.jpeg", fig4, height=200, width=250, units="mm", dpi=300)
