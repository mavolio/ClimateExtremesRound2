library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//ANPP//")

theme_set(theme_bw(12))

trt<-read.csv("CEE_treatments_2018.csv")%>%
  rename(Plot=plot)

dat<-read.csv("ANPP_2012-2017_combinedrawdata.csv")%>%
  select(-X)%>%
  mutate(TotGrass=Andro+Sorg+Grass,
         TotForb=Solidago+Forbs,
         Total=Andro+Sorg+Grass+Solidago+Forbs+Woody,
         Totnowood=Andro+Sorg+Grass+Solidago+Forbs)%>%
  filter(Total!=0)%>%
  mutate(drop=ifelse(Plot==210&year==2015|Plot==102&year==2014, 1, 0))%>%
  filter(drop!=1)


#histogram to look for outliers
dathist<-dat%>%
  group_by(Plot, Rep, year)%>%
  gather(type, value, c(Andro, Sorg, Grass, Solidago, Forbs, Woody, TotGrass, TotForb, Total, Totnowood))

ggplot(data=dathist, aes(x=value))+
  geom_histogram()+
  facet_wrap(~type, scales="free")

#looking for outliers using car package
subtot<-subset(dathist, type=="Total")
outliermodel<-lm(value~year, data=subtot)
plot(outliermodel)
outlierTest(outliermodel)


plotave<-dat%>%
  group_by(Plot, year)%>%
  summarize_at(vars(Andro, Sorg, Grass, Solidago, Forbs, Woody, TotGrass, TotForb, Total, Totnowood), mean)%>%
  left_join(trt)%>%
  gather(type, value, Andro:Totnowood)%>%
  mutate(biomass=value*10)%>%
  select(Plot, year, drt, type, biomass, block)%>%
  filter(drt!=".")

#export data for dave
plotave_total<-plotave%>%
  filter(type=="Total")

write.csv(plotave_total, "ANPP_2012-2017_plotaverages.csv", row.names=F)

#looking at plots over time
ggplot(data=subset(plotave, type=="Total"&drt=="PD-D"), aes(x=year, y=biomass, color=as.factor(Plot)))+
  geom_point()

#histogram to look for outliers
ggplot(data=plotave, aes(x=biomass))+
  geom_histogram()+
  facet_wrap(~type, scales="free")

trtave<-plotave%>%
  group_by(year, drt, type)%>%
  summarize(mean=mean(biomass), sd=sd(biomass), n=length(biomass))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=trtave, aes(x=year, y=mean, color=drt))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=0, ymax=Inf, alpha = .2, fill="light gray")+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  facet_wrap(~type, scales="free")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Treatment", labels=c("C->C", "C->D", "D->C", "D->D"), values=c("blue", "orange", "lightblue", "red"))

total<-ggplot(data=subset(trtave, type=="Total"), aes(x=year, y=mean, color=drt))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.075)+
  xlab("Year")+
  ylab(expression("Total ANPP (g m"*{}^{-2}*")"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  annotate(x=2011.9, y=900, "text", label="B")+
  annotate(x=2015, y=840, "text", label="*", size=8)+
  annotate(x=2017, y=860, "text", label="*", size=8)

##doing stats on total

m1<-lmer(biomass~drt*as.factor(year)+(1|block/Plot)
              ,data=subset(plotave, type=="Total"))
summary(m1)
anova(m1, ddf="Kenward-Roger") #use this ddf for repeated measures.

#doing contrasts 
emmeans(m1, pairwise~drt|year, adjust="holm")

####another approach to doing contrasts on just 2015

m15<-lmer(biomass~drt+(1|block)
         ,data=subset(plotave, type=="Total"&year==2015))
summary(m15)
anova(m15, ddf="Kenward-Roger") #use this ddf for repeated measures.

#doing contrasts 
emmeans(m15, pairwise~drt, adjust="dunnettx")


#trying contrasts a different way
sub2014<-subset(plotave, type=="Total"&year==2014)
sub2015<-subset(plotave, type=="Total"&year==2015)

pairwise.t.test(x=sub2015$biomass, g=sub2015$drt)




##doing stats on grass
mg<-lmer(biomass~drt*as.factor(year)+(1|block/Plot)
         ,data=subset(plotave, type=="TotGrass"))
summary(mg)
anova(mg, ddf="Kenward-Roger") #use this ddf for repeated measures.

#doing contrasts 
emmeans(mg, pairwise~drt|year)

#trying contrasts a different way
subg2015<-subset(plotave, type=="TotGrass"&year==2015)
pairwise.t.test(x=subg2015$biomass, g=subg2015$drt)

###working on community production difference figure
ctdiff<-trtave%>%
  select(year, type, mean, drt)%>%
  filter(drt=="C-C"|drt=="PD-D", type=="Total")%>%
  spread(drt, mean)%>%
  group_by(year)%>%
  rename(c='C-C', d='PD-D')%>%
  mutate(pd=(d-c)/c)

