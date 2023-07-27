library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//")

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

#there all look normal to me, I am not going to log transform
ggplot(data=NPP, aes(x=log(anpp)))+
  geom_histogram()
ggplot(data=NPP, aes(x=bnpp))+
  geom_histogram()
ggplot(data=NPP, aes(x=npp))+
  geom_histogram()

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

#graphing this in a mirror plot
NPPmeans<-NPP %>% 
  group_by(drt, year) %>% 
  summarise(manpp=mean(anpp, na.rm=T), mbnpp=mean(bnpp, na.rm=T),
            sdanpp=sd(anpp,na.rm=T), sdbnpp=sd(bnpp,na.rm=T), n=length(bnpp)) %>% 
  mutate(seanpp=sdanpp/sqrt(n), sebnpp=sdbnpp/sqrt(n))


#2015 plot

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

  

##Resistance and Recovery (or resilience)

#For ANPP
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

#models
ggplot(data=baci.tot.2, aes(x=drt, y=val))+
  geom_boxplot()+
  facet_wrap(~variable, scales = "free")

m.resistance<-lmer(val~drt+(1|block), data=subset(baci.tot.2, 
                                                            variable == "Resistance"))
anova(m.resistance) 
emmeans(m.resistance, pairwise~drt, adjust = "tukey")

#we are going to call recovery resilience
m.recovery<-lmer(val~drt+(1|block), data=subset(baci.tot.2, 
                                                          variable == "Recovery"))
anova(m.recovery)  
emmeans(m.recovery, pairwise~drt, adjust = "tukey")

# #we are not going to include resilience
# m.resilience<-lmer(val~drt+(1|block/halfblock), data=subset(baci.tot.2, 
#                                                             variable == "Resilience"))
# anova(m.resilience)  
# emmeans(m.resilience, pairwise~drt, adjust = "tukey")

#graphing this

#means
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


#For BNPP
bnpp.2<-NPP%>%
  mutate(period=ifelse(year==2015, "During", ifelse(year == 2016, "After", "drop")))%>%
  filter(period != "drop")

baci.bnpp.1<-bnpp.2%>%
  select(-year, -npp, -anpp) %>% 
  pivot_wider(names_from = period, values_from = bnpp)%>%
  mutate(Recovery=(After-During)/During)

baci.bnpp.2<-baci.bnpp.1%>%
  pivot_longer(cols= During:Recovery, names_to = "variable", values_to = "val")


#models
ggplot(data=baci.bnpp.2, aes(x=val))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")


#we are going to call recovery resilience
m.bnpp.recovery<-lmer(val~drt+(1|block), data=subset(baci.bnpp.2, 
                                                variable == "Recovery"))
anova(m.bnpp.recovery)  
emmeans(m.bnpp.recovery, pairwise~drt, adjust = "tukey")

#means
baci.bnpp.3<-baci.bnpp.2%>%
  filter(variable=="Recovery") %>% 
  group_by(drt, variable)%>%
  summarise(mean= mean(val, na.rm=T), sdev=sd(val, na.rm=T), n=length(val)) %>% 
  mutate(se=sdev/sqrt(8))


ggplot(data=subset(baci.bnpp.3, variable == "Recovery"), aes(x=drt, y = mean, fill=drt))+
  geom_col(aes(y=mean), width = .5, color = 'black')+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  ylab("BNPP Recovery")+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=0.3, label="A", size=4)+
  annotate("text", x=2, y=0.9, label="A", size=4)+
  annotate("text", x=3, y=0.8, label="A", size=4)+
  annotate("text", x=4, y=3, label="B", size=4)

#For NPP
npp.2<-NPP%>%
  mutate(period=ifelse(year==2015, "During", ifelse(year == 2016, "After", "drop")))%>%
  filter(period != "drop")

baci.npp.1<-npp.2%>%
  select(-year, -bnpp, -anpp) %>% 
  pivot_wider(names_from = period, values_from = npp)%>%
  mutate(Recovery=(After-During)/During)

baci.npp.2<-baci.npp.1%>%
  pivot_longer(cols= During:Recovery, names_to = "variable", values_to = "val")


#models
ggplot(data=baci.npp.2, aes(x=val))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")


#we are going to call recovery resilience
m.npp.recovery<-lmer(val~drt+(1|block), data=subset(baci.npp.2, 
                                                     variable == "Recovery"))
anova(m.npp.recovery)  
emmeans(m.npp.recovery, pairwise~drt, adjust = "tukey")

#means
baci.npp.3<-baci.npp.2%>%
  filter(variable=="Recovery") %>% 
  group_by(drt, variable)%>%
  summarise(mean= mean(val, na.rm=T), sdev=sd(val, na.rm=T), n=length(val)) %>% 
  mutate(se=sdev/sqrt(8))


ggplot(data=subset(baci.npp.3, variable == "Recovery"), aes(x=drt, y = mean, fill=drt))+
  geom_col(aes(y=mean), width = .5, color = 'black')+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  scale_x_discrete(limits=c("C-C", 'PD-C', 'C-D', 'PD-D'), labels=c("C-C", 'D-C', 'C-D', 'D-D'))+
  ylab("NPP Recovery")+
  scale_fill_manual(values=c('blue', 'orange','dodgerblue', 'red')) +
  xlab("")+
  theme_bw(12)+
  theme(panel.grid = element_blank(), legend.position = "none")+
  annotate("text", x=1, y=0.3, label="A", size=4)+
  annotate("text", x=2, y=0.45, label="A", size=4)+
  annotate("text", x=3, y=0.85, label="A", size=4)+
  annotate("text", x=4, y=1.55, label="B", size=4)

##combine to one dataset
baci.tot.clean<-baci.tot.1 %>% 
  select(plot, drt, block, Resistance, Recovery) %>% 
  rename(ANPP.Resist=Resistance, ANPP.Recov=Recovery)

baci.bnpp.clean<-baci.bnpp.1 %>% 
  select(plot, drt, block, Recovery) %>% 
  rename(BNPP.Recov=Recovery)
allrecov<-baci.tot.clean %>% 
  left_join(baci.bnpp.clean)

write.csv(NPP, "C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\BNPP\\NPP_2015-2016.csv", row.names=F)
write.csv(allrecov, "C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\BNPP\\ResistRecov.csv", row.names=F)
