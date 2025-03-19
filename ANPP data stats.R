library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(magick)
library(jpeg)
library(gridExtra)
library(ggrepel)



setwd("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//ANPP//")



theme_set(theme_bw(12))

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
  filter(drop!=1) %>% 
  filter(year>2012&year<2017)

plot(dat$Andro, dat$Total)


#histogram to look for outliers
dathist<-dat%>%
  group_by(Plot, Rep, year)%>%
  gather(type, value, c(Andro, Sorg, Grass, Solidago, Forbs, Woody, TotGrass, TotForb, Total, Totnowood, Other))

ggplot(data=dathist, aes(x=log(value)))+
  geom_histogram()+
  facet_wrap(~type, scales="free")

#looking for outliers using car package
subtot<-subset(dathist, type=="Total")
outliermodel<-lm(value~year, data=subtot)
plot(outliermodel)
outlierTest(outliermodel)

plotave<-dat%>%
  group_by(Plot, year)%>%
  summarize_at(vars(Andro, Sorg, Grass, Solidago, Forbs, Woody, TotGrass, TotForb, Total, Totnowood, Other, Othernowood), mean)%>%
  left_join(trt)%>%
  gather(type, value, Andro:Othernowood)%>%
  mutate(biomass=value*10)%>%
  select(Plot, year, drt1,drt2, drt, type, biomass, halfblock, block)%>%
  filter(drt2!=".")

plot(plotave$Andro, plotave$Total)

#export data for dave
plotave_total<-plotave%>%
  filter(type=="Total"|type=="Total")

write.csv(plotave_total, "ANPP_2012-2017_plotaverages.csv", row.names=F)

#looking at plots over time
ggplot(data=subset(plotave, type=="Total"&drt=="PD-D"), aes(x=year, y=biomass, color=as.factor(Plot)))+
  geom_point()

#histogram to look for outliers
ggplot(data=plotave, aes(x=biomass))+
  geom_histogram()+
  facet_wrap(~type, scales="free")




##doing stats on total only for the paper

mtot<-lmer(biomass~drt*as.factor(year)+(1|block/Plot)
         , data=subset(plotave, type=="Total"))
anova(mtot) #use this ddf for repeated measures.

#doing contrasts 
emmeans(mtot, pairwise~drt|year, adjust="tukey")

trtave<-plotave%>%
  group_by(year, drt, type)%>%
  summarize(mean=mean(biomass), sd=sd(biomass), n=length(biomass))%>%
  mutate(se=sd/sqrt(n))

tottoplot<-trtave %>% 
  filter(type=="Total") %>% 
  mutate(label=ifelse(year==2015&drt=="C-C", "A", ifelse(year==2015&drt=="PD-C", "AB", ifelse(year==2015&drt=="C-D", "BC", ifelse(year==2015&drt=="PD-D", "C", "")))))

# ggplot(data=subset(trtave, type %in% c("Total",'Andro','Sorg','TotGrass','TotForb')), aes(x=year, y=mean, color=drt))+
#   annotate("rect", xmin=2013.5, xmax=2015.5, ymin=0, ymax=Inf, alpha = .2, fill="light gray")+
#   geom_point()+
#   geom_line()+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
#   facet_wrap(~type, scales="free")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   scale_color_manual(name="Treatment", labels=c("C->C", "C->D", "D->C", "D->D"), values=c("blue", "orange", "lightblue", "red"))+
#   facet_wrap(~type, scales="free")


# Making Figure 1 ---------------------------------------------------------
d<-ggplot(data=tottoplot, aes(x=year, y=mean, color=drt, label=label))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.075)+
  xlab("Year")+
  ylab(expression("Total ANPP (g m"*{}^{-2}*")"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  geom_text(color="black", nudge_x = 0.15, nudge_y = 20)

Treatments<-c('C-C', 'D-C', 'C-D', 'D-D')
Y2010<-c(1, 0, 1, 0)
Y2011<-c(1, 0, 1, 0)
Y2012<-c(1, 1, 1, 1)
Y2013<-c(1, 1, 1, 1)
Y2014<-c(1, 1, 0, 0)
Y2015<-c(1, 1, 0, 0)
Y2016<-c(1, 1, 1, 1)

treatfig<-data.frame(Treatments, Y2010, Y2011, Y2012, Y2013, Y2014, Y2015, Y2016) %>% 
  pivot_longer(Y2010:Y2016, names_to = 'year', values_to = 'drttrt') %>% 
  separate(year, into = c('prefix', 'Year'), sep=1)


b<-ggplot(data=treatfig, aes(x=Year, y=Treatments, fill=as.factor(drttrt)))+
  geom_tile()+
  scale_y_discrete(limits=c('D-D', 'D-C', 'C-D', 'C-C'))+
  scale_fill_manual(values=c('red', 'blue'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none')+
  ylab("")
b

#make ppt bell curve figure in ppt.r code - get cee_ppt2, and mean_ppt and sd_ppt from this file and quantiles
c<-ggplot(data=cee_ppt2, aes(x=ppt, y=dnorm, color=Trt, label=Yr))+
  scale_x_continuous(limits = c(0,1000))+
  scale_color_manual(values=c('blue', 'red'))+
  geom_vline(xintercept = quantile_.05, color='red')+
  geom_vline(xintercept = quantile_.95, color='blue')+
  annotate(geom = 'text', label='5%', x = 200, y=0.0015, color='red')+
  annotate(geom = 'text', label= '95%', x=875, y=0.0014, color='blue')+
  geom_point(size=4)+
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_ppt, sd = sd_ppt), color='black')+
  geom_text_repel(color='black', nudge_x = 50)+
  ylab('Probability density')+
  xlab('Growing season precipitation (mm)')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')

fig1<-grid.arrange(b, 
          arrangeGrob(c, d, ncol=2),
          nrow=2, heights=c(0.75, 2.25))

pfig1 <- as_ggplot(fig1) +                                # transform to a ggplot
  draw_plot_label(label = c("B)", "C)", "D)"), size = 12,
                  x = c(0, 0, 0.5), y = c(1, 0.75, 0.75))
pfig1
ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig1_Feb25.jpeg', pfig1, width=8, height=5, units='in')


# ##looking at 2014 2015 difference
# drtbuild<-plotave %>% 
#   filter(type=='Total', year==2014|year==2015, drt=='C-D'|drt=='PD-D') %>% 
#   select(year, Plot, drt, block, biomass) %>% 
#   pivot_wider(names_from = year, values_from = biomass, names_prefix = 'y') %>% 
#   mutate(drt2diff=y2015-y2014)
# 
# mdrt2<-lmer(drt2diff~drt+(1|block)
#            , data=drtbuild)
# anova(mdrt2) #use this ddf for repeated measures.
# emmeans(mdrt2, pairwise~drt, adjust="tukey")

# ##doing stats on Andro
# mandro<-lmer(log(biomass)~drt*as.factor(year)+(1|block/Plot)
#          ,data=subset(plotave, type=="Andro"))
# summary(mandro)
# anova(mandro, ddf="Kenward-Roger") #use this ddf for repeated measures.
# #doing contrasts 
# emmeans(mandro, pairwise~drt|year, adjust="holm")
# 
# ##doing stats on Sorg
# msorg<-lmer(log(biomass+0.01)~drt*as.factor(year)+(1|block/Plot)
#          ,data=subset(plotave, type=="Sorg"))
# summary(msorg)
# anova(msorg) #use this ddf for repeated measures.
# #doing contrasts 
# emmeans(msorg, pairwise~drt|year, adjust="holm")
# 
# ##doing stats on Grass
# mgrass<-lmer(log(biomass)~drt*as.factor(year)+(1|block/Plot)
#             ,data=subset(plotave, type=="TotGrass"))
# summary(mgrass)
# anova(mgrass) #use this ddf for repeated measures.
# #doing contrasts 
# emmeans(mgrass, pairwise~drt|year, adjust="holm")
# 
# ##doing stats on Forbs
# mforb<-lmer(log(biomass+0.01)~drt*as.factor(year)+(1|block/Plot)
#          ,data=subset(plotave, type=="TotForb"))
# summary(m1)
# anova(mforb, ddf="Kenward-Roger") #use this ddf for repeated measures.
# 
# #doing contrasts 
# emmeans(mforb, pairwise~drt|year, adjust="holm")
# 

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


##ANPP 2015, 2016
totand<-trtave%>%
  filter(type %in% c("Andro", "Sorg", "Forbs", "Woody", "Solidago", "Grass"), year==2015|year==2016)

ggplot(data=totand, aes(x=drt, y=mean, fill=type))+
  geom_bar(stat="identity")+
  #geom_errorbar(position="identity", aes(ymin=mean-se, ymax=mean+se), width=0.5)+
  facet_wrap(~year)

#2015-2016 diff
totand2<-plotave%>%
  filter(type %in% c("Total", "Andro", "Other"), year==2015|year==2016)%>%
  mutate(yr=ifelse(year==2016, "y2016", "y2015"))%>%
  ungroup()%>%
  select(yr, block, Plot, biomass, type, drt)%>%
  spread(yr, biomass)%>%
  mutate(change=y2016-y2015)%>%
  mutate(type2=factor(type, levels=c("Total","Andro","Other"), labels=c("Total","A. gerardii","Other"))) %>%
  filter(Plot!=210)

diffmean<-totand2%>%
  group_by(drt, type, type2)%>%
  summarize(mean=mean(diff, rm.na=T), sd=sd(diff), n=length(diff))%>%
  mutate(se=sd/sqrt(n))%>%
  mutate(stat=ifelse(type=="Andro"&drt=="C-C"|type=="Andro"&drt=="PD-C", "B", ifelse(type=="Andro"&drt=="C-D", "AB", ifelse(type=="Andro"&drt=="PD-D", "A", ""))))

difffig<-
ggplot(data=diffmean, aes(x=drt, y=mean, fill=drt))+
  geom_bar(stat="identity", color="black")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  scale_fill_manual(name="Treatment", breaks=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  facet_wrap(~type2)+
  xlab("Treatment")+
  ylab("ANPP Difference (2016-2015)")+
  scale_x_discrete(limits=c("C-C", "PD-C", "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  
ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\Fig4.jpeg", difffig, height=125, width=300, units="mm", dpi=300)

##doing stats on difference
#total
mt<-lmer(diff~drt+(1|block)
         ,data=subset(totand2, type=="Total"))
summary(mt)
anova(mt, ddf="Kenward-Roger") #use this ddf for repeated measures.

#andro
ma<-lmer(diff~drt+(1|block)
         ,data=subset(totand2, type=="Andro"))
summary(ma)
anova(ma, ddf="Kenward-Roger") #use this ddf for repeated measures.

#doing contrasts ##no longer sig b/c correct for multiple tests
emmeans(ma, pairwise~drt, adjust="holm")


#other

mo<-lmer(diff~drt+(1|block)
         ,data=subset(totand2, type=="Other"))
summary(mo)
anova(mo, ddf="Kenward-Roger") #use this ddf for repeated measures.


##correct pval

p<-c(0.08451, 0.02364, 0.8959)

p.adjust(p, method="BH")


  



# 
# ###working on community production difference figure
# ctdiff<-trtave%>%
#   select(year, type, mean, drt)%>%
#   filter(drt=="C-C"|drt=="PD-D", type=="Total")%>%
#   spread(drt, mean)%>%
#   group_by(year)%>%
#   rename(c='C-C', d='PD-D')%>%
#   mutate(pd=(d-c)/c)
# 
# ggplot(ceemeans15, aes(x=trt, fill = trt)) + 
#   geom_col(aes(y=mA), width = .5, color = 'black')+ 
#   geom_col(aes(y=-mB), width = .5, color = 'black')+
#   geom_errorbar(aes(ymin=mA-seA, ymax=mA+seA), width=.1) +
#   geom_errorbar(aes(ymin=-mB+seB, ymax=-mB-seB), width=.1) +
#   geom_hline(aes(yintercept = 0)) +
#   scale_y_continuous(limits = c(-550, 850)) +
#   ylab(expression(BNPP~(g~m^-2)~~ANPP~(g~m^-2)))+
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   xlab("")+
#   theme_bw(12) +
#   theme(panel.grid = element_blank(), legend.position = "none")
# 
# 
