library(gridExtra)
library(ggrepel)
library(tidyverse)
library(ggpubr)
library(cowplot)

theme_set(theme_bw(12))

###data
ppt<-read.csv("C://Users/mavolio2/Dropbox//Konza Research/CEE_Part2/Precipitation/Manhattan_Climate_Daily1900-2012.csv")
cee_ppt <- read.csv('C://Users/mavolio2/Dropbox//Konza Research//CEE_Part2/Precipitation/CEE_ppt_2010_2018_v3.csv')

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
  filter(drop!=1) %>% 
  filter(year>2012&year<2017)


ppt_april_august <- ppt %>%
  dplyr::filter(Month < 9) %>%
  dplyr::filter(Month > 3)
ppt_april_august_total <- aggregate(Precip~Year,sum,data=ppt_april_august)

#get long-term mean
mean_ppt <- mean(ppt_april_august_total$Precip)

#get SD
sd_ppt <- sd(ppt_april_august_total$Precip)

##getting points for histogram fig
cee_ppt2<-cee_ppt %>% 
  select(Yr, C, D) %>% 
  pivot_longer(C:D, names_to='Trt', values_to = 'ppt') %>%
  mutate(dnorm=dnorm(ppt, mean=mean_ppt, sd=sd_ppt)) %>% 
  filter(Yr<2017) %>% 
  mutate(drop=case_when(
    Yr==2012&Trt=='D' ~1,
    Yr==2013&Trt=='D'~ 1,
    Yr==2016&Trt=='D'~1,
    .default = 0
  )) %>% 
  filter(drop==0)

#getting quantiles
quantile_.05  <- qnorm(0.05,mean=mean_ppt,sd_ppt)
quantile_.07  <- qnorm(0.07,mean=mean_ppt,sd_ppt)
quantile_.9  <- qnorm(0.9,mean=mean_ppt,sd_ppt)
quantile_.95  <- qnorm(0.95,mean=mean_ppt,sd_ppt)

####getting averages of anpp
plotave<-dat%>%
  group_by(Plot, year)%>%
  summarize_at(vars(Andro, Sorg, Grass, Solidago, Forbs, Woody, TotGrass, TotForb, Total, Totnowood, Other, Othernowood), mean)%>%
  left_join(trt)%>%
  gather(type, value, Andro:Othernowood)%>%
  mutate(biomass=value*10)%>%
  select(Plot, year, drt1,drt2, drt, type, biomass, halfblock, block)%>%
  filter(drt2!=".")

write.csv(plotave, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Analyses in SAS//ANPP_2013_2016forSAS.csv', row.names=F)

write.csv(plotave, 'C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//ANPP//ANPP_2013_2016plotaverages.csv', row.names=F)


####Doing stats on ANPP. This is now being done in SAS, As I think they are better able to handle the complex experimental design
# mtot<-lmer(biomass~drt*as.factor(year)+(1|block/Plot)
#            , data=subset(plotave, type=="Total"))
# anova(mtot) #use this ddf for repeated measures.
# 
# #doing contrasts 
# emmeans(mtot, pairwise~drt|year, adjust="tukey")

#what percent is andro

pandro<-plotave %>%
  pivot_wider(names_from = 'type', values_from = 'biomass', values_fill = 0) %>%
  mutate(pandro=(Andro/Total)*100) %>%
  group_by(drt) %>%
  summarize(pandromean=mean(pandro))

#graphing this
andro<-plotave %>%
  pivot_wider(names_from = 'type', values_from = 'biomass', values_fill = 0) %>%
  mutate(pandro=(Andro/Total)*100) %>%
  group_by(year, drt) %>%
  summarize(pandromean=mean(pandro), sd=sd(pandro), n=length(pandro))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=andro, aes(x=year, y=pandromean, color=drt))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=pandromean-se, ymax=pandromean+se), width=0.075)+
  xlab("Year")+
  ylab(expression('Percent of ANPP that is A. gerardii'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))


#getting averages to make plot
trtave<-plotave%>%
  group_by(year, drt, type)%>%
  summarize(mean=mean(biomass), sd=sd(biomass), n=length(biomass))%>%
  mutate(se=sd/sqrt(n))

tottoplot<-trtave %>% 
  filter(type=="Total") %>% 
  mutate(label=ifelse(year==2015&drt=="C-C", "A", ifelse(year==2015&drt=="PD-C", "AB", ifelse(year==2015&drt=="C-D", 'BC', ifelse(year==2015&drt=='PD-D', "C", ifelse(year==2013&drt=='PD-C', "A", ifelse(year==2013&drt=='PD-D', 'B', ifelse(year==2013&drt=='C-C'|year==2013&drt=='C-D', 'AB', ""))))))))


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
  geom_text(color="black", nudge_x = 0.15, nudge_y = -10, size=3)

d
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
c

fig1<-grid.arrange(b,
                   arrangeGrob(c, d, ncol=2),
                   nrow=2, heights=c(1, 3))

pfig1 <- as_ggplot(fig1) +                                # transform to a ggplot
  draw_plot_label(label = c("B)", "C)", "D)"), size = 12,
                  x = c(0, 0, 0.5), y = c(1, 0.75, 0.75))
pfig1
#jpeg
ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig1_March25.jpeg', pfig1, width=8, height=5, units='in')
#pdf
ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig1_March25.pdf', pfig1, width=8, height=5, units='in')

