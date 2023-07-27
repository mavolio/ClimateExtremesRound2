## cee drought and recovery round 2

library(lme4)
library(car)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(patchwork)

## testing treatment effects on stem density in 2015

# stems15<-read.csv("~/Desktop/Stem Data/Stem Data/CEE_StemDensity_Spring2015_5-6removed.csv")
# 
# str(stems15)
# stems15$Plot<-as.factor(stems15$Plot)
# stems15$Block<-as.factor(stems15$Block)
# 
# allstems15<-lmer(All ~ Trt + (1|Block), data=stems15)
# hist(residuals(allstems15))
# Anova(allstems15)
# 
# means15<-stems15%>%
#   group_by(Trt)%>%
#   summarize(mA=mean(Andro), mS=mean(Sorg), mAll=mean(All), mG=mean(Grass), mF=mean(Forb), mW=mean(Wood),
#             sdA=sd(Andro), sdS=sd(Sorg), sdAll=sd(All), sdG=sd(Grass), sdF=sd(Forb), sdW=sd(Wood),n=length(Andro))%>%
#   mutate(seA=sdA/sqrt(8), seS=sdS/sqrt(8), seAll=sdAll/sqrt(8), seG=sdG/sqrt(8), seF=sdF/sqrt(8), seW=sdW/sqrt(8), )
# 
# means15$Trt <- factor(means15$Trt, levels = c("C-C", "D-C", "C-D", "D-D"))
# 
# all_plot<-ggplot(means15,aes(x=Trt, y=mAll, fill=Trt)) +
#   geom_errorbar(aes(ymin=mAll-seAll, ymax=mAll+seAll), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("All Species") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# all_plot
# 
# grass_plot<-ggplot(means15,aes(x=Trt, y=mG, fill=Trt)) +
#   geom_errorbar(aes(ymin=mG-seG, ymax=mG+seG), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("Grasses") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# grass_plot
# 
# forb_plot<-ggplot(means15,aes(x=Trt, y=mF, fill=Trt)) +
#   geom_errorbar(aes(ymin=mF-seF, ymax=mF+seF), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("Forbs") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# forb_plot
# 
# wood_plot<-ggplot(means15,aes(x=Trt, y=mW, fill=Trt)) +
#   geom_errorbar(aes(ymin=mW-seW, ymax=mW+seW), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("Wood") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# wood_plot
# 
# andro_plot<-ggplot(means15,aes(x=Trt, y=mA, fill=Trt)) +
#   geom_errorbar(aes(ymin=mA-seA, ymax=mA+seA), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("Andropogon gerardii") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# andro_plot
# 
# sorg_plot<-ggplot(means15,aes(x=Trt, y=mS, fill=Trt)) +
#   geom_errorbar(aes(ymin=mS-seS, ymax=mS+seS), width=.5) +
#   stat_summary(geom='bar',fun = 'mean',color='black') +
#   ylab(expression(paste("# stems (0.1 m"^"2",")"))) +
#   xlab('Treatment') +
#   ggtitle("Sorgastrum nutans") +
#   scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
#   theme(
#     axis.text.x = element_text(color='black',size=15),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     legend.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     axis.ticks = element_line(color='black'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(),
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# sorg_plot


## testing treatment effects on ANPP, BNPP, NPP in 2015, 2016

BNPP<-read.csv("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//BNPP//CEE_ANPP_BNPP_WP_stems_2015.csv")

trts<-read.csv("C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//ANPP//CEE_treatments_2023.csv")
ANPP<-read.csv()

bnpp15<-read.csv("~/Desktop/CEE_R/CEE_IGCs_2015_R.csv")

npp<-bnpp %>% 
  left_join(trts)

bnppmodel<-lmer(bnpp ~ trt + (1|block), data=bnpp15)
hist(residuals(bnppmodel))
Anova(bnppmodel)

emmeans(bnppmodel, pairwise ~ trt)

bnppmeans15<-bnpp15%>%
  group_by(trt)%>%
  summarize(mB=mean(bnpp),
            sdB=sd(bnpp), nB=length(bnpp))%>%
  mutate(seB=sdB/sqrt(nB))

bnppmeans15$trt <- factor(bnppmeans15$trt, levels = c("C-C", "D-C", "C-D", "D-D"))

bnpp_plot<-ggplot(bnppmeans15,aes(x=trt, y=mB, fill=trt)) +
  geom_errorbar(aes(ymin=mB-seB, ymax=mB+seB), width=.5) +
  stat_summary(geom='bar',fun = 'mean',color='black') +
  ylab(expression(paste("BNPP (g m"^"2",")"))) +
  xlab('Treatment') +
  ggtitle("All Species") +
  scale_fill_manual(name="Treatment", values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme(
    axis.text.x = element_text(color='black',size=15),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=16),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks = element_line(color='black'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
bnpp_plot


## testing correlations of ANPP with WP, stem density, and BNPP in 2015

cee15<-read.csv("C:\\Users//mavolio2//Dropbox//Konza Research//CEE_Part2//BNPP//CEE_ANPP_BNPP_WP_stems_2015.csv")

str(cee15)
cee15$plot<-as.factor(cee15$plot)
cee15$block<-as.factor(cee15$block)

cee15<-cee15 %>% mutate(trt = fct_recode(trt,
                                                                 "D->C"="D_C",
                                                                 "D->D"="D_D",
                                                                 "C->C"="C_C",
                                                                 "C->D"="C_D"))

cee15$trt <- factor(cee15$trt, levels = c("C->C", "D->C", "C->D", "D->D"))

lm.anpp.bnpp<- lm(anpp~bnpp, data=cee15)
summary(lm.anpp.bnpp)

anpp.bnpp.plot <- ggplot(cee15, aes(x=bnpp, y=anpp, color=trt)) + 
  xlab(expression(BNPP~(g~m^-2))) + ylab(expression(ANPP~(g~m^-2))) + 
  geom_point() + 
  geom_text(x=80, y=1000, label=expression(paste("R"^"2"," = 0.38")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), axis.title.y = element_blank())
anpp.bnpp.plot

lm.wp.anpp<- lm(anpp~wp, data=cee15)
summary(lm.wp.anpp)

wp.anpp.plot <- ggplot(cee15, aes(x=wp, y=anpp, color=trt)) + 
  xlab('Water Potential (MPa)') + ylab(expression(ANPP~(g~m^-2))) + 
  geom_point() + 
  geom_text(x=-3.45, y=1000, label=expression(paste("R"^"2"," = 0.63")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = "none", axis.title.y = element_blank())
wp.anpp.plot

lm.stems.anpp<- lm(anpp~stems, data=cee15)
summary(lm.stems.anpp)

stems.anpp.plot <- ggplot(cee15, aes(x=stems, y=anpp, color = trt)) + 
  xlab(expression(paste("# of Stems (0.1 m"^"2",")"))) + ylab(expression(ANPP~(g~m^-2))) +
  geom_point() + 
  geom_text(x=45, y=1000, label=expression(paste("R"^"2"," = 0.05")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'dodger blue','orange',  'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = 'none')
stems.anpp.plot


anpp.corr2015<-(stems.anpp.plot + wp.anpp.plot + anpp.bnpp.plot)
anpp.corr2015

ggsave(filename = "ANPP Correlation Plots 2015.pdf",
       plot = anpp.corr2015,
       bg = "transparent",
       width = 12, height = 4, units = "in",
       dpi = 600)


lm.wp.npp<- lm(npp~wp, data=cee15)
summary(lm.wp.npp)

wp.npp.plot <- ggplot(cee15, aes(x=wp, y=npp, color=trt)) + 
  xlab('Water Potential (MPa)') + ylab(expression(NPP~(g~m^-2))) + 
  geom_smooth(method=lm, aes(group=1), color='black') +
  geom_point() + 
  scale_y_continuous(limits = c(0, 1275)) +
  geom_text(x=-3.15, y=1250, label=expression(paste("R"^"2"," = 0.72, p<0.001")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), axis.title.y = element_blank())
wp.npp.plot

lm.stems.npp<- lm(npp~stems, data=cee15)
summary(lm.stems.npp)

stems.npp.plot <- ggplot(cee15, aes(x=stems, y=npp, color = trt)) + 
  xlab(expression(paste("# of Stems (0.1 m"^"2",")"))) + ylab(expression(NPP~(g~m^-2))) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1275)) +
  geom_text(x=55, y=1250, label=expression(paste("R"^"2"," = 0.14, p = 0.07")), color='black', size=3) +
  scale_color_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = 'none')
stems.npp.plot


lm.wp.stems.npp<- lm(npp~wp + stems, data=cee15)
summary(lm.wp.stems.npp)

npp.corr2015<-(stems.npp.plot + wp.npp.plot)
npp.corr2015


fig3<-(mirror.plot + stems.npp.plot + wp.npp.plot)
fig3

ggsave(filename = "fig3.pdf",
       plot = fig3,
       bg = "transparent",
       width = 12, height = 4, units = "in",
       dpi = 600)



## testing correlations of ANPP with WP, stem density, and BNPP in 2016

cee16<-read.csv("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\BNPP\\CEE_ANPP_BNPP_WP_stems_2016.csv")

str(cee16)
cee16$plot<-as.factor(cee16$plot)
cee16$block<-as.factor(cee16$block)

cee16$trt <- factor(cee16$trt, levels = c("C-C", "D-C", "C-D", "D-D"))

lm.anpp.bnpp2<- lm(anpp~bnpp, data=cee16)
summary(lm.anpp.bnpp2)

anpp.bnpp.plot2 <- ggplot(cee16, aes(x=bnpp, y=anpp, color=trt)) + 
  xlab(expression(BNPP~(g~m^-2))) + ylab(expression(ANPP~(g~m^-2))) + 
  geom_point() + 
  geom_text(x=230, y=1200, label=expression(paste("R"^"2"," = 0.06")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), axis.title.y = element_blank())
anpp.bnpp.plot2

#lm.wp.anpp<- lm(anpp~wp, data=cee15)
#summary(lm.wp.anpp)

#wp.anpp.plot <- ggplot(cee15, aes(x=wp, y=anpp, color=trt)) + 
#  xlab('Water Potential (MPa)') + ylab(expression(ANPP~(g~m^-2))) + 
#  geom_point() + 
#  geom_text(x=-3.45, y=1000, label=expression(paste("R"^"2"," = 0.63")), color='black', size=3) +
#  scale_color_manual(name="Treatment",
#                     values=c('blue', 'orange', 'dodger blue', 'red')) +
#  theme_bw(12) +
#  theme(panel.grid = element_blank(), legend.position = "none", axis.title.y = element_blank())
#wp.anpp.plot

lm.stems.anpp2<- lm(anpp~stems, data=cee16)
summary(lm.stems.anpp2)

stems.anpp.plot2 <- ggplot(cee16, aes(x=stems, y=anpp, color = trt)) + 
  xlab(expression(paste("# of Stems (0.1 m"^"2",")"))) + ylab(expression(ANPP~(g~m^-2))) +
  geom_point() + 
  geom_text(x=65, y=1200, label=expression(paste("R"^"2"," = 0.00")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'orange', 'dodger blue', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = 'none')
stems.anpp.plot2

anpp.corr2016<-(stems.anpp.plot2 + anpp.bnpp.plot2)
anpp.corr2016

ggsave(filename = "ANPP Correlation Plots 2016.pdf",
       plot = anpp.corr2016,
       bg = "transparent",
       width = 8, height = 4, units = "in",
       dpi = 600)


#lm.wp.anpp<- lm(anpp~wp, data=cee15)
#summary(lm.wp.anpp)

#wp.anpp.plot <- ggplot(cee15, aes(x=wp, y=anpp, color=trt)) + 
#  xlab('Water Potential (MPa)') + ylab(expression(ANPP~(g~m^-2))) + 
#  geom_point() + 
#  geom_text(x=-3.45, y=1000, label=expression(paste("R"^"2"," = 0.63")), color='black', size=3) +
#  scale_color_manual(name="Treatment",
#                     values=c('blue', 'orange', 'dodger blue', 'red')) +
#  theme_bw(12) +
#  theme(panel.grid = element_blank(), legend.position = "none", axis.title.y = element_blank())
#wp.anpp.plot

lm.stems.npp2<- lm(npp~stems, data=cee16)
summary(lm.stems.npp2)

lm.stems.npp2<- lm(npp~wp, data=cee16)
summary(lm.stems.npp2)

stems.npp.plot2 <- ggplot(cee16, aes(x=stems, y=npp, color = trt)) + 
  xlab(expression(paste("# of Stems (0.1 m"^"2",")"))) + ylab(expression(NPP~(g~m^-2))) +
  geom_point() + 
  geom_text(x=65, y=1700, label=expression(paste("R"^"2"," = 0.00")), color='black', size=3) +
  scale_color_manual(name="Treatment",
                     values=c('blue', 'orange', 'dodger blue', 'red')) +
  theme_bw(12) +
  theme(panel.grid = element_blank())
stems.npp.plot2

#npp.corr2016<-(stems.npp.plot2 + npp.wp.plot2)
#npp.corr2016

ggsave(filename = "NPP Correlation Plots 2016.pdf",
       plot = stems.npp.plot2,
       bg = "transparent",
       width = 5, height = 4, units = "in",
       dpi = 600)



nppmodel<-lmer(npp ~ trt + (1|block), data=cee15)
hist(residuals(nppmodel))
Anova(nppmodel)

emmeans(nppmodel, pairwise ~ trt)

nppmeans15<-cee15%>%
  group_by(trt)%>%
  summarize(m=mean(npp, na.rm=TRUE), sd=sd(npp, na.rm=TRUE), n=length(npp))%>%
mutate(se=sd/sqrt(n))

nppmeans15$trt <- factor(nppmeans15$trt, levels = c("C-C", "D-C", "C-D", "D-D"))

npp_plot<-ggplot(nppmeans15, aes(x=trt, y=m, fill=trt)) +
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.5) +
  stat_summary(geom='bar',fun = 'mean',color='black') +
  ylab(expression(paste("NPP (g m"^"2",")"))) +
  xlab('Treatment') +
  ggtitle("All Species 2015") +
  scale_fill_manual(name="Treatment", values=c('blue', 'dodger blue', 'orange', 'red')) +
  theme_bw() +
  theme(
    axis.text.x = element_text(color='black',size=15),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=16),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks = element_line(color='black'),
    panel.grid = element_blank(), 
    panel.background = element_rect(fill=NA))
npp_plot

ggsave(filename = "NPP 2015.pdf",
       plot = npp_plot,
       bg = "transparent",
       width = 5, height = 4, units = "in",
       dpi = 600)


## ANPP and BNPP by treatment in 2015 mirror plot 

ceemeans15<-cee15%>%
  group_by(trt)%>%
  summarize(mB=mean(bnpp, na.rm=TRUE), mA=mean(anpp, na.rm=TRUE),
            sdB=sd(bnpp, na.rm=TRUE), sdA=sd(anpp, na.rm=TRUE), nB=length(bnpp), nA=length(anpp))%>%
  mutate(seB=sdB/sqrt(nB), seA=sdA/sqrt(nA))

ceemeans15$trt <- factor(ceemeans15$trt, levels = c("C->C", "D->C", "C->D", "D->D"))

mirror.plot <- ggplot(ceemeans15, aes(x=trt, fill = trt)) + 
  geom_col(aes(y=mA), width = .5, color = 'black')+ 
  geom_col(aes(y=-mB), width = .5, color = 'black')+
  geom_errorbar(aes(ymin=mA-seA, ymax=mA+seA), width=.1) +
  geom_errorbar(aes(ymin=-mB+seB, ymax=-mB-seB), width=.1) +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous(limits = c(-550, 850)) +
  ylab(expression(BNPP~(g~m^-2)~~~~~~~~~~~~~~~~~ANPP~(g~m^-2)))+
  scale_fill_manual(values=c('blue', 'dodger blue', 'orange', 'red')) +
  xlab("")+
  theme_bw(12) +
  theme(panel.grid = element_blank(), legend.position = "none")

mirror.plot

ggsave(filename = "ANPP_BNPP_2015_bargraph.pdf",
       plot = p,
       bg = "transparent",
       width = 6, height = 4, units = "in",
       dpi = 600)
