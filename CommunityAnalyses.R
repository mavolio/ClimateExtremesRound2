#community composition through time

library(vegan)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(codyn)
library(ggh4x) #need to get facet colors
library(ggpubr)
library(cowplot)
theme_set(theme_bw(12))

#set wd
my.wd<-setwd("C:/Users/mavolio2/Dropbox/Konza Research")

#read in data
spdat<-read.csv(paste(my.wd, "/CEE_Part2/Sppcomp/Entered/spcomp_cee_2010_2017.csv", sep = ""))%>%
  filter(year>2011)

trt<-read.csv(paste(my.wd, "/CEE_Part2/ANPP/CEE_treatments_2023.csv", sep = ""))

spdat2<-spdat%>%
  left_join(trt)%>%
  filter(drt!=".")%>%
  select(Block, plot, spnum2, cov1, year, drt)

totcov<-spdat2%>%
  group_by(plot, year, drt)%>%
  summarize(total=sum(cov1))
  
reldat<-spdat2%>%
  left_join(totcov)%>%
  mutate(relcov=cov1/total)%>%
  select(-cov1, -total) %>% 
  filter(2012<year&year<2017)

ggplot(data=subset(reldat, spnum2=="s2"), aes(x=year, y=relcov))+
  geom_point()+
  facet_wrap(~drt)

cattraits<-read.csv(paste(my.wd, "/pplots/traits_2021.csv", sep=""))%>%
  filter(Genus!="")%>%
  select(Species.Number, Genus, Species, Annual.Peren.Bi, Forb.grass.shrub, C3.C4, N.fixer..Y.N...)%>%
  mutate(genus_species=paste(tolower(Genus), Species, sep="_"))%>%
  rename(lifespan=Annual.Peren.Bi,
         growthform=Forb.grass.shrub,
         photopath=C3.C4,
         Nfix=N.fixer..Y.N...)%>%
  mutate(spnum2=paste("s",Species.Number, sep="")) %>% 
  select(-Genus, -Species) %>% 
  mutate(spnum2=ifelse(spnum2=='s533', 's127', spnum2))

#get average cover of each species in a treatment for each year of the experiment (average over the plots)
ave<-spdat2%>%
  group_by(year, drt, spnum2)%>%
  summarize(mabund=mean(cov1))

# subyears<-reldat%>%
#   filter(year==2016)

#make the dataset wide for vegan
compwide<-reldat%>%
  spread(spnum2, relcov, fill=0)

#pull out plot info
plots<-compwide[,1:4]

#run nmds
mds<-metaMDS(compwide[,5:85], trymax = 500)
mds

#extract NMDS coordinates and bind to plot info for graphs
#the label step adds the label for the first and last year of data
scores<-plots%>%
  bind_cols(as.data.frame(mds$points))

ggplot(data=scores, aes(x=MDS1, y=MDS2, color=drt))+
  geom_point(size=3)+
  scale_color_manual(name="Treatment", values = c("blue", "orange", "lightblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("NMDS2")+
  xlab("NMDS1")+
  facet_wrap(~year)+
  stat_ellipse(size=1, aes(color=drt))
  
  
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=drt, shape=as.factor(year)))+
    geom_point(size=3)+
    scale_color_manual(name="Treatment", values = c("blue", "orange", "lightblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("NMDS2")+
    xlab("NMDS1")+
    #stat_ellipse(size=1, aes(color=drt))+
  annotate(x=-0.8, y = 1, "text", label= "Stress = 0.19")
  
  
  #make figure with first and last year labeled and path between points connected
avescores<-scores%>%
   group_by(year, drt)%>%
  summarize(NMDS1=mean(MDS1), NMDS2=mean(MDS2), sd1=sd(MDS1), sd2=mean(MDS2))%>%
    mutate(se1=sd1/sqrt(8), se2=sd2/sqrt(8))

#make figure with error bars fo year subsets
nmdsfig<-
ggplot(data=avescores, aes(x=NMDS1, y=NMDS2, color=drt, label=year))+
    geom_point(size=5)+
    geom_text(hjust=0, vjust=-2)+
    geom_path()+
    geom_errorbar(aes(ymin=NMDS2-se2, ymax=NMDS2+se2))+
    geom_errorbarh(aes(xmin=NMDS1-se1, xmax=NMDS1+se1))+
  scale_color_manual(name="Treatment", values = c("blue", "orange", "dodgerblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
    ylab("NMDS2")+
    xlab("NMDS1")+
    annotate(x=-0.5, y = 0.2, "text", label= "Stress = 0.19")
nmdsfig  

#permanova and permdisp on for each year
#run a for loop
permout=data.frame()
yrlist=unique(reldat$year)

for (i in 1:length(yrlist)){
  
  sub<-reldat%>%
    filter(year==yrlist[i])%>%
    spread(spnum2, relcov, fill=0)
  
  perm<-adonis2(sub[5:length(sub)]~sub$drt)
  
  dist<-vegdist(sub[5:length(sub)], method='bray')  
  bd<-betadisper(dist, sub$drt)
  pbd<-permutest(bd)
  
    out<-data.frame(
    year=yrlist[i],
    permf=perm$F[1],
    permp=perm$'Pr(>F)'[1],
    betaf=pbd$tab$F[1],
    betap=pbd$tab$`Pr(>F)`[1]
  )
  permout<-rbind(permout, out)
}

padj<-permout%>%
  mutate(perm.padj=p.adjust(permp, method = "BH"),
         beta.padj=p.adjust(betap, method = "BH"))


###using coydn to track community changes thorugh time

mult_diff<-multivariate_difference(df=reldat, time.var="year", species.var='spnum2', abundance.var="relcov", replicate.var="plot", treatment.var = "drt", reference.treatment = "C-C")

compdiff<-ggplot(data=mult_diff, aes(x=year, y=composition_diff, color=drt2))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line()+
  xlab("Year")+
  ylab("Compositional Difference")+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate(x=2014, y=0.23, "text", label="*", size=8)+
  annotate(x=2015, y=0.25, "text", label="*", size=8)+
  annotate(x=2016, y=0.36, "text", label="*", size=8)

#to make fig 2 also have to run the ANPP data stats to make total fig and the soil resp
fig2<-grid.arrange(total, soilresp, compdiff, ncol=1)
ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\Fig2.jpeg", fig2, height=300, width=150, units="mm", dpi=300)


##what is different about the communties?
rankdiff<-RAC_difference(df=reldat, time.var="year", species.var='spnum2', abundance.var="relcov", replicate.var="plot", treatment.var="drt", reference.treatment = "C-C")

rankdiffmean<-rankdiff%>%
  gather(measure, value, richness_diff:species_diff)%>%
  group_by(year, drt2, measure)%>%
  summarize(mean=mean(value), sd=sd(value), n=length(value))%>%
  mutate(se=sd/sqrt(n))

rank<-ggplot(data=rankdiffmean, aes(x=year, y=mean, color=drt2))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)+
  facet_wrap(~measure, scales="free")+
  scale_color_manual(name="Treatment", label=c("C->D", "D->C", "D->D"), values=c("orange", "dodgerblue", 'red'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\RAC_diff.jpeg", rank, height=200, width=200, units="mm", dpi=300)

mult_chg<-multivariate_change(df=reldat, time.var="year", species.var='spnum2', abundance.var="relcov", replicate.var="plot", treatment.var = "drt", reference.time = 2013)

ggplot(data=mult_chg, aes(x=year2, y=composition_change, color=drt))+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")+
  geom_point(size=3)+
  geom_line()+
  xlab("Year")+
  ylab("Compositional Change")+
  scale_color_manual(name="Treatment", label=c("C->C", "C->D", "D->C", "D->D"), values=c("blue", "orange", "dodgerblue", 'red'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(total, compdiff, ncol=1)

rac_chg<-RAC_change(df=reldat, time.var="year", species.var='spnum2', abundance.var="relcov", replicate.var="plot")

rankchangemean<-rac_chg%>%
  left_join(trt)%>%
  gather(measure, value, richness_change:losses)%>%
  group_by(year2, drt, measure)%>%
  summarize(mean=mean(value), sd=sd(value), n=length(value))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=rankchangemean, aes(x=year2, y=mean, color=drt))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)+
  facet_wrap(~measure, scales="free")+
  scale_color_manual(name="Treatment", breaks=c("C-C", 'PD-C', "C-D", "PD-D"), labels=c("C->C", "D->C", "C->D", "D->D"), values=c("blue", "dodgerblue", "orange", "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())#+
  #annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")



##to make RACs - not sure are going to do this.
#get average cover of each species in each treatment over all years (average over plots and years). 
#join categorical triats and create trait categories
racave<-ave%>%
  filter(2012<year & year < 2017) %>% 
  group_by(year, drt, spnum2)%>%
  summarize(mabund=mean(mabund))%>%
  mutate(rank=rank(-mabund, ties.method = "first")) %>% 
  left_join(cattraits) %>%
  mutate(trait_cat=ifelse(growthform=="F"&lifespan=="A", "Annual Forb",
                          ifelse(growthform=="G"&lifespan=="A", "Annual Gram.",
                                 ifelse(growthform=="G"&photopath=="C3", "C3 Gram.",
                                        ifelse(growthform=="G"&photopath=="C4", "C4 Gram.",
                                               ifelse(growthform=='F'|growthform=="S"&Nfix=="N", "Non-N-Fixing Forb",
                                                      ifelse(growthform=='F'|growthform=="S"&Nfix=="Y", "N-Fixing Forb","UNK")))))))%>%
  separate(genus_species, into=c("genus", "species"), sep="_")%>%
  mutate(genera=toupper(substr(genus, 1, 1)),
         sp=paste(genera, species, sep=". "))


ggplot(data=racave, aes(x=rank, y=mabund, label=sp, color=trait_cat))+
  geom_point(size=2)+
  facet_grid(drt~year)+
  geom_text()+ 
  scale_color_manual(name="Functional Type", values=c("forestgreen", "chartreuse3", "green", "darkblue", "lightblue", "deepskyblue"), breaks = c("C3 Gram.", "C4 Gram.", "Annual Gram.","Non-N-Fixing Forb", "N-Fixing Forb", "Annual Forb"))

#top sp plot
topsp<-ave%>%
  filter(2012<year & year < 2017) %>% 
  group_by(drt, spnum2)%>%
  summarize(mabund=mean(mabund))%>%
  mutate(rank=rank(-mabund, ties.method = "first")) %>% 
  filter(rank<6) %>% 
  select(drt, spnum2)
 
topsptograph<-racave %>% 
  right_join(topsp) %>% 
  mutate(Drt=factor(drt, levels=c('C-C', 'PD-C', 'C-D', 'PD-D')))

strip_cee <- strip_themed(background_x = elem_list_rect(fill = list("blue", "dodgerblue", "orange", "red")))
labels=c('C-C'= 'C-C', 'C-D'='C-D', 'PD-C'='D-C', 'PD-D'='D-D')

racs<-
ggplot(data=topsptograph, aes(x=year, y=mabund, shape=sp, color=trait_cat))+
  geom_point(size=3, stroke = 1.5)+
  geom_line(size=1)+
  facet_wrap2(~Drt,  strip=strip_cee, labeller=labeller(Drt=labels), nrow=1)+
  scale_y_continuous(limits=c(0,100))+
  scale_shape_manual(name='Species', values=c(1:10))+
  scale_color_manual(name="Functional\nType", values=c("green3", "forestgreen","purple"), breaks = c("C3 Gram.", "C4 Gram.", "Non-N-Fixing Forb"), labels=c('C3 Gram.', 'C4 Gram.', 'Forb'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+
  ylab("Species Cover")+
  theme(strip.text = element_text(colour = 'white', face='bold'), legend.position = 'bottom')+
  guides(shape = guide_legend(nrow = 4), col=guide_legend(nrow=2))
racs
fig2<-grid.arrange(nmdsfig, racs)

pfig2 <- as_ggplot(fig2) +                                # transform to a ggplot
  draw_plot_label(label = c("A)", "B)"), size = 12,
                  x = c(0, 0), y = c(1, 0.5))
pfig2

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig2_May25.jpeg', pfig2, width=11, height=12, units='in')

####figure that dave wants of community and anpp in one fig - need ANPP code for this to work
comdat<-mult_diff%>%
  filter(drt2=="PD-D")%>%
  left_join(ctdiff)%>%
  select(year, pd, composition_diff)%>%
  gather(measure, value, pd:composition_diff)
  

cdDiff<-ggplot(data=comdat, aes(x=year, y=value, color=measure))+
  geom_point(size=3)+
  geom_line()+
  xlab("Year")+
  ylab('Control-Drought Difference')+
  scale_color_manual(values =c("black", "darkgreen"), name="", label=c("Community Composition\nDifference", "ANPP Percent Difference"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0)+annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")

ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\Control_Drought_diff.jpeg", cdDiff, height=100, width=150, units="mm", dpi=300)

