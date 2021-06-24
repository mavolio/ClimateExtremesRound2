#community composition through time

library(vegan)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(codyn)

theme_set(theme_bw(12))

#set wd
my.wd<-setwd("C:/Users/mavolio2/Dropbox/Konza Research")

#read in data
spdat<-read.csv(paste(my.wd, "/CEE_Part2/Sppcomp/Entered/spcomp_cee_2010_2017.csv", sep = ""))%>%
  filter(year>2011)

trt<-read.csv(paste(my.wd, "/CEE_Part2/Sppcomp/CEE_treatments_2018.csv", sep = ""))

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
  select(-cov1, -total)

# cattraits<-read.csv(paste(my.wd, "/pplots/traits_2021.csv", sep=""))%>%
#   filter(Genus!="")%>%
#   select(Genus, Species, Annual.Peren.Bi, Forb.grass.shrub, C3.C4, N.fixer..Y.N...)%>%
#   mutate(genus_species=paste(tolower(Genus), Species, sep="_"))%>%
#   rename(lifespan=Annual.Peren.Bi,
#          growthform=Forb.grass.shrub,
#          photopath=C3.C4,
#          Nfix=N.fixer..Y.N...)%>%
#   select(-Genus, -Species)

#get average cover of each species in a treatment for each year of the experiment (average over the plots)
# ave<-spdat2%>%
#   group_by(year, drt, spnum2)%>%
#   summarize(mabund=mean(cov1))

subyears<-reldat%>%
  filter(year==2016)

#make the dataset wide for vegan
compwide<-subyears%>%
  spread(spnum2, relcov, fill=0)

#pull out plot info
plots<-compwide[,1:4]

#run nmds
mds<-metaMDS(compwide[,5:85], trymax = 100)
mds

#extract NMDS coordinates and bind to plot info for graphs
#the label step adds the label for the first and last year of data
scores<-plots%>%
  bind_cols(as.data.frame(mds$points))

#%>%
  mutate(label=ifelse(year==2012|year==2015, as.character(year),""))%>%
  group_by(year, drt, label)


#%>%
  summarize(NMDS1=mean(MDS1), NMDS2=mean(MDS2), sd1=sd(MDS1), sd2=mean(MDS2))%>%
  mutate(se1=sd1/sqrt(8), se2=sd2/sqrt(8))

#make figure with first and last year labeled and path between points connected
NMDS<-
  ggplot(data=scores, aes(x=MDS1, y=MDS2, color=drt, label=label))+
  geom_point(size=3)+
  geom_path()+
  geom_text_repel(show.legend = F)+
  scale_color_manual(name="Treatment", values = c("blue", "orange", "lightblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("NMDS2")+
  xlab("NMDS1")#+
  annotate(x=-0.35, y = -0.4, "text", label= "Stress = 0.19")


#make figure with error bars fo year subsets
  ggplot(data=scores, aes(x=NMDS1, y=NMDS2, color=drt, label=label))+
    geom_point(size=3)+
    geom_text_repel(show.legend = F)+
    geom_errorbar(aes(ymin=NMDS2-se2, ymax=NMDS2+se2))+
    geom_errorbarh(aes(xmin=NMDS1-se1, xmax=NMDS1+se1))+
  scale_color_manual(name="Treatment", values = c("blue", "orange", "lightblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("NMDS2")+
    xlab("NMDS1")
  
  #make figure with error bars fo year subsets
  ggplot(data=scores, aes(x=MDS1, y=MDS2, color=drt))+
    geom_point(size=3)+
    scale_color_manual(name="Treatment", values = c("blue", "orange", "lightblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("NMDS2")+
    xlab("NMDS1")
  

#permanova on for each year
#run a for loop
permout=data.frame()
yrlist=unique(reldat$year)

for (i in 1:length(yrlist)){
  
  sub<-reldat%>%
    filter(year==yrlist[i])%>%
    spread(spnum2, relcov, fill=0)
  
  perm<-adonis(sub[5:length(sub)]~sub$drt)
  
  out<-data.frame(
    year=yrlist[i],
    f=perm$aov.tab$F.Model[1],
    p=perm$aov.tab$'Pr(>F)'[1]
  )
  permout<-rbind(permout, out)
}

padj<-permout%>%
  mutate(padj=p.adjust(p, method = "BH"))

sub2013<-spdat2%>%
  filter(year==2017)%>%
  spread(spnum2, cov1, fill=0)


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
  annotate(x=2011.9, y=0.35, "text", label="A")+
  annotate(x=2012, y=0.30, "text", label="*", size=8)+
  annotate(x=2014, y=0.23, "text", label="*", size=8)+
  annotate(x=2015, y=0.25, "text", label="*", size=8)+
  annotate(x=2016, y=0.36, "text", label="*", size=8)

#to make fig 2 also have to run the ANPP data stats to make total fig and the soil resp
fig2<-grid.arrange(compdiff, total, soilresp, ncol=1)
ggsave("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\CEE_Part2\\Manuscript\\Fig2.jpeg", fig2, height=300, width=125, units="mm", dpi=300)


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

mult_chg<-multivariate_change(df=reldat, time.var="year", species.var='spnum2', abundance.var="relcov", replicate.var="plot", treatment.var = "drt", reference.time = 2012)

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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("rect", xmin=2013.5, xmax=2015.5, ymin=-Inf, ymax=Inf, alpha = .2, fill="gray")



##to make RACs - not sure are going to do this.
#get average cover of each species in each treatment over all years (average over plots and years). 
#join categorical triats and create trait categories
racave<-ave%>%
  group_by(treatment, genus_species)%>%
  summarize(mabund=mean(mabund))%>%
  mutate(rank=rank(-mabund, ties.method = "first"))%>%
  left_join(cattraits)%>%
  mutate(trait_cat=ifelse(growthform=="F"&lifespan=="A", "Annual Forb",
                          ifelse(growthform=="G"&lifespan=="A", "Annual Gram.",
                                 ifelse(growthform=="G"&photopath=="C3", "C3 Gram.",
                                        ifelse(growthform=="G"&photopath=="C4", "C4 Gram.",
                                               ifelse(growthform=='F'|growthform=="S"&Nfix=="N", "Non-N-Fixing Forb",
                                                      ifelse(growthform=='F'|growthform=="S"&Nfix=="Y", "N-Fixing Forb","UNK")))))))%>%
  separate(genus_species, into=c("genus", "species"), sep="_")%>%
  mutate(genera=toupper(substr(genus, 1, 1)),
         sp=paste(genera, species, sep=". "))%>%
  mutate(name=ifelse(rank<4, sp, ""))

#make new labels for facet_wrap step  
collabel<-c(
  N1P0="Control", 
  N1P3="P",
  N2P0="N",
  N2P3="N+P")

#great rac figure
rac<-
  ggplot(data=racave, aes(x=rank, y=mabund, label=name))+
  geom_line()+
  geom_point(aes(color=trait_cat), size=2)+
  scale_color_manual(name="Functional Type", values=c("forestgreen", "chartreuse3", "green", "darkblue", "lightblue", "deepskyblue"), breaks = c("C3 Gram.", "C4 Gram.", "Annual Gram.","Non-N-Fixing Forb", "N-Fixing Forb", "Annual Forb"))+
  geom_text_repel(max.overlaps = 8, size=3)+
  facet_wrap(~treatment, labeller = labeller(treatment=collabel))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Abundance")+
  xlab("Rank")

#bind both figures together.
grid.arrange(NMDS, rac, ncol=2)


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

