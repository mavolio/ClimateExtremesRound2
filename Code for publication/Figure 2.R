#community composition through time

library(vegan)
library(tidyverse)
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

  #make figure with first and last year labeled and path between points connected
avescores<-scores%>%
   group_by(year, drt)%>%
  summarize(NMDS1=mean(MDS1), NMDS2=mean(MDS2), sd1=sd(MDS1), sd2=mean(MDS2))%>%
    mutate(se1=sd1/sqrt(8), se2=sd2/sqrt(8))

#make figure with error bars fo year subsets
nmdsfig<-
ggplot(data=avescores, aes(x=NMDS1, y=NMDS2, color=drt, label=year))+
    geom_point(size=5)+
    geom_text(hjust=0, vjust=-2, show.legend = F)+
    geom_path()+
    geom_errorbar(aes(ymin=NMDS2-se2, ymax=NMDS2+se2))+
    geom_errorbarh(aes(xmin=NMDS1-se1, xmax=NMDS1+se1))+
  scale_color_manual(name="Treatment", values = c("blue", "orange", "dodgerblue", "red"), labels=c("C->C", "C->D", "D->C", "D->D"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("NMDS2")+
    xlab("NMDS1")+
    annotate(x=-0.4, y = 0.2, "text", label= "Stress = 0.17")

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



#get average cover of each species in each treatment over all years (average over plots and years). 
#join categorical triats and create trait categories

#get average cover of each species in a treatment for each year of the experiment (average over the plots)
ave<-spdat2%>%
  group_by(year, drt, spnum2)%>%
  summarize(mabund=mean(cov1))


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
  mutate(Drt=factor(drt, levels=c('C-C', 'PD-C', 'C-D', 'PD-D')),
         sp2=ifelse(sp=='A. ericoides', 'S. ericoides', sp)
         )

strip_cee <- strip_themed(background_x = elem_list_rect(fill = list("blue", "dodgerblue", "orange", "red")))
labels=c('C-C'= 'C-C', 'C-D'='C-D', 'PD-C'='D-C', 'PD-D'='D-D')

racs<-
ggplot(data=topsptograph, aes(x=year, y=mabund, shape=sp, color=trait_cat))+
  geom_point(size=3, stroke = 1.5)+
  geom_line(size=1)+
  facet_wrap2(~Drt,  strip=strip_cee, labeller=labeller(Drt=labels), nrow=1)+
  scale_y_continuous(limits=c(0,100))+
  scale_color_manual(name="Functional\nType", values=c("green3", "forestgreen","purple"), breaks = c("C3 Gram.", "C4 Gram.", "Non-N-Fixing Forb"), labels=c('C3 Gram.', 'C4 Gram.', 'Forb'))+
  scale_shape_manual(name='Species', values=c(1:10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Year")+
  ylab("Absolute Cover")+
  theme(strip.text = element_text(colour = 'white', face='bold'), legend.position = 'bottom', axis.text.x=element_text(angle=70, hjust=1))+
  guides(shape = guide_legend(nrow = 4), col=guide_legend(nrow=2))
racs
fig2<-grid.arrange(nmdsfig, racs)

pfig2 <- as_ggplot(fig2) +                                # transform to a ggplot
  draw_plot_label(label = c("A)", "B)"), size = 12,
                  x = c(0, 0), y = c(1, 0.5))
pfig2

ggsave('C://Users//mavolio2//Dropbox//Konza Research//CEE_Part2//Manuscript//Fig2_May25.jpeg', pfig2, width=7.6, height=8, units='in')

