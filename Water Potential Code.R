waterpotential<-read.csv(file.choose(),header=TRUE)


library(lme4)
library(lmerTest)
library(pbkrtest)
library(emmeans)
library(dplyr)
library(tidyverse)

##only 2015 data

waterpotential2015<-subset(waterpotential,year!="2014"&year!="2016")

Model<-lmer(wp~Treatment*Date+(1|Block/Plot)
            ,data=waterpotential
)
anova(Model,ddf="Kenward-Roger")
emmeans(Model,pairwise~Treatment|Date)
