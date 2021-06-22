ANPP2012<-read.csv(file.choose(),header=TRUE)
ANPP2013<-read.csv(file.choose(),header=TRUE)
ANPP2014<-read.csv(file.choose(),header=TRUE)
ANPP2015<-read.csv(file.choose(),header=TRUE)
ANPP2016<-read.csv(file.choose(),header=TRUE)
ANPP2017<-read.csv(file.choose(),header=TRUE)

library(dplyr)

##Make columns the same
ANPP2012 <- subset(ANPP2012,select=-c(X,Obs.))
ANPP2012 <- subset(ANPP2012,select=-c(Grass.Litter,Forb.Litter))
ANPP2014 <- subset(ANPP2014,select=-c(Grass.Litter,Forb.Litter))
ANPP2015 <- subset(ANPP2015,select=-c(notes))
ANPP2016 <- subset(ANPP2016,select=-c(Notes))
ANPP2017 <- subset(ANPP2017,select=-c(Notes))

##Rename 2012 reps
library(dplyr)
library(tidyverse)

ANPP2012<-ANPP2012 %>% mutate(Rep = fct_recode(Rep,
                                                         "1"="A",
                                                         "2"="B",
                                                         "3"="C",
))

##Add Years Column to all

ANPP2012<-ANPP2012 %>%
  mutate(newcol = 2012)

ANPP2012<-ANPP2012 %>% 
  rename(
    year = newcol,
  )

ANPP2013<-ANPP2013 %>%
  mutate(newcol = 2013)

ANPP2013<-ANPP2013 %>% 
  rename(
    year = newcol,
  )


ANPP2014<-ANPP2014 %>%
  mutate(newcol = 2014)

ANPP2014<-ANPP2014 %>% 
  rename(
    year = newcol,
  )


ANPP2015<-ANPP2015 %>%
  mutate(newcol = 2015)

ANPP2015<-ANPP2015 %>% 
  rename(
    year = newcol,
  )


ANPP2016<-ANPP2016 %>%
  mutate(newcol = 2016)

ANPP2016<-ANPP2016 %>% 
  rename(
    year = newcol,
  )


ANPP2017<-ANPP2017 %>%
  mutate(newcol = 2017)

ANPP2017<-ANPP2017 %>% 
  rename(
    year = newcol,
  )

##bind all years together

ANPP2012_2017<-rbind(ANPP2012,ANPP2013,ANPP2014,ANPP2015,ANPP2016,ANPP2017)

write.csv(ANPP2012_2017, "ANPP_2012-2017_combinedrawdata.csv")

