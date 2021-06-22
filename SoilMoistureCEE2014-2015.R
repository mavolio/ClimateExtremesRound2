moisture2014<-read.csv(file.choose(),header=TRUE)
moisture2015<-read.csv(file.choose(),header=TRUE)

library(reshape2)

library(dplyr)

##2014
moisture2014a <- subset(moisture2014,select=-c(X))

moisture2014b<-melt(moisture2014a,id.vars=c("day","year"))

moisture2014c<-moisture2014b %>% 
  rename(
    ID = variable,
    soil_moisture = value 
  )


moisture2014c$ID<-gsub("sm","",as.character(moisture2014c$ID))

CEEtreatments<-read.csv(file.choose(),header=TRUE)

moisture2014treatment<-merge(moisture2014c,CEEtreatments,by="ID")

write.csv(moisture2014treatment, "CEE_daily soil moisture 2014_cleaneddata_061821.csv")

##2015

moisture2015a <- subset(moisture2015,select=-c(X))

moisture2015b<-melt(moisture2015a,id.vars=c("day","year"))

moisture2015c<-moisture2015b %>% 
  rename(
    ID = variable,
    soil_moisture = value 
  )


moisture2015c$ID<-gsub("sm","",as.character(moisture2015c$ID))



moisture2015treatment<-merge(moisture2015c,CEEtreatments,by="ID")

write.csv(moisture2015treatment, "CEE_daily soil moisture 2015_cleaneddata_061821.csv")

