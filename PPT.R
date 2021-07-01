
# make  PDF of PPT

pkgs <- c("dplyr", "cowplot","ggplot2",
          "rstudioapi","tidyr")

#library(scales)

#load
lapply(pkgs, library, character.only = TRUE)

# Set working directory 
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# barplot 

# Load file 
ppt<-read.csv("Manhattan_Climate_Daily1900-2012.csv")
head(ppt)

#subset to april-august
ppt_april_august <- ppt %>%
  dplyr::filter(Month < 9) %>%
  dplyr::filter(Month > 3)

#summary(ppt_april_august)

#sum to get GSP
ppt_april_august_total <- aggregate(Precip~Year,sum,data=ppt_april_august)
head(ppt_april_august_total)

#get long-term mean
mean_ppt <- mean(ppt_april_august_total$Precip)

#get SD
sd_ppt <- sd(ppt_april_august_total$Precip)

cee_ppt <- read.csv('CEE_ppt_2010_2018_v2.csv')

#--------------
# PDF figure for figure 1 -------

# head(cee_ppt)
# summary(ppt_april_august_total)

#get prob. densities 

#droughts/recoveries
dnorm.2010 <- dnorm(297.1, mean=mean_ppt,sd=sd_ppt)
dnorm.2011 <- dnorm(236.3, mean=mean_ppt,sd=sd_ppt)
dnorm.2012 <- dnorm(626.5, mean=mean_ppt,sd=sd_ppt)
dnorm.2013 <- dnorm(552.2, mean=mean_ppt,sd=sd_ppt)
dnorm.2014 <- dnorm(262.2, mean=mean_ppt,sd=sd_ppt)
dnorm.2015 <- dnorm(240.9, mean=mean_ppt,sd=sd_ppt)
dnorm.2016 <- dnorm(710.3, mean=mean_ppt,sd=sd_ppt)
dnorm.2017 <- dnorm(449.3, mean=mean_ppt,sd=sd_ppt)
dnorm.2016.pnorm <- pnorm(710.3, mean=mean_ppt,sd=sd_ppt)

#controls
cee_ppt
dnorm.2014.c <- dnorm(448.7, mean=mean_ppt,sd=sd_ppt)
dnorm.2015.c <- dnorm(653.3, mean=mean_ppt,sd=sd_ppt)

#get normal distribution of PPT
toy.df<-rnorm(10000,mean=mean_ppt,sd_ppt)
y<-dnorm(toy.df,mean=mean_ppt,sd_ppt)


quantile_.05  <- qnorm(0.05,mean=mean_ppt,sd_ppt)
quantile_.95  <- qnorm(0.95,mean=mean_ppt,sd_ppt)

#make df to later merge
y.df<-as.data.frame(y)
y.df$id <- rownames(y.df)

toy.df.df <- as.data.frame(toy.df)
toy.df.df$id <- rownames(toy.df.df)

merge.y.toy <- merge(toy.df.df,y.df,by=c('id'))
merge.y.toy.ordered <- merge.y.toy %>%
  dplyr::arrange(toy.df)


pdf('PDF_rainfall.pdf',width=8,height=6)

plot(merge.y.toy.ordered$toy.df,merge.y.toy.ordered$y,type='l',
     ylab='Probability density',xlab='Growing season precipitation (mm)',
     cex.lab=1.25,ylim=c(.0001,0.0025),xlim=c(30,1050),pch=19,cex=1.5,lwd='2')
text(190,0.002,'5%',cex=1.5,col='red')
text(750,0.002,'95%',cex=1.5,col='blue')
# text(300,0.002,'10%',cex=1.5,col='red')
abline(v=quantile_.05,add=TRUE,col='red',add=TRUE,lwd=5)
abline(v=quantile_.95 ,add=TRUE,col='blue',add=TRUE,lwd=5)
#abline(v=252,add=TRUE,col='red',add=TRUE,lwd=3,lty=2)
#abline(v=427,add=TRUE,col='black',add=TRUE,lwd=2,lty='dotted')
#points(297.1,dnorm.2010,col='tan1',pch=21,cex=2.1,add=TRUE)
#points(236.3,dnorm.2011,col='tan1',pch=19,cex=2.1)

#drought years
points(262.2,dnorm.2014,col='black',bg='red',pch=21,cex=3)
text(305,dnorm.2014,'2014',cex=0.75)
points(240.9,dnorm.2015,col='black',bg='red',pch=21,cex=3)
text(285,dnorm.2015,'2015',cex=0.75)

#controls during drought years
points(448.7,dnorm.2014.c,col='black',bg='blue',pch=21,cex=3)
text(495,dnorm.2014.c,'2014',cex=0.75)
points(653.3,dnorm.2015.c,col='black',bg='blue',pch=21,cex=3)
text(610,dnorm.2015.c,'2015',cex=0.75)

#recovery years
points(710.3,dnorm.2016,col='black',bg='grey',pch=21,cex=3)
text(665,dnorm.2016,'2016',cex=0.75)
points(449.3,0.0022,col='black',bg='grey',pch=21,cex=3) #essentially same as 2014 control, so offset a little
text(405,0.0022,'2017',cex=0.75)
legend(370, 0.0008, legend=c("Drought","Control", "Recovery"),         #alpha legend: 0.015, 150
       col=c("red", "blue","grey"), lty=1.25,lwd=5,cex=1.25,box.lty=0)

dev.off()

#----