
# make  PDF of PPT

pkgs <- c("dplyr", "cowplot","ggplot2",
          "rstudioapi","tidyr")

#load
lapply(pkgs, library, character.only = TRUE)

# Set working directory 
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# barplot 

# Load file 
ppt<-read.csv("Manhattan_Climate_Daily1900-2012-1.csv")
head(ppt)

ppt_april_august <- ppt %>%
  dplyr::filter(Month < 9) %>%
  dplyr::filter(Month > 3)

summary(ppt_april_august)

ppt_april_august_total <- aggregate(Precip~Year,sum,data=ppt_april_august)
head(ppt_april_august_total)

hist(ppt_april_august_total$Precip)

test.dist <- dnorm(n=500, mean=mean_ppt,sd=sd_ppt)
toy.df <-  seq(10, 1000, length=500)
y <- dnorm(toy.df, mean=mean_ppt,sd=sd_ppt)

#get prob. densities 
dnorm.2010 <- dnorm(297.1, mean=mean_ppt,sd=sd_ppt)
dnorm.2011 <- dnorm(236.3, mean=mean_ppt,sd=sd_ppt)
dnorm.2014 <- dnorm(262.2, mean=mean_ppt,sd=sd_ppt)
dnorm.2015 <- dnorm(240.9, mean=mean_ppt,sd=sd_ppt)


pdf('PDF_rainfall.pdf',width=8,height=6)

plot(toy.df,y,lwd = 4,type = "l",ylab='Probability density',xlab='Growing season precipitation',cex.lab=1.25)
abline(v=221,add=TRUE,col='red',add=TRUE,lwd=3)
abline(v=717,add=TRUE,col='blue',add=TRUE,lwd=3)
#abline(v=427,add=TRUE,col='black',add=TRUE,lwd=2,lty='dotted')
points(297.1,0.0016,col='tan1',pch=19,cex=2.1,add=TRUE)
points(236.3,dnorm.2011,col='tan1',pch=19,cex=2.1)
points(262.2,dnorm.2014,col='red',pch=19,cex=2.1)
points(240.9,dnorm.2015,col='red',pch=19,cex=2.1)
text(180,0.002,'5%',cex=1.5)
text(770,0.002,'95%',cex=1.5)
legend(325, 0.0005, legend=c("First Drought", "Second Drought"),         #alpha legend: 0.015, 150
       col=c("tan1", "red"), lty=1.25,lwd=5,cex=1.25,box.lty=0)

dev.off()

#years and precip
#2010 = 297.1
#2011 = 236.3
#2014 = 262.2
#2015: 2040.9

# hist(ppt_april_august_total$Precip,xlab='Growing season precipitation',ylab='Frequency',cex.lab=1.25,main='',
#      breaks=50)
# abline(v=221,add=TRUE,col='red',add=TRUE,lwd=4)
# abline(v=717,add=TRUE,col='blue',add=TRUE,lwd=4)
# 
# #save 
# pdf('sm_bar_plot_growing_season.pdf',width=8,height=5)
# print(bar_plot_gs)
# dev.off()
