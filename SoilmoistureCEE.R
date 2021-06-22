# CEE water potential plots

#
pkgs <- c("dplyr", "cowplot","ggplot2",
         "rstudioapi","tidyr")

#load
lapply(pkgs, library, character.only = TRUE)

moisture2014<-read.csv(file.choose(),header=TRUE)
moisture2015<-read.csv(file.choose(),header=TRUE)

moisture<-rbind(moisture2014,moisture2015)


library(car)
outliermodel<-lm(soil_moisture~Treatment,data=moisture)
plot(outliermodel)
outlierTest(outliermodel)

moisture2<-moisture[-c(12887),]

outliermodel<-lm(soil_moisture~Treatment,data=moisture2)
plot(outliermodel)
outlierTest(outliermodel)

moisture3<-moisture2[-c(10543,8725),]


#plot data: time series by year
time_series<-ggplot(moisture3,aes(x=day,y=soil_moisture,color=Treatment)) +
  facet_wrap(~year) +
  stat_summary(geom="line",fun.y="mean",size=0.5,aes(color=Treatment,group=Treatment)) +
  scale_fill_manual(values=c('C-C'='white','C-D'='grey70',
                             'PD-D'='red','PD-C'='blue')) + 
  ylab('Soil Moisture (%)') +
  xlab('Day') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=7),
    legend.position = c(0.92,0.3),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save 
pdf('CEE_soilmoisture_.pdf',width=7.5,height=3)
print(time_series)
dev.off()

#
#

##subset  days by doy:  get rid of anything before 91 and after 243

moisturegrowingseason<-subset(moisture3,day<"91",day>"243")

#barplot by year

std.error <- function(x){
  
  se<-sd(x)/sqrt(sum(!is.na(x)))
  return(se)
  
}

cee_mean_2 <- aggregate(soil_moisture~year+Treatment,mean,data=moisturegrowingseason)
colnames(cee_mean_2) <-c('year','Treatment','sm.mean')
cee_se_2 <- aggregate(soil_moisture~year+Treatment,std.error,data=moisturegrowingseason)
colnames(cee_se_2) <-c('year','Treatment','sm.se')

cee_time_series_2 <- merge(cee_mean_2,cee_se_2,by=c('year','Treatment'))

#plot data: time series by year
bar_plot<-ggplot(cee_time_series_2,aes(x=Treatment,y=sm.mean)) +
  facet_wrap(~year) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=sm.mean-sm.se, ymax=sm.mean+sm.se), width=.5) +
  stat_summary(geom='bar',fun = 'mean',color='black',fill='grey') +
  #scale_y_continuous(expand = c(0,0),limits = c(0,-3)) +
  # scale_x_continuous(expand = c(0,0),limits = c(-.0033,0.0082)) +
  ylab('Soil moisture (%)') +
  xlab('Year') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=7),
    legend.position = c(0.92,0.3),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save 
pdf('bar_plot.pdf',width=8,height=5)
print(bar_plot)
dev.off()

  