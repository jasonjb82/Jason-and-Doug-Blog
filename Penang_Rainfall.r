# Load required libraries 
library(plyr)
library(ggplot2)
library(lubridate)
library(date)
 
# Setting work directory
 
setwd("d:\\ClimData")
 
# Reading and reformatting GSOD raw data downloaded from NCDC
 
dat<-read.table("CDO2812586929956.txt",header=F,skip=1)
 
colnames(dat)<-c("stn","wban","yearmoda","temp","tempc","dewp","dewpc","slp","slpc","stp","stpc","visib","visibc","wdsp","wdspc","mxspd","gust","maxtemp","mintemp","prcp","sndp","frshtt")
 
dat$yearmoda <- strptime(dat$yearmoda,format="%Y%m%d")
 
dat$prcp <- as.character(dat$prcp)
dat$prcp1 <-as.numeric(substr(dat$prcp,1,4))
dat$prcpflag <- substr(dat$prcp,5,5)
 
# Convert precipitation from inches to mms
 
dat$rain  <- dat$prcp1*25.4
 
# Remove erronous values
 
dat$rain[dat$rain > 1000 ] <- NA
 
dat$year <- as.numeric(format(dat$yearmoda,"%Y"))
dat$month <- as.numeric(format(dat$yearmoda,"%m"))
dat$day <- as.numeric(format(dat$yearmoda,"%d"))
 
# Getting cumulative sum of rain/year
 
dat$date<-as.Date(dat$yearmoda)
 
 
# Subsetting required period
 
dat2 <- subset(dat, year >= 2005 & month <= 4 )
 
# Extracting required columns for transforming data
 
dat3 <- dat2[, c(25,29)]
 
# Replace na's with 0's for ddply function
 
dat3$rain[is.na(dat3$rain)] <- 0
 
dat3 <- ddply(dat3,.(year(date)),transform, cumRain = cumsum(rain))
 
dat4 <- ddply(dat3,.(date,year(date)),summarize, max = max(cumRain))
 
dat5 <- dat4[c(diff(as.numeric(substr(dat4$date, 9, 10))) < 0, TRUE), ]
 
dat5$year <- as.numeric(format(dat5$date,"%Y"))
dat5$month <- as.numeric(format(dat5$date,"%m"))
dat5$day <- as.numeric(format(dat5$date,"%d"))
 
# Calculate julian day for labeling
 
dat5$jday <- strptime(dat5$date, "%Y-%m-%d")$yday+1
 
 
# Plot cumulative rainfall
 
plot.title = 'Cumulative Rainfall by Year - January to May (2005-2014)'
plot.subtitle = 'Data source : Federal Climate Complex, Global Surface Summary Of Day Data Version 7'
 
cr<-  ggplot(dat3, aes(x = yday(date), y = cumRain, color = factor(year(date)))) +
      geom_line(size=0.5,linetype='solid') + geom_point(size=1.5) + theme_bw() +
      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
      guides(color = guide_legend(title = "Year", title.position = "top")) +
      scale_x_continuous(breaks=c(0,30,60,90,120,150,180,210,240,270,300,330,360)) +
      scale_y_continuous(limits=c(0,800))+
      xlab("Julian Day") + ylab("Rainfall (mm)\n")+
      theme(panel.border = element_rect(colour = "black",fill=F,size=1),
      panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "ivory",colour = "black"),
      legend.position="right") 
 
cr <- cr + geom_text(data = subset(dat5, jday > 100 | year == 2014 & jday > 120 ), (aes(x = yday(date), y = max, label = year(date))),size=3,vjust=-0.2, hjust=-0.2)
 
cr 
 
ggsave(cr, file="Cumulative_RF_Penang.png", width=13, height=7,type = "cairo-png")