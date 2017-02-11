# Code to produce cumulative precipitation plot
# Load required libraries
 
library(plyr)
library(ggplot2)
library(lubridate)
library(date)
library(ggthemes)
 
# Setting work directory
 
setwd("d:\\ClimData")
 
# Reading and reformatting GSOD raw data downloaded from NCDC
 
dat<-read.table("CDO1553156579351.txt",header=F,skip=1)
 
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
 
dat2 <- subset(dat, year >= 2005 )
 
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
 
# Calculate julian day
 
dat5$jday <- strptime(dat5$date, "%Y-%m-%d")$yday+1
 
# Data frame with x-axis breaks and labels
 
dataxis <- dat5 %>% group_by(year) %>% mutate(daynum = seq_along(year))
 
df_xaxis <- dataxis %>% filter(year == 2014, month != lead(month)) %>%  # Last day of month
            mutate(days_in_month = daynum - lag(daynum),                # Days in month
            midpoint = lag(daynum) + days_in_month / 2)                 # Month midpoints
 
df_xaxis$midpoint[1] <- 31 / 2
 
# Plot cumulative rainfall
 
cr<-  ggplot(dat3, aes(x = yday(date), y = cumRain, color = factor(year(date)))) +
      geom_line(size=0.5,linetype='solid') + geom_point(size=1.2) + theme_bw() +
      ggtitle("Penang's Cumulative Rainfall by Year (2005 - 2014)") + 
      guides(color = guide_legend(title = "Year", title.position = "top")) +  
      geom_hline(yintercept = seq(0,3000, by=500), color = "wheat4",linetype="dotted",size=0.5) +
      geom_vline(xintercept = df_xaxis$jday, color = "wheat4", linetype = "dotted", size = 0.5) +
      geom_vline(xintercept = 0, color = "grey20", size = 1) + 
      scale_x_continuous(expand = c(0, 0), limits=c(0,380),
      breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
      labels = c("January", "February", "March", "April",
      "May", "June", "July", "August", "September",
      "October", "November", "December"))+
      scale_y_continuous(breaks=seq(0,3000,by=500))+ 
      xlab("") + ylab("Rainfall (mms)\n")+ 
      theme(panel.border = element_rect(colour="grey20",size=0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.x=element_blank(),
      legend.position="right",
      axis.title.y=element_text(size=14,face="plain",family="Clear Sans"),
      axis.text.x=element_text(size=12,face="plain",family="Clear Sans"),
      axis.text.y=element_text(size=12,face="plain",family="Clear Sans"),
      legend.text=element_text(size=10,face="plain",family="Clear Sans"),
      legend.title=element_text(size=10,face="bold",family="Clear Sans"),
      plot.title=element_text(size=20,face="bold",family="Clear Sans",hjust = 0.012, vjust = 1.2),
      legend.key=element_blank()) 
 
cr <- cr + geom_text(data = subset(dat5, jday > 350 ), (aes(x = yday(date), y = max, label = year(date))),size=4,vjust=-0.2, hjust=-0.2,fontface="bold",family="Clear Sans")
 
cr 
 
# Save plot to png
 
ggsave(cr, file="Cumulative_RF_Penang_r1.png", dpi=500,width=15, height=7,type = "cairo-png")