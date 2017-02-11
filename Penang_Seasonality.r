# Load package libraries
 
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(reshape2)
library(circular)
library(lubridate)
library(grid)
library(zoo)
library(gridExtra)
library(weathermetrics)
library(RColorBrewer)
 
# Setting work directory
 
setwd("d:\\ClimData")
 
# Reading and reformatting raw hourly data downloaded from NCDC
 
dat<-read.table("831677118578dat.txt",header=TRUE,fill=TRUE,na.strings=c("*","**","***","****","*****","******","0.00T*****"))
 
colnames(dat)<-tolower(colnames(dat))
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 8 * 60 * 60
 
dat$year <- as.numeric(format(dat$dates,"%Y"))
 
dat$month <- as.numeric(format(dat$dates,"%m"))
 
dat$hour<-substring(as.character(dat$dates),12,13)
 
dat$min<-substr(dat$dates,15,16)
 
dat$time<-paste(dat$hour,dat$min,sep=":")
 
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$dewpc <- (dat$dewp-32) * (5/9)
 
dat$tempc[dat$tempc<=10] <- NA
 
dat$tempc[dat$tempc>=40] <- NA
 
dat$dir[dat$dir == 990.0] <- NA 
 
dat$wspd <- (dat$spd)*0.44704
 
# Convert precipitation from inches to mms
 
dat$rain  <- dat$pcp24*25.4
 
# Calculate relative humidity & heat index using weathermetrics package
 
dat$rh <- dewpoint.to.humidity(t = dat$tempc, dp = dat$dewpc, temperature.metric = "celsius")
 
dat$hi <- heat.index(t = dat$tempc,rh = dat$rh,temperature.metric = "celsius",output.metric = "celsius",round = 2)
 
# Commands to reformat dates
 
dat$year <- as.numeric(as.POSIXlt(dat$dates)$year+1900)
dat$month <- as.numeric(as.POSIXlt(dat$dates)$mon+1)
dat$monthf <- factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
dat$weekday <- as.POSIXlt(dat$dates)$wday
dat$weekdayf <- factor(dat$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
dat$yearmonth <- as.yearmon(dat$dates)
dat$yearmonthf <- factor(dat$yearmonth)
dat$week <- as.numeric(format(as.Date(dat$dates),"%W"))
dat$hour <- as.numeric(format(strptime(dat$dates, format = "%Y-%m-%d %H:%M"),format = "%H"))
 
dat <- ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))
 
# Extract data from 1980 onwards
 
dat1 <- subset(dat, year >= 1980 )
 
# Summarize data for weather variables by hour and month
 
dat2 <- ddply(dat1,.(monthf,hour),summarize, wspd = mean(wspd,na.rm=T))
 
dat3 <- ddply(dat1,.(monthf,hour),summarize, temp = mean(tempc,na.rm=T))
 
dat4 <- ddply(dat1,.(monthf,hour),summarize, hi = mean(hi,na.rm=T))
 
dat6 <- ddply(dat1,.(monthf,hour),summarize, rh = mean(rh,na.rm=T))
 
## Plot Temperature circular Plot
 
p1 = ggplot(dat3, aes(x=monthf, y=hour, fill=temp)) +
  geom_tile(colour="grey70") +
  scale_fill_gradientn(colours = c("#99CCFF","#81BEF7","#FFFFBD","#FFAE63","#FF6600","#DF0101"),name="Temperature\n(Degree C)\n")+
  scale_y_continuous(breaks = seq(0,23),
                     labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am","7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                              "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm","8:00pm","9:00pm","10:00pm","11:00pm")) +
  coord_polar(theta="x") +
  ylab("HOUR OF DAY")+
  xlab("")+
  ggtitle("Temperature")+
  theme(panel.background=element_blank(),
  axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
  axis.title.x=element_text(size=7,colour="grey20"),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text.y=element_text(size=5,colour="grey20"),
  axis.text.x=element_text(size=10,colour="grey20",face="bold"),
  plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
  plot.margin = unit(c(-0.25,0.1,-1,0.25), "in"),
  legend.key.width=unit(c(0.2,0.2),"in"))
 
p1
 
 
## Plot Wind Speed circular plot
 
p2 = ggplot(dat2, aes(x=monthf, y=hour, fill=wspd)) +
  geom_tile(colour="grey70") +
  scale_fill_gradientn(colours = rev(topo.colors(7)),name="Wind Speed\n(meter/sec)\n")+
  scale_y_continuous(breaks = seq(0,23),
                     labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am","7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                              "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm","8:00pm","9:00pm","10:00pm","11:00pm")) +
  coord_polar(theta="x") +
  ylab("HOUR OF DAY")+
  xlab("")+
  ggtitle("Wind Speed")+
  theme(panel.background=element_blank(),
  axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
  axis.title.x=element_text(size=7,colour="grey20"),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text.y=element_text(size=5,colour="grey20"),
  axis.text.x=element_text(size=10,colour="grey20",face="bold"),
  plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
  plot.margin = unit(c(-0.25,0.25,-1,0.25), "in"),
  legend.key.width=unit(c(0.2,0.2),"in"))
 
p2
 
 
# Plot Heat Index circular plor
 
p3 = ggplot(dat4, aes(x=monthf, y=hour, fill=hi)) +
  geom_tile(colour="grey70") +
  scale_fill_gradientn(colours = c("#99CCFF","#81BEF7","#FFFFBD","#FFAE63","#FF6600","#DF0101"),name="Temperature\n(Degree C)\n")+
  scale_y_continuous(breaks = seq(0,23),
                     labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am","7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                              "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm","8:00pm","9:00pm","10:00pm","11:00pm")) +
  coord_polar(theta="x") +
  ylab("HOUR OF DAY")+
  xlab("")+
  ggtitle("Heat Index")+
  theme(panel.background=element_blank(),
  axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
  axis.title.x=element_text(size=7,colour="grey20"),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text.y=element_text(size=5,colour="grey20"),
  axis.text.x=element_text(size=10,colour="grey20",face="bold"),
  plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
  plot.margin = unit(c(-0.5,0.25,-0.25,0.25), "in"),
  legend.key.width=unit(c(0.2,0.2),"in"))
 
p3
 
 
# Plot Relative Humidity circular plor
 
col<-brewer.pal(11,"Spectral")
 
p4 = ggplot(dat6, aes(x=monthf, y=hour, fill=rh)) +
  geom_tile(colour="grey70") +
  scale_fill_gradientn(colours=col,name="Relative Humidity\n(%)\n")+
  scale_y_continuous(breaks = seq(0,23),
                     labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am","7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                              "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm","8:00pm","9:00pm","10:00pm","11:00pm")) +
  coord_polar(theta="x") +
  ylab("HOUR OF DAY")+
  xlab("")+
  ggtitle("Relative Humidity")+
  theme(panel.background=element_blank(),
  axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
  axis.title.x=element_text(size=7,colour="grey20"),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text.y=element_text(size=5,colour="grey20"),
  axis.text.x=element_text(size=10,colour="grey20",face="bold"),
  plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
  plot.margin = unit(c(-0.5,0.1,-0.25,0.25), "in"),
  legend.key.width=unit(c(0.2,0.2),"in"))
 
p4
 
## Plot and export to png file
 
png(file="Weather_Variables_Circular_Plot.png",width = 12, height = 10, units = "in",
    bg = "white", res = 400, family = "", restoreConsole = TRUE,
    type = "cairo")
 
grid.arrange(p1,p2,p4,p3,nrow=2,main=textGrob("\nWeather Variables by Month and Hour\n(Bayan Lepas Weather Station)",
gp=gpar(fontsize=18,col="grey20",fontface="bold")),sub=textGrob("Source: NOAA National Climatic Data Centre (NCDC)\n",
                                                                                                              gp=gpar(fontsize=9,col="grey20",fontface="italic")))
 
dev.off()