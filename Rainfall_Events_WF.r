## Load required libraries
 
library(weatherData)
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(lubridate)
library(zoo)
 
# Get data for PWS using weatherData package
 
pws <- getWeatherForDate("IPENANGB2", "2014-09-01","2014-09-30", station_type = "id",opt_detailed=T, opt_custom_columns=T, custom_columns=c(1,2,6,7,10,13))
 
# Rename columns
 
colnames(pws)<-c("time","time1","tempc","wdd","wspd","prcp","rain")
 
## Adding date columns
 
pws$time<-as.POSIXct(pws$time1,format="%Y-%m-%d %H:%M:%S",tz="Australia/Perth")
pws$year <- as.numeric(format(pws$time,"%Y"))
pws$date <-as.Date(pws$time,format="%Y-%m-%d",tz="Australia/Perth")
pws$year <- as.numeric(as.POSIXlt(pws$date)$year+1900)
pws$month <- as.numeric(as.POSIXlt(pws$date)$mon+1)
pws$monthf <- factor(pws$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
pws$weekday <- as.POSIXlt(pws$date)$wday
pws$weekdayf <- factor(pws$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
pws$yearmonth <- as.yearmon(pws$date)
pws$yearmonthf <- factor(pws$yearmonth)
pws$week <- as.numeric(format(as.Date(pws$date),"%W"))
pws$weekf<- factor(pws$week)
pws$jday<-yday(pws$date)
pws$hour <- as.numeric(format(strptime(pws$time, format = "%Y-%m-%d %H:%M"),format = "%H"))
pws$min <- as.numeric(format(strptime(pws$time, format = "%Y-%m-%d %H:%M"),format = "%M"))
 
# Remove duplicate values
 
# Function to compute consecutive differences 
dif <- function(x) c(diff(x), NA) 
 
# Apply the function to each individual 
 
pws<-ddply(pws, 'jday', transform, actrain = dif(rain)) 
 
pws$actrain[pws$actrain<0]<- 0
 
pws$actrain[is.na(pws$actrain)] <- 0
 
# Summarize rainfall data by hour and by day
 
rd <- ddply(pws,.(weekday,weekdayf),summarize, raintot = sum(actrain,na.rm=T))
 
rh <- ddply(pws,.(hour),summarize, raintot = sum(actrain,na.rm=T))
 
# Plot rainfall by hour
 
h <-  ggplot(rh,aes(hour,raintot,fill=raintot))+geom_bar(stat="identity")+
      geom_text(aes(label=round(raintot,3)), vjust = -0.4, size = 2.5)+
      ggtitle("Total rainfall by hour for September 2014 - WorldFish Weather Station\n")+
      scale_fill_continuous(low='grey90', high='steelblue') + theme_bw()+
      xlab("\nHour of Day")+
      ylab("Total Precipitation (mm)\n")+
      theme(axis.text.x  = element_text(size=10), legend.position="none",
      panel.background=element_blank(),
      axis.title.x=element_text(size=10,colour="grey20"),
      axis.title.y=element_text(size=10,colour="grey20"),
      panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1, face="bold",size = 13, colour = "grey20"))
 
h
 
# Plot rainfall by Week Day
 
d <- ggplot(rd,aes(weekday,raintot,fill=raintot))+geom_bar(stat="identity")+
     geom_text(aes(label=round(raintot,3)), vjust = -0.4, size = 2.5)+
     ggtitle("Total rainfall by day for September 2014 - WorldFish Weather Station\n")+
     scale_fill_continuous(low='grey90', high='steelblue') + theme_bw()+
     xlab("\nDay of Week")+
     ylab("Total Precipitation (mm)\n")+
     scale_x_continuous(breaks=c(0,1,2,3,4,5,6),labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
     theme(axis.text.x  = element_text(size=10), legend.position="none",
     panel.background=element_blank(),
     axis.title.x=element_text(size=10,colour="grey20"),
     axis.title.y=element_text(size=10,colour="grey20"),
     panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
     axis.text.x=element_text(size=10,colour="grey20",face="bold"),
     plot.title = element_text(lineheight=1, face="bold",size = 13, colour = "grey20"))
 
d
 
# Save plots to png
 
ggsave(h,file="WF-PWS-RFbyHr.png",width=7,height=5,dpi=400,type="cairo")
ggsave(d,file="WF-PWS-RFbyDay.png",width=7,height=5,dpi=400,type="cairo")