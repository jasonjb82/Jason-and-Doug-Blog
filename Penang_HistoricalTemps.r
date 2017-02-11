# Load required libraries
 
library(zoo)
library(lubridate)
library(plyr)
library(ggplot2)
library(grid)
library(XLConnect)
library(reshape2)
library(extrafont)
 
# Set working directory
 
setwd("D:/ClimData")
 
# Read table of 1885 Temperatures
 
temp1885<-readWorksheet(loadWorkbook("1885_Temps.xlsx"),sheet=1)
 
temps1885 <- temp1885[c(1,2,10,11,12,13,14,15)]
 
colnames(temps1885) <- c("month","monthf","temp_9am","temp_3pm","temp_9pm","mon_mean","mon_max","mon_min")
 
temps1885$year <- "1885"
 
temps1885_df <- temps1885[c(1,2,9,3:8)]
 
 
# Read data from NCDC
 
dat<-read.table("2110827004508dat.txt",header=TRUE,fill=TRUE,na.strings=c("*","**","***","****","*****","******","0.00T*****"))
 
colnames(dat)<-tolower(colnames(dat))
 
# Convert hourly data from UTC to local time zone
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 8 * 60 * 60
 
# Convert temperatures in Degree Fahrenheit to Degree Celcius
 
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$tempc[dat$tempc<=10] <- NA
dat$tempc[dat$tempc>=40] <- NA 
 
# Extract times and dates and reformat data
 
dat$year <- as.numeric(as.POSIXlt(dat$dates)$year+1900)
dat$month <- as.numeric(as.POSIXlt(dat$dates)$mon+1)
dat$monthf <- factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
dat$jday <- yday(dat$dates)
dat$hour <- as.numeric(format(strptime(dat$dates, format = "%Y-%m-%d %H:%M"),format = "%H"))
dat$minute <- as.numeric(format(strptime(dat$dates, format = "%Y-%m-%d %H:%M"),format = "%M"))
 
dat_01 <- subset(dat, year >= 2003 & year <= 2013 )
 
dat_02 <- dat_01[c(34,35,36,37,38,39,40,41)]
 
dat_9am <- subset(dat_02,hour == 9 & minute == 0 )
dat_3pm <- subset(dat_02,hour == 15 & minute == 0 )
dat_9pm <- subset(dat_02,hour == 21 & minute == 0 )
 
dat_9am_melt<- ddply(dat_9am, .(month,monthf), summarize, temp_9am=mean(tempc,na.rm=T))
dat_3pm_melt<- ddply(dat_3pm, .(month,monthf), summarize, temp_3pm=mean(tempc,na.rm=T))
dat_9pm_melt<- ddply(dat_9pm, .(month,monthf), summarize, temp_9pm=mean(tempc,na.rm=T))
 
dat_hourly <- cbind(dat_9am_melt,dat_3pm_melt[3],dat_9pm_melt[3])
 
dat_melt<- ddply(dat_01, .(month,monthf), summarize, mon_mean=mean(tempc,na.rm=T),mon_max=max(tempc,na.rm=T),mon_min=min(tempc,na.rm=T))
 
dat_yrs <- cbind(dat_melt,dat_hourly[-c(1,2)]) 
 
dat_yrs$year <- "10 Year Average (2003-2013)"
 
tempcomb <- rbind(dat_yrs,temps1885_df)
 
tempcomb_df <- melt(tempcomb, id.vars=c("month","monthf","year"),variable.name="type",value.name="temps")
 
# Plot data
 
p <- ggplot(data=tempcomb_df, aes(x=monthf,temps, colour=year))+
     geom_point(data=subset(tempcomb_df,year!=1885),size=3, alpha=0.5,aes(group=year))+ 
     geom_line(data=subset(tempcomb_df,year!=1885),size=1, alpha=0.5,aes(group=year))+
     geom_point(data=subset(tempcomb_df,year==1885),size=3, alpha=0.5,aes(group=year))+ 
     geom_line(data=subset(tempcomb_df,year==1885),size=1, alpha=0.5,aes(group=year))+
     geom_text(aes(label=round(temps,1)),hjust=1, vjust=-1,size=2.5)+
     facet_wrap(~type)+
     xlab("")+ ylab ("Temperature (Degree C)\n") +
     labs(title="Comparison of year 1885 and 10 year monthly average (2003-2013) temperatures in Penang\n")+
     theme(panel.background = element_rect(fill= "transparent"),
     plot.title = element_text(lineheight=1.2, face="bold",size = 13, colour = "grey20",family="Clear Sans"),
     panel.border = element_rect(colour = "grey90",fill=F,size=0.5),
     panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
     axis.line=element_blank(),axis.title.x=element_blank(),
     axis.text.x=element_text(face="bold",size = 10, colour = "grey20",family="Clear Sans"),
     axis.title.y=element_text(face="bold",size = 10, colour = "grey20",family="Clear Sans"),
     axis.text.y=element_text(face="bold",size = 8, colour = "grey20",family="Clear Sans"),
     legend.title = element_text(colour="black", size=12, face="bold",family="Clear Sans"),
     legend.text = element_text(colour="black", size=10, family="Clear Sans"),
     legend.position="bottom",
     axis.ticks=element_line(size=0.5,colour="grey20"),panel.grid.major.y = element_line(colour="grey90",linetype="longdash",size=1))+
     guides(color = guide_legend(title = "Year", title.position = "top"))
 
p
 
 
# Save plot to png
 
ggsave(p,file="PenangTemps_1885_10yrAverage.png",dpi=500,w=12,h=8,unit="in",type="cairo-png")