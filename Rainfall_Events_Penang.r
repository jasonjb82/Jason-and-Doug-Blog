# Load required libraries
 
library(weatherData)
library(zoo)
library(lubridate)
library(plyr)
library(ggplot2)
library(circular)
library(grid)
library(RColorBrewer)
 
# Download hourly weather data from Weather Underground 
 
we <- getWeatherForDate("WMKP", "2002-01-01","2013-12-31", opt_detailed=T, opt_custom_columns=T, custom_columns=c(11))
 
# Convert to characters
 
we$Events<-as.character(we$Events)
 
# Assign values of '1' to Rain Event and 'NA' to Non-Rain Event
 
we$Events[we$Events == "" ] <- NA
 
we$Events[we$Events == "Rain"] <- 1
 
we$Events[we$Events == "Rain-Thunderstorm"] <- 1
 
we$Events[we$Events == "Thunderstorm"] <- NA
 
# Convert to numeric
 
we$Events<-as.numeric(we$Events)
 
# Create date and time columns
 
we$Dates<-as.POSIXct(we$Time)
 
we$year <- as.numeric(as.POSIXlt(we$Dates)$year+1900)
we$month <- as.numeric(as.POSIXlt(we$Dates)$mon+1)
we$monthf <- factor(we$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
we$weekday <- as.POSIXlt(we$Dates)$wday
we$weekdayf <- factor(we$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
we$week <- as.numeric(format(as.Date(we$Dates),"%W"))
we1$hour <- as.numeric(format(strptime(we1$Dates, format = "%Y-%m-%d %H:%M"),format = "%H"))
we1$min <- as.numeric(format(strptime(we1$Dates, format = "%Y-%m-%d %H:%M"),format = "%M"))
 
## Use only data on the hour
 
we1<- subset(we, min == 0)
 
we2 <- ddply(we1,.(monthf,hour),summarize, event = sum(Events,na.rm=T))
 
# Define colour palette
 
col<-brewer.pal(9,"Blues")
 
# Plot circular chart of rain event frequency
 
r1 <-  ggplot(we2, aes(x=monthf, y=hour, fill=event)) +
       geom_tile(colour="grey70") +
       scale_fill_gradientn(colours=col,guide=FALSE)+
       scale_y_continuous(breaks = seq(0,23),
       labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am",
       "7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm",
       "8:00pm","9:00pm","10:00pm","11:00pm")) +
       coord_polar(theta="x") +
       ylab("HOUR OF DAY")+
       xlab("Source: Weather Underground (2014)")+
       ggtitle("Rain Event Frequency by Month and Hour\n(Bayan Weather Station)\n")+
       guides(colour = guide_legend(show = FALSE)) +
       theme(panel.background=element_blank(),
       axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
       axis.title.x=element_text(size=7,colour="grey20"),
       panel.grid=element_blank(),
       axis.ticks=element_blank(),
       axis.text.y=element_text(size=5,colour="grey20"),
       axis.text.x=element_text(size=10,colour="grey20",face="bold"),
       plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
       plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
 
r1
 
# Save to png file
 
ggsave(r1,file="Rain_Event_Frequency_Plot.png", width=6, height=6, dpi=400,type="cairo-png")