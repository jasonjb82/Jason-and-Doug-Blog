# Load required libraries
 
library(weatherData)
library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)
library(MASS)
library(scales)
library(extrafont)
library(reshape2)
library(plyr)
library(grid)
library(chron)
 
# Load fonts
 
loadfonts()
 
# Get weather data from Weather Underground using weatherData library
 
we <- getWeatherForDate("WMKK", "2001-01-01","2014-12-31", opt_detailed=T, opt_custom_columns=T, custom_columns=c(11))
 
# Convert to character
 
we$Events <- as.character(we$Events) 
 
# Assign values of '1' to Thunderstorm Event and '0' to Non-Thunderstorm Event
 
we$Events[we$Events == "" ] <- NA
we$Events[we$Events == "Rain"] <- 0
we$Events[we$Events == "Rain-Thunderstorm"] <- 1
we$Events[we$Events == "Thunderstorm"] <- 1
we$Events[is.na(we$Events)] <- 0
 
# Convert to numeric
 
we$Events<-as.numeric(we$Events)
 
# Calculate number of events
 
No_Events <- tapply(diff(c(0, we$Events)) == 1, as.Date((we$Time),tz="Asia/Kuala_Lumpur"), sum)
 
we.df <- data.frame(Date = as.Date(names(No_Events)), No_Event = unname(No_Events))
 
# Rename columns
 
colnames(we.df) <- c("date","no_event")
 
# Add date columns
 
we.df$month <- as.numeric(as.POSIXlt(we.df$date)$mon+1)
we.df$year <- strftime(we.df$date, "%Y")
we.df$week <- as.Date(cut(we.df$date,breaks = "week",start.on.monday = TRUE))
we.df$monthf <- factor(we.df$month,levels=as.character(1:12),
                labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
we.df$day <- strftime(we.df$date,'%A')
we.df$weekend = chron::is.weekend(we.df$date)
 
# Aggregate no events by month
 
month.agg <- aggregate(no_event ~ month + year, we.df, FUN = sum)
month.agg$date <- as.POSIXct(paste(month.agg$year, month.agg$month, "01", sep = "-"))
month.agg$monthf <- factor(month.agg$month,levels=as.character(01:12),
                    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
# Plot number of events by month
 
ts <- ggplot(month.agg,aes(x=date,y=no_event,fill=no_event)) +
      geom_bar(stat="identity") +
      stat_smooth(method = "glm.nb",formula = y ~ x, data = month.agg, se = TRUE,colour="blue",size=0.5)+
      scale_fill_continuous(low="lightgoldenrod",high="red")+
      scale_x_datetime(labels = date_format("%Y"),breaks = "1 year")+
      ggtitle("Number of Monthly Thunderstorm Events from 2001 - 2014\n(Observations at KLIA, Selangor)\n")+
      theme_bw()+ ylab("No of events\n") + xlab("")+
      theme(axis.text.x  = element_text(size=11,family="Clear Sans",face="bold"), legend.position="none",
      axis.text.y  = element_text(size=11,family="Clear Sans",face="bold"), legend.position="none",
      panel.background=element_blank(),
      axis.title.y=element_text(size=12,colour="grey20",face="bold",family="Clear Sans"),
      panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
      plot.title = element_text(lineheight=1, face="bold",size = 15, colour = "grey20",family="Clear Sans"))
 
ts
 
ggsave(ts,file="D:/Thunderstorm_Events_KLIA_2000-2014.png",dpi=300,w=12,h=6,type="cairo-png")
 
month.mean <- ddply(month.agg,.(monthf),summarize,mean_events=mean(no_event,na.rm=T))
 
boxplot.title = 'Number of Thunderstorm Events by Month (2001 - 2014)'
boxplot.subtitle = 'Data source : Weather Underground (2015)'
 
# Box plot number of thunderstorm events by month
 
b <- ggplot(month.agg,aes(monthf,no_event,col=year)) +
     geom_boxplot(outlier.shape = NA,fill="ivory2",range=0.5,col="black")+
     geom_point(size=4,alpha=1/2,width=0.25,height=0.25,shape=18)+
     ggtitle("Number of Thunderstorm Events by Month at KLIA, Selangor (2001 - 2014) \n Source: Weather Underground (2015)\n")+
     ylab("No of events\n")+ xlab("") + stat_boxplot(geom ='errorbar',col="black")+
     scale_y_continuous(breaks=c(0,20,40,60),expand=c(0.1,0))+
     theme_bw()+
     geom_text(aes(label=year),hjust=-0.25, vjust=0,size=3,face="bold",family="Segoe UI Semibold")+
     theme(axis.text.x  = element_text(size=11,family="Clear Sans",face="bold"), legend.position="none",
     axis.text.y  = element_text(size=11,family="Clear Sans",face="bold"), legend.position="none",
     panel.background=element_blank(),
     axis.title.y=element_text(size=12,colour="grey20",face="bold",family="Clear Sans"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     plot.title = element_text(lineheight=1, face="bold",size = 15, colour = "grey20",family="Clear Sans"))
 
b
 
ggsave(b,file="D:/Thunderstorm_Events_byMonth_KLIA_2000-2014.png",dpi=300,w=12,h=6,type="cairo-png")
 
 
# Reformat weather event data from Weather Underground to get count of hourly thunderstorm events
 
we$hour <- as.numeric(format(strptime(we$Time, format = "%Y-%m-%d %H:%M"),format = "%H"))
we$min <- as.numeric(format(strptime(we$Time, format = "%Y-%m-%d %H:%M"),format = "%M"))
 
# Subset only events on the hour
 
we2<- subset(we, min == 0)
 
we3 <- ddply(we2,.(hour),summarize, event = sum(Events,na.rm=T))
 
# Plot number of thunderstorm events by hour of day
 
hr <- ggplot(we3,aes(x=hour,y=event))+
      geom_bar(breaks=seq(0,24),width = 1,colour="black",stat="identity",fill="blue",drop=TRUE)+ 
      coord_polar(start=0)+
      theme_minimal()+
      ylab("Count\n")+
      xlab("Source: Weather Underground (2015)")+
      scale_x_continuous(limits=c(0,24),breaks=seq(0,24),labels=seq(0,24))+
      ggtitle("Thunderstorm Event Frequency by Hour\nobserved at KLIA, Selangor\n")+
      guides(colour = guide_legend(show = FALSE)) +
      theme(panel.background=element_blank(),
      axis.title.y=element_text(size=12,hjust=0.65,colour="grey20",family="Clear Sans"),
      axis.title.x=element_text(size=10,colour="grey20",family="Clear Sans"),
      axis.ticks=element_blank(),
      axis.text.y=element_text(size=10,colour="grey20",family="Clear Sans"),
      axis.text.x=element_text(size=10,colour="grey20",face="bold",family="Clear Sans"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
 
hr
 
ggsave(hr,file="D:/Thunderstorm_Events_byHourofDay_KLIA_2000-2014.png",dpi=300,w=8,h=8,type="cairo-png")
