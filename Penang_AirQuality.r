# Load required libraries
 
library(ggplot2)
library(reshape)
library(lubridate)
library(zoo)
library(openair)
library(scales)
library(grid)
library(extrafont)
library(weatherData)
 
loadfonts()
 
# Set working directory
 
setwd("D:/Projects/AP/ReadingsByRegion/")
 
# Read data from csv file 
 
df <- read.table('USM.csv',header=F,sep=":")
 
# Rename columns
 
colnames(df) <- c("station_id","station_name","dates","hour","api","unk","unk1")
 
# Remove last 2 columns - not relevant information
 
df <- df[-c(6,7)]
 
# Add minute column
 
df$minute <- as.character("00")
 
df$time <- as.character(paste(df$hour,df$minute,sep = ":" ))
 
df$date <- as.POSIXct(paste(df$dates,df$time, format = "%Y-%m-%d %H:%M" ))
 
# Convert 0 to NA
 
df$api[df$api == 0] <- NA
 
# Date and time columns
 
df$year<-as.numeric(as.POSIXlt(df$date)$year+1900)
df$month<-as.numeric(as.POSIXlt(df$date)$mon+1)
df$monthf<-factor(df$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
df$weekday = as.numeric(format(as.POSIXlt(df$date),"%u"))
df$yearmonth<-as.yearmon(df$dates)
df$yearmonthf<-factor(df$yearmonth)
df$week <- as.numeric(format(as.POSIXlt(df$date),"%W"))
df$day = strftime(df$date,'%A')
df$jday <- strptime(df$date, "%Y-%m-%d")$yday+1
df$hour <- as.numeric(format(strptime(df$date, format = "%Y-%m-%d %H:%M"),format = "%H"))
 
 
##### Creating roseplot of pollutant magnitude over 24 hours #####
 
# Subset data for 2014
 
df2014 <- subset(df,year==2014)
 
# Set Color Table
 
colortable = c("#99FFFF", "#00FFFF", "#00FF00", "#CCFF33", "#FFFF00",
               "#FFCC00", "#FF6600", "#FF3333", "#FF33CC", "#660033")
 
# Cut data into ten parts
 
STN = "USM"
Time_of_Day = df2014$hour[df2014$station_name==STN]
mag = cut_number(round(df2014$api,100)[df2014$station_name==STN],n = 10)
rosedata = data.frame(dir=Time_of_Day,mag=mag)
 
# Plot rose chart
 
rc <- ggplot(rosedata,aes(x=Time_of_Day,fill=mag))+  geom_bar(binwidth = 1, drop = TRUE) +
      coord_polar() + xlab("") + ylab("") + 
      scale_x_continuous(breaks = seq(0,23),
      labels=c("12.00am","1:00am","2:00am","3:00am","4:00am","5:00am","6:00am",
      "7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm","1:00pm",
      "2:00pm","3:00pm","4:00pm","5:00pm","6:00pm","7:00pm","8:00pm","9:00pm",
      "10:00pm","11:00pm")) +
      ggtitle("\nDaily API readings in 2014") +  scale_fill_manual(values=colortable,name="API Reading")+
      theme(panel.background=element_blank(),
      axis.title.y=element_text(size=10,hjust=0.75,colour="grey20"),
      axis.title.x=element_text(size=7,colour="grey20"),
      legend.title=element_text(size=13,colour="grey20",face="bold",family="Myriad Pro"),
      legend.text=element_text(size=11,colour="grey20",face="bold",family="Myriad Pro"),
      panel.grid=element_blank(),
      axis.ticks=element_blank(),
      axis.text.y=element_text(size=10,colour="grey20",family="Clear Sans"),
      axis.text.x=element_text(size=10,colour="grey20",face="bold",family="Clear Sans"),
      plot.title = element_text(lineheight=1, face="bold",size = 20, colour = "grey20",family="Graphik-Black"),
      plot.margin = unit(c(-0.5,0.25,-0.25,0.25), "in"),
      legend.key.width=unit(c(0.2,0.2),"in"))
 
rc
 
# Save plot to png
 
ggsave(file="D:/rc.png",dpi=300,w=10,h=8,unit="in",type="cairo-png")
 
 
### Reading weather data from Weather Underground using weatherData library ####
 
# Get weather data for 2014
 
we <- getWeatherForDate("WMKP", "2014-01-01","2015-12-31", opt_detailed=T, opt_custom_columns=T, custom_columns=c(1,6,7,8,9,13,14))
 
# Rename columns
 
colnames(we) <- c("date","date_myt","vis_km","wda","ws","gspd_kmh","wd","date_utc")
 
# Create date and time columns
 
we$date<-as.POSIXct(we$date)
we$hour <- as.numeric(format(strptime(we$date, format = "%Y-%m-%d %H:%M"),format = "%H"))
we$min <- as.numeric(format(strptime(we$date, format = "%Y-%m-%d %H:%M"),format = "%M"))
 
# Only use data on the hour
 
we_df<- subset(we, min == 0)
 
# Remove duplicate data
 
we_df$date[duplicated(we_df$date)]
 
# Merge API and weather data
 
we_api <- merge(we_df, df2014, by=c("date","date")) 
 
# Reformat yearmonth column for use in openair library
 
we_api$yearmonth <- as.factor(we_api$yearmonth)
 
# Reformat wind speed column
 
we_api$ws[we_api$ws == "Calm"] <- 0
we_api$ws <- as.numeric(we_api$ws)
 
# Plot 'calendar plot' and save as png
 
png(filename = "D:/Projects/AP/USM_API_2014_CalendarPlot.png",height=8,width=10,
    bg = "white",units='in', res = 400, family = "",  type = "cairo-png")
 
calendarPlot(we_api, pollutant = "api",year="2014",
             main = "Daily average API readings at USM\n
             with wind-speed scaled wind direction overlay (2014)\n",
             key.header = "API\n(Air Pollutant Index)",
             cols="heat", annotate = "ws")
 
dev.off()