# Load required libraries
 
library(ggplot2)
library(scales)
library(grid)
library(plyr)
library(lubridate)
library(zoo)
 
# Set working directory
 
setwd("D:/ClimData/SeaLevel")
 
# Read csv file
 
sl<-read.csv("rqd0138a.csv",header=FALSE)
 
# Rename columns
 
colnames(sl)<-c("year","month","day","sl_mm")
 
# Format date columns
 
sl$date <- as.Date(paste(sl$year,sl$month,sl$day),format="%Y%m%d")
sl$month <- as.numeric(format(sl$date,"%m"))
sl$year <- as.numeric(format(sl$date,"%Y"))
sl$monthf <- factor(sl$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
sl$mday <- strptime(sl$date, "%Y-%m-%d")$mday
sl$jday <- strptime(sl$date, "%Y-%m-%d")$yday+1
sl$daymth <- as.character(paste(sl$month,sl$day,sep="-"))
sl$daymth <-as.Date(sl$daymth,format="%m-%d")
 
# Classify data into seasons
 
sl$season <- "Season"
 
sl$season[sl$month == 1 & sl$mday >= 1 |  sl$month == 2 & sl$mday <= 13| sl$month == 1]<-'Winter'
sl$season[sl$month == 2 & sl$mday >= 14 |  sl$month == 4 & sl$mday <= 14 | sl$month == 3]<-'Spring'
sl$season[sl$month == 4 & sl$mday >= 15 |  sl$month == 6 & sl$mday <= 14 | sl$month == 5]<-'Summer'
sl$season[sl$month == 6 & sl$mday >= 15 |  sl$month == 8 & sl$mday <= 17 | sl$month == 7]<-'Monsoon'
sl$season[sl$month == 8 & sl$mday >= 18 |  sl$month == 10 & sl$mday <= 18| sl$month == 9]<-'Autumn'
sl$season[sl$month == 10 & sl$mday >= 19 |  sl$month == 12 & sl$mday <= 16| sl$month == 11]<-'Late Autumn'
sl$season[sl$month == 12 & sl$mday >= 17 |  sl$month == 12 & sl$mday <= 31| sl$month == 1]<-'Winter'
 
sl$season = factor(sl$season, c("Winter", "Spring", "Summer", "Monsoon","Autumn","Late Autumn"))
 
## Plot Sea Level
 
hp_sl <-   ggplot(sl, aes(date, sl_mm,colour=season))+
           #geom_line(size=0.5)+
           geom_point(shape=5,size=1)+
           geom_smooth(method="lm",size=0.5,col="red")+
           scale_x_date(name="\n\n\n Source: University of Hawaii Sea Level Centre / Bangladesh Inland Water Transport Authority (BIWTA) - 2014",labels=date_format("%Y"),breaks = date_breaks("2 years"))+
           ylab("Milimetres (mm)\n")+
           xlab("\nYear")+
           theme_bw()+
           ggtitle("Sea Level at Charchanga - Bangladesh (1980-2000)\n")+
           theme(plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
           panel.border = element_rect(colour = "black",fill=F,size=1),
           panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
           panel.grid.minor = element_blank(),
           axis.title.y=element_text(size=11,colour="grey20"),
           axis.title.x=element_text(size=9,colour="grey20"),
           panel.background = element_rect(fill = NA,colour = "black"))
 
hp_sl
 
# Get gradient and add to plot
 
m <- lm(sl_mm~year, data=sl )
ms <- summary(m)
 
slope <- coef(m)[2]
lg <- list(slope = format(slope, digits=3))
eq <- substitute(italic(Gradient)==slope,lg)
eqstr <-as.character(paste(as.expression(eq),"/year"))
hp_sl <- hp_sl + annotate(geom="text",as.Date(-Inf, origin = '1970-01-01'), y = Inf, 
         hjust = -0.1, vjust = 2, label = eqstr,parse = TRUE,size=3)
 
hp_sl
 
# Save plot to png
 
ggsave(hp_sl, file="Charchanga_SeaLevel_Plot_Seasons.png", width=10, height=6,dpi=400,unit="in",type="cairo")
 
# Code to produce html code of embedded sea level stations map using googleVis
 
# Load libraries
 
library(RCurl)
library(XML)
library(leafletR)
library(googleVis)
 
# Convert html table into data frame
 
theurl <- "http://uhslc.soest.hawaii.edu/data/download/rq"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
 
tbl <- tables[[which.max(n.rows)]]
 
bgd.tbl <- subset(tbl, Country =="Bangladesh")
 
bgd.tbl$Latitude <- as.numeric(levels(bgd.tbl$Latitude)[bgd.tbl$Latitude])
bgd.tbl$Longitude <- as.numeric(levels(bgd.tbl$Longitude)[bgd.tbl$Longitude])
 
google.location <- paste(bgd.tbl$Latitude, bgd.tbl$Longitude, sep = ":")
stations.google <- data.frame(bgd.tbl, google.location)
 
# Plot map
 
map <- gvisMap(data = stations.google, locationvar = "google.location",tipvar = "Location",
       options=list(showTip=TRUE, enableScrollWheel=TRUE,mapType='terrain', useMapTypeControl=TRUE,width=100,height=400,
       icons=paste0("{","'default': {'normal': 'http://i.imgur.com/f3q6Oaj.gif',\n",
       "'selected': 'http://i.imgur.com/f3q6Oaj.gif'","}}")))
 
plot(map)