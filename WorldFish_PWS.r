# Load required libraries
 
library(weatherData)
library(ggplot2)
library(mgcv)
library(scales)
library(plyr)
library(reshape2)
library(gridExtra)
library(lubridate)
library(weathermetrics)
 
station.id="IPENANGB2"
date="2014-08-26"
 
# Get detailed weather station data
 
wd<-getDetailedWeather(station.id,date,station_type = "id", opt_all_columns=T)
 
str(wd)
 
# Rename columns
 
colnames(wd)<-c("time","time1","tempc","dewpc","slp","wd","wdd","wspd","guspd","hum","prcp","conditions",
                 "cc","rain","software","date_utc","unk")
 
wd$time<-as.POSIXct(wd$time1,format="%Y-%m-%d %H:%M:%S")
 
wd$rain[wd$rain < 0 ] <- NA
 
# Plot temperatures
 
dt_p <- ggplot(wd, aes(time, tempc)) + 
        xlab("Time") + ylab(as.expression(expression( paste("Temperature (", degree,"C)","\n")))) + 
        geom_line(colour="red",size=0.5) +
        geom_point(colour="red",size=1)+
        theme_bw() +
        ggtitle(paste('Plot of weather variables for',station.id, 'on',strftime(date,"%d-%b-%Y"),'\n\n',"Temperature\n")) +
        scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
        theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
        panel.border = element_rect(colour = "black",fill=F,size=1),
        panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "ivory",colour = "black"))
 
dt_p
 
# Plot windspeed
 
wd$wspd[wd$wspd=="Calm"] <- 0
 
wd$wspd<-as.numeric(wd$wspd)
 
winds <- subset(melt(wd[,c("time","wspd")], id = "time"), value > 0)
 
dws_p <- ggplot(winds, aes(time, value))+
         geom_bar(stat="identity",col="limegreen",width=.5)+
         geom_point(data=wd,aes(time,guspd),col="seagreen4",size=1.5,shape=1)+        
         scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
         ylab("km/hour\n")+
         xlab("Time")+
         scale_y_continuous(limits=c(0,20))+
         theme_bw()+
         theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
         panel.border = element_rect(colour = "black",fill=F,size=1),
         panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "ivory",colour = "black"))+        
         ggtitle("Wind and gust speed\n")
 
dws_p
 
# Plot wind vectors
 
wd$u <- (-1 * wd$wspd) * sin((wd$wdd) * pi / 180.0)
wd$v <- (-1 * wd$wspd) * cos((wd$wdd) * pi / 180.0)
dw = subset(wd, u != 0 & v != 0)
 
v_breaks = pretty_breaks(n = 5)(min(dw$v):max(dw$v))
v_labels = abs(v_breaks)
 
dwd_p <-  ggplot(data = dw, aes(x = time, y = 0)) +
          theme_bw() +
          theme(plot.margin = unit(c(0, 1, 0.5, 0.5), 'lines')) +
          geom_segment(aes(xend = time + u*360, yend = v), arrow = arrow(length = unit(0.15, "cm")), size = 0.5) + 
          geom_point(data = subset(dw, !is.na(u)), alpha = 0.5,size=1) +
          scale_x_datetime(name="Time",labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours")) +
          ggtitle("Wind vectors\n")+
          scale_y_continuous(name = "Wind vectors (km/h)\n", labels = v_labels, breaks = v_breaks)+
          theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
          panel.border = element_rect(colour = "black",fill=F,size=1),
          panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "ivory",colour = "black"))
 
dwd_p
 
# Plot Sea Level Pressure
 
wd$slp <- as.numeric(as.character(wd$slp))
 
dslp_p <- ggplot(wd, aes(time, slp)) + 
          geom_point(col="purple4",size=1) + 
          geom_step(col="purple",size=0.5)+
          scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
          ggtitle("Sea Level Pressure\n")+
          ylab("hPa\n\n")+
          xlab("Time")+
          theme_bw()+ 
          theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
          panel.border = element_rect(colour = "black",fill=F,size=1),
          panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "ivory",colour = "black"))
 
dslp_p
 
 
# Plot Relative Humidity
 
dh_p <-  ggplot(wd, aes(time, hum)) + 
         geom_point(colour="royalblue4",size=1) +
         geom_step(col="royalblue",size=0.5)+
         xlab("Time") + ylab("Relative Humidity (%)\n") + 
         ggtitle("Relative humidity\n")+
         theme_bw() +
         scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
         theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
         panel.border = element_rect(colour = "black",fill=F,size=1),
         panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "ivory",colour = "black"))
 
dh_p
 
# Plot Precipitation
 
dr_p <- ggplot(wd, aes(time, prcp)) + 
        xlab("Time") + ylab("Rainfall (mm)") + 
        geom_bar(stat="identity",colour="darkcyan",size=0.5,fill="darkcyan") +
        #geom_point(colour="darkcyan",size=2)+
        theme_bw() +
        ggtitle("Precipitation\n")+
        scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
        theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
        panel.border = element_rect(colour = "black",fill=F,size=1),
        panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "ivory",colour = "black"))
 
dr_p
 
# Plot Cumulative Precipitation
 
dcr_p <- ggplot(wd, aes(time, rain)) + 
         xlab("Time") + ylab("Rainfall (mm)") + 
         geom_line(colour="darkcyan",size=0.5) +
         geom_point(colour="darkcyan",size=1)+
         theme_bw() +
         ggtitle("Cumulative Precipitation\n")+
         scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
         theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
         panel.border = element_rect(colour = "black",fill=F,size=1),
         panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "ivory",colour = "black"))
 
dcr_p
 
 
# Calculate Heat Index using weathermetrics
 
wd$hi <- heat.index(t = wd$tempc,rh = wd$hum,temperature.metric = "celsius",output.metric = "celsius",round = 2)
 
# Plot Heat Index
 
dhi_p <- ggplot(wd, aes(time, hi)) + 
         xlab("Time") + ylab(as.expression(expression(paste("Temperature (", degree,"C)",)))) + 
         geom_line(colour="red",size=0.5) +
         geom_point(colour="red",size=1)+
         theme_bw() +
         ggtitle("Heat Index\n") +
         scale_x_datetime(labels = date_format("%I:%M:%p"),breaks = date_breaks("4 hours"))+
         scale_y_continuous(breaks=c(25,30,35,40,45))+
         theme(plot.title = element_text(lineheight=1.2, face="bold",size = 16, colour = "grey20"),
         panel.border = element_rect(colour = "black",fill=F,size=1),
         panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "ivory",colour = "black"))
 
dhi_p
 
# Export weather variables plot to png file
 
plot.name1 <- paste(station.id,"_WU_",date,".png",sep="")
 
png(plot.name1,width = 12, height = 25, units = "in",
     bg = "white", res = 500, family = "", restoreConsole = TRUE,
     type = "cairo")
 
grid.draw(rbind(ggplotGrob(dt_p),ggplotGrob(dh_p),ggplotGrob(dr_p),ggplotGrob(dcr_p),ggplotGrob(dslp_p),ggplotGrob(dws_p),ggplotGrob(dwd_p),ggplotGrob(dhi_p),size="first"))
 
dev.off()