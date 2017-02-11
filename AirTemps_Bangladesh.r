# Load required libraries
 
library(ggplot2)
library(scales)
library(grid)
library(plyr)
library(lubridate)
library(zoo)
 
# Set working directory
 
setwd("D:/ClimData/BGD")
 
################################
## Plot Air Temps for Rangpur ##
################################
 
dat <- read.table("826316390028dat.txt",header=TRUE,fill=TRUE,
       na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
colnames(dat)<-tolower(colnames(dat))
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 6 * 60 * 60
 
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$date <- as.Date(dat$dates,format="%Y%m%d")
 
dat$year <- as.numeric(format(dat$dates,"%Y"))
 
dat$month <- as.numeric(format(dat$dates,"%m"))
 
dat$hour  <- as.numeric(format(dat$dates,"%H"))
 
dat$week  <- as.numeric(format(dat$dates,"%W"))
 
dat$monthf <- factor(dat$month,levels=as.character(1:12),
              labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
# Chuck out data that is < 0 and > 45
 
dat <- dat[dat$tempc > 0 & dat$tempc < 50, ]
 
datb<-subset(dat, year >= 1982 & year <= 2013)
 
plot.title = 'Daily Air Temperatures in Rangpur (1982-2013)'
plot.subtitle = 'Data source: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)'
 
b <- ggplot(data=datb, aes(x=dates, y=tempc)) +
     geom_jitter(colour="grey40",size=1.5,alpha=0.4) +
     scale_y_continuous(breaks=c(10,15,20,25,30,35,40)) +
     theme_bw()+
     scale_x_datetime(breaks = "5 year", minor_breaks = "5 year", labels=date_format("%Y")) +
     xlab("") + ylab(as.expression(expression( paste("Temperature (", degree,"C)") ))) +
     ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
     theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
     geom_smooth(aes(group = 1), method = "lm",size=0.75,colour="red")
 
b
 
# Get linear equation value and gradient term
 
m <- lm(tempc~year, data=datb )
ms <- summary(m)
 
slope <- coef(m)[2]
 
lg <- list(slope = format(slope, digits=3))
 
eqg <- substitute(italic(Gradient)==slope,lg)
 
eqgstr <-as.character(as.expression(eqg))
 
# Add gradient term value to plot
 
b <- b +annotate(geom="text",as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, 
                      hjust = -0.1, vjust = 2, label = eqgstr,parse = TRUE,size=3.5)
 
b
 
col<-c("blue","green","yellow","orange","red")
 
wt <- ggplot(data=datb,aes(x=year,y=week,fill=tempc))+ geom_tile(colour=NA,size=0.65)+
      theme_bw()+
      scale_fill_gradientn(colours=col,name=as.expression(expression( paste("Temperature (", degree,"C)"))))+
      coord_equal(ratio=0.2)+
      ylab("WEEK OF YEAR\n")+
      xlab("\nYEAR\n\nSource: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)")+
      scale_x_continuous(expand = c(0,0),breaks = seq(1982, 2015, 2)) +
      scale_y_discrete(expand = c(0,0),breaks = seq(0,52,4))+
      ggtitle("Average weekly air temperatures in Rangpur\n")+
      theme(panel.background=element_rect(fill="grey90"),
      panel.border=element_rect(colour="white"),
      axis.title.y=element_text(size=10,colour="grey20"),
      axis.title.x=element_text(size=10,colour="grey20"),
      axis.text.y=element_text(size=10,colour="grey20",face="bold"),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.key.width=unit(c(0.1,0.1),"in"))
 
wt
 
# Exporting file to png
 
ggsave(b, file="Rangpur_AT_Plot.png", width=10, height=6,dpi=400,unit="in",type="cairo")
 
ggsave(wt, file="Rangpur_AT_HeatMap_Plot.png", width=15, height=6,dpi=400,unit="in",type="cairo")
 
 
##############################
## Plot Air Temps for Dhaka ##
##############################
 
dat <-read.table("820756390032dat.txt",header=TRUE,fill=TRUE,
      na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
colnames(dat)<-tolower(colnames(dat))
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 6 * 60 * 60
 
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$year <- as.numeric(format(dat$dates,"%Y"))
 
dat$month <- as.numeric(format(dat$dates,"%m"))
 
dat$hour  <- as.numeric(format(dat$dates,"%H"))
 
dat$week  <- as.numeric(format(dat$dates,"%W"))
 
dat$monthf <- factor(dat$month,levels=as.character(1:12),
              labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
# Chuck out data that is < 0 and > 45
 
dat <- dat[dat$tempc > 0 & dat$tempc < 50, ]
 
datb <- subset(dat, year >= 1998 & year <= 2013)
 
plot.title = 'Daily Air Temperatures in Dhaka (1998-2013)'
plot.subtitle = 'Data source: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)'
 
b <- ggplot(data=datb, aes(x=dates, y=tempc)) +
     geom_jitter(colour="grey40",size=1.5,alpha=0.4) +
     scale_y_continuous(breaks=c(10,15,20,25,30,35,40)) +
     theme_bw()+
     scale_x_datetime(breaks = "1 year", minor_breaks = "1 year", labels=date_format("%Y"))  +
     xlab("") + ylab(as.expression(expression( paste("Temperature (", degree,"C)") ))) +
     ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
     theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
     geom_smooth(aes(group = 1), method = "lm",size=0.75,colour="red")
 
b
 
# Get linear equation value and gradient term
 
m <- lm(tempc~year, data=datb )
ms <- summary(m)
 
slope <- coef(m)[2]
 
lg <- list(slope = format(slope, digits=3))
 
eqg <- substitute(italic(Gradient)==slope,lg)
 
eqgstr <-as.character(as.expression(eqg))
 
# Add gradient term value to plot
 
b <- b +annotate(geom="text",as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, 
                 hjust = -0.1, vjust = 2, label = eqgstr,parse = TRUE,size=3.5)
 
b
 
col<-c("blue","green","yellow","orange","red")
 
wt <- ggplot(data=datb,aes(x=year,y=week,fill=tempc))+ geom_tile(colour=NA,size=0.65)+
      theme_bw()+
      scale_fill_gradientn(colours=col,name=as.expression(expression( paste("Temperature (", degree,"C)"))))+
      coord_equal(ratio=0.15)+
      ylab("WEEK OF YEAR\n")+
      xlab("\nYEAR\n\nSource: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)")+
      scale_x_continuous(expand = c(0,0),breaks = seq(1998, 2015,1)) +
      scale_y_discrete(expand = c(0,0),breaks = seq(0,52,4))+
      ggtitle("Average weekly air temperatures in Dhaka\n")+
      theme(panel.background=element_rect(fill="grey90"),
      panel.border=element_rect(colour="white"),
      axis.title.y=element_text(size=10,colour="grey20"),
      axis.title.x=element_text(size=10,colour="grey20"),
      axis.text.y=element_text(size=10,colour="grey20",face="bold"),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.key.width=unit(c(0.1,0.1),"in"))
 
wt
 
# Exporting file to png
 
ggsave(b, file="Dhaka_AT_Plot.png", width=10, height=6,dpi=400,unit="in",type="cairo")
 
ggsave(wt, file="Dhaka_AT_HeatMap_Plot.png", width=10, height=5,dpi=400,unit="in",type="cairo")
 
 
################################
## Plot Air Temps for Barisal ##
################################
 
dat <- read.table("8998396389910dat.txt",header=TRUE,fill=TRUE,
       na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
colnames(dat)<-tolower(colnames(dat))
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 6 * 60 * 60
 
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$year <- as.numeric(format(dat$dates,"%Y"))
 
dat$month <- as.numeric(format(dat$dates,"%m"))
 
dat$hour  <- as.numeric(format(dat$dates,"%H"))
 
dat$week  <- as.numeric(format(dat$dates,"%W"))
 
dat$monthf <- factor(dat$month,levels=as.character(1:12),
              labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
# Chuck out data that is < 0 and > 45
 
dat <- dat[dat$tempc > 0 & dat$tempc < 50, ]
 
datb<-subset(dat, year >= 1998 & year <= 2013)
 
plot.title = 'Daily Air Temperatures in Barisal (1998-2013)'
plot.subtitle = 'Data source: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)'
 
b <- ggplot(data=datb, aes(x=dates, y=tempc)) +
     geom_jitter(colour="grey40",size=1.5,alpha=0.4) +
     scale_y_continuous(breaks=c(10,15,20,25,30,35,40)) +
     theme_bw()+
     scale_x_datetime(breaks = "1 year", minor_breaks = "1 year", labels=date_format("%Y"))  +
     xlab("") + ylab(as.expression(expression( paste("Temperature (", degree,"C)") ))) +
     ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
     theme(plot.title = element_text(face = "bold",size = 16,colour="black"))+
     geom_smooth(aes(group = 1), method = "lm",size=0.75,colour="red")
 
b
 
# Get linear equation value and gradient term
 
m <- lm(tempc~year, data=datb )
ms <- summary(m)
 
slope <- coef(m)[2]
 
lg <- list(slope = format(slope, digits=3))
 
eqg <- substitute(italic(Gradient)==slope,lg)
 
eqgstr <-as.character(paste(as.expression(eqg),as.expression(paste("/year"))))
 
# Add gradient term value to plot
 
b <- b +annotate(geom="text",as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, 
                 hjust = -0.1, vjust = 2, label = eqgstr,parse = TRUE,size=3.5)
 
b
 
col<-c("blue","green","yellow","orange","red")
 
wt <- ggplot(data=datb,aes(x=year,y=week,fill=tempc))+ geom_tile(colour=NA,size=0.65)+
      theme_bw()+
      scale_fill_gradientn(colours=col,name=as.expression(expression( paste("Temperature (", degree,"C)"))))+
      coord_equal(ratio=0.15)+
      ylab("WEEK OF YEAR\n")+
      xlab("\nYEAR\n\nSource: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)")+
      scale_x_continuous(expand = c(0,0),breaks = seq(1998, 2015,1)) +
      scale_y_discrete(expand = c(0,0),breaks = seq(0,52,4))+
      ggtitle("Average weekly air temperatures in Barisal\n")+
      theme(panel.background=element_rect(fill="grey90"),
      panel.border=element_rect(colour="white"),
      axis.title.y=element_text(size=10,colour="grey20"),
      axis.title.x=element_text(size=10,colour="grey20"),
      axis.text.y=element_text(size=10,colour="grey20",face="bold"),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.key.width=unit(c(0.1,0.1),"in"))
 
wt
 
# Exporting file to png
 
ggsave(b, file="Barisal_AT_Plot.png", width=10, height=6,dpi=400,unit="in",type="cairo")
 
ggsave(wt, file="Barisal_AT_HeatMap_Plot.png", width=10, height=6,dpi=400,unit="in",type="cairo")
 
 
############################################
## Plot Air Temps for All Stations (2013) ##
############################################
 
# Read raw data from files
 
datr <- read.table("826316390028dat.txt",header=TRUE,fill=TRUE,
        na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
datd <- read.table("820756390032dat.txt",header=TRUE,fill=TRUE,
        na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
datb <- read.table("8998396389910dat.txt",header=TRUE,fill=TRUE,
        na.strings=c("*","**","***","****","*****","******","0.00T*****","0.00T**","0.00T"))
 
# Bind data from 3 stations
 
bgt<-rbind(datr,datb,datd)
 
colnames(bgt)<-tolower(colnames(bgt))
 
bgt$usaf <- gsub("418590", "Rangpur", bgt$usaf)
bgt$usaf <- gsub("419220", "Dhaka", bgt$usaf)
bgt$usaf <- gsub("419500", "Barisal", bgt$usaf)
 
Sys.setenv(TZ = "UTC")
bgt$dates <- as.POSIXct(strptime(bgt$yr..modahrmn,format="%Y%m%d%H%M"))  + 6 * 60 * 60
 
bgt$tempc <- (bgt$temp-32) * (5/9)
 
bgt$year <- as.numeric(format(bgt$dates,"%Y"))
 
bgt$month <- as.numeric(format(bgt$dates,"%m"))
 
bgt$hour  <- as.numeric(format(bgt$dates,"%H"))
 
bgt$week  <- as.numeric(format(bgt$dates,"%W"))
 
bgt$monthf <- factor(bgt$month,levels=as.character(1:12),
              labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
bgt1 <-subset(bgt,year == 2013 )
 
plot.title = 'Mean Air Temperatures for 2013 (by Month)'
plot.subtitle = 'Source: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)'
 
bt <-  ggplot(bgt1, aes(x = monthf, y = tempc,colour=factor(usaf),group=usaf))+
       geom_smooth(method="loess",se=F,size=1.2,span=1.5)+theme_bw()+
       xlab("\nMonth") + ylab(as.expression(expression( paste("Temperature (", degree,"C)"))))+
       ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
       theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
       guides(color = guide_legend(title = "Locality", title.position = "top")) +
       theme(panel.border = element_rect(colour = "black",fill=F,size=1),
       panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
       panel.grid.minor = element_blank(),
       panel.background = element_rect(fill = "ivory",colour = "black"),
       legend.position="right")
bt
 
# Exporting file to png
 
ggsave(bt, file="MonthlyAirTemps_BGD.png", width=10, height=6,dpi=400,unit="in",type="cairo")