##### Code to read and produce animation of boat night light detection data from NGDC site #####
 
# Load required libraries
 
library(RCurl)
library(rvest)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(rgdal)
library(grid)
library(animation)
library(gridExtra)
library(extrafont)
 
# Set working directory
 
setwd("D:/Projects/BoatLights")
 
# Load fonts
 
loadfonts()
 
# Set date range for csv files to be downloaded from NGDC site
 
date.range <- seq.Date(from=as.Date('2015-02-01'),as.Date('2015-02-28'),by='1 day')
 
# Get dates into relevant format
 
date <- as.character(date.range)
date1 <- gsub("-","",date)
 
 
# Get urls to scrape boat light csv files off  NGDC site
 
urls <- sprintf((paste("http://mapserver.ngdc.noaa.gov/viirs_data/indo_boats///",c(date1),"/VNB_npp_d",c(date1),
        "_ssec_v12.csv",sep="")))
 
# Read boat light data files in csv format and bind them
 
boats <- do.call(rbind,lapply(urls,read.csv))
 
# Extract useful columns
 
boats.df <- boats[-c(18,19)]
 
# Reformat date and add other date and time columns
 
boats.df$Date <- as.POSIXct(boats.df$Date_Mscan, format="%Y-%m-%d %H:%M:%S",tz="GMT")
boats.df$Dates <- as.Date(boats.df$Date)
boats.df$Year <- as.numeric(as.POSIXlt(boats.df$Date)$year+1900)
boats.df$Month <- as.numeric(as.POSIXlt(boats.df$Date)$mon+1)
boats.df$Monthf <- factor(boats.df$Month,levels=as.character(1:12),
                 labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
boats.df$Weekday = as.numeric(format(as.POSIXlt(boats.df$Date),"%u"))
boats.df$Yearmonth <- as.yearmon(boats.df$Date)
boats.df$Yearmonthf <- factor(boats.df$Yearmonth)
boats.df$Week <- as.numeric(format(as.POSIXlt(boats.df$Date),"%W"))
boats.df$Day <- strftime(boats.df$Date,'%A')
boats.df$Jday <- strptime(boats.df$Date, "%Y-%m-%d")$yday+1
boats.df$Hour <- as.numeric(format(strptime(boats.df$Date, format = "%Y-%m-%d %H:%M"),format = "%H"))
 
# Extract only detections that are classified as 'boat' from quality flag for nightboat detection algorithm
 
boats.df1 <- subset(boats.df,QF_Detect==1)
 
# Import shapefiles
 
world <- readOGR(dsn="D:/GeoSpatial/Data/World_Boundaries", layer="TM_WORLD_BORDERS-0.3") # World boundary shp's download from www.thematicmapping.org
 
eez <- readOGR(dsn="D:/Projects/BoatLights", layer="EEZs") # EEZ boundary shp's downloaded from www.marineregions.org
 
mpa <- readOGR(dsn="D:/Projects/BoatLights", layer="MPAs") # MPA boundary shp's downloaded from CT Atlas website - www.ctlas.reefbase.org
 
# Fortify layers for ggplot mapping
 
world.df <- fortify(world)
 
eez.df <- fortify(eez)
 
mpa.df <- fortify(mpa)
 
# Set theme options for map
 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_rect(fill="black"),
                         panel.border = element_rect(colour="black"),
                         legend.text = element_text(size = 8,face="bold",family="Myriad Pro"),
                         legend.position = "right",
                         legend.key.width=unit(0.75,"line"),
                         legend.key.height=unit(0.75,"line"),
                         legend.direction="horizontal",
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         plot.title = element_text(size=20,face="bold",family="Graph Black",hjust=0,vjust=-0.5),
                         axis.title.y = element_blank()))
 
 
# Run loop to produce boat light maps for each night in February 2015
 
for (i in date)
 
{
 
 
b <- ggplot(subset(boats.df1,Dates ==i),aes(Lon_DNB,Lat_DNB))+
     geom_polygon(data=mpa.df, aes(long,lat, group=group,fill="MPA"),colour=NA,alpha=0.5)+
     geom_point(color = "gold", size = 0.5, alpha = 0.5) +
     geom_polygon(data=eez.df, aes(long,lat, group=group,colour="EEZ"),fill=NA,size=0.25,linetype="dashed")+
     geom_polygon(data=world.df, aes(long,lat, group=group),fill="black",colour="darkgray",size=0.35)+
     theme_bw()+ ylab("")+ xlab("")+
     ggtitle((paste("Boat Lights detected from VIIRS on ",i,"\n"))) +
     scale_fill_manual(name="",values=c(MPA="forestgreen"))+
     scale_colour_manual(name="",values=c(EEZ="dimgray"))+
     coord_equal(xlim=c(95,140),ylim=c(-15,9))+
     theme_opts
 
b
 
bg <- arrangeGrob(b, sub = textGrob("Source: VIIRS Boat Detection (VBD) Data (PROTOTYPE) - NGDC (2015)\n", hjust = -0.33,
      vjust=-0.3,gp = gpar(fontface = "bold", fontsize = 8, fontfamily="Tw Cen MT Condensed")))
 
bg
 
ggsave(bg,file=paste("D:/Projects/BoatLights/boatlights_",i,".png",sep=""),width=8,height=5,unit="in",type="cairo-png")
 
}
 
# Set path for Image Magick convert program 
 
path.to.convert <- paste0(shortPathName("C:/Program Files/ImageMagick-6.9.0-Q16)/"),"convert.exe")
ani.options(convert=path.to.convert)
 
 
# Convert png files to GIF animation
 
files = sprintf('boatlights_%s.png', c(date))
im.convert(files, output = 'BoatLights_Feb2015.gif')