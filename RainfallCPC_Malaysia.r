## Load package libraries
 
library(raincpc)
library(SDMTools)
library(raster)
library(ggplot2)
library(rgdal)
library(grid)
library(maptools)
 
# Set working directory
 
setwd("D:/ClimData/")
 
## Get raw CPC rain data - data has a 2 day lag
 
cpc_get_rawdata(2014,12,17,2014,12,24) 
 
## Read raw CPC rain data into raster grids
 
rain1 <- cpc_read_rawdata(2014, 12, 17)
rain2 <- cpc_read_rawdata(2014, 12, 18)
rain3 <- cpc_read_rawdata(2014, 12, 19)
rain4 <- cpc_read_rawdata(2014, 12, 20)
rain5 <- cpc_read_rawdata(2014, 12, 21)
rain6 <- cpc_read_rawdata(2014, 12, 22)
rain7 <- cpc_read_rawdata(2014, 12, 23)
rain8 <- cpc_read_rawdata(2014, 12, 24)
 
# Combine multiple day rasters
 
rain_tot <- rain1 + rain2 + rain4 + rain5 + rain6 + rain7 + rain8
 
# Get summary of raster grid
 
print(rain_tot)
 
raster_ggplot <- function(rastx) {
 
require(SDMTools)
 
  stopifnot(class(rastx) == "RasterLayer")
 
  gfx_data <- getXYcoords(rastx)
  # lats need to be flipped
  gfx_data <- expand.grid(lons = gfx_data$x, lats = rev(gfx_data$y), 
                          stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  gfx_data$rain <- rastx@data@values
 
  return (gfx_data)
}
 
rain_gg <- raster_ggplot(rain_tot)
 
# Read shapefile of country boundaries (shapefiles can be downloaded from http://thematicmapping.org/downloads/world_borders.php)
 
bounds <- readOGR(dsn="D:/Data/World_Borders", layer="TM_WORLD_BORDERS-0.3")
 
## Extents of ggplot map
 
xmin<-95
xmax<-120
ymin<--10
ymax<-15
 
interval <-(xmax-xmin)/5
 
lon_vals <- seq(xmin, xmax, 0.5)
lat_vals <- seq(ymin, ymax, 0.5)
 
 
# Set theme options
 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_rect(fill="grey95"),
                         panel.border = element_rect(colour="black"),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         legend.key.size=unit(0.35,"in"),
                         legend.key.width=unit(0.15,"in"),
                         legend.text=element_text(size=14,family="Myriad Pro Cond"),
                         legend.title=element_text(size=16,family="Myriad Pro Cond"),
                         plot.title = element_text(size=23,face="bold",vjust = -10,hjust=0.96,family="Graph Black"),
                         legend.position = c(0.17, 0), 
                         legend.justification = c(1, 0), 
                         legend.background = element_blank(),
                         axis.title.y = element_blank()))
 
# Plot rainfall map
 
rf <-  ggplot()+
       geom_raster(data=rain_gg,aes(x=lons,y=lats,fill=rain),alpha=0.8) +
       scale_fill_gradientn(colours=c("#FFFFFF","#ADD8E6","#95BBE9","#7E9EEC","#6781F0","#5064F3","#3948F6","#222BFA","#0B0EFD","#0A02FE","#1F06FC","#350AFA","#4A0EF8","#6013F6","#7517F3"),limits=c(0,1200),na.value=NA, name="Rainfall (mm)\n")+
       geom_polygon(data=bounds, aes(long,lat, group=group),colour="grey30",fill="transparent",size=0.35)+
       coord_equal(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + theme_bw()+
       ggtitle("Extreme rainfall event over Malaysia\n(17th to 24th of December 2014)\n")+
       xlab("") + ylab("")+ theme_opts +
       annotate("text", x = 115.00, y = -9.5, label = "Data source: Climate Prediction Center - NOAA (2014)",size=5,family="Myriad Pro Cond") 
 
plot(rf)
 
# Save rainfall map to png file
 
ggsave(rf,file="D:/ClimData/CPC_Extreme_Rainfall_Event_MYS_Dec2014.png",dpi=500,w=10,h=10,unit="in",type="cairo-png")