# Setting work directory
 
setwd("d:\\ClimData")
 
list.files()
 
# Reading and reformatting raw daily data downloaded from ISD NCDC
 
dat<-read.table("2110827004508dat.txt",header=TRUE,fill=TRUE,na.strings=c("*","**","***","****","*****","******","0.00T*****"))
 
colnames(dat)<-tolower(colnames(dat))
 
# Convert hourly data from UTC to local time zone
 
Sys.setenv(TZ = "UTC")
dat$dates <- as.POSIXct(strptime(dat$yr..modahrmn,format="%Y%m%d%H%M"))  + 8 * 60 * 60
  
# Convert temperatures in Degree Fahrenheit to Degree Celcius
  
dat$tempc <- (dat$temp-32) * (5/9)
 
dat$tempc[dat$tempc<=10] <- NA
dat$tempc[dat$tempc>=40] <- NA 
 
# Extract years and month 
 
dat$year <- as.numeric(format(dat$dates,"%Y"))
 
dat$month <- as.numeric(format(dat$dates,"%m"))
  
# Load metvurst library
 
library(metvurst)
 
 
# Subset data for selected years
 
datsub <- subset(dat,year == 1979 | year == 1985 | year == 1989  | year == 1999 | year == 2001 | year == 2005 | year == 2010 | year == 2013 )
 
png(filename = "Penang_Daily_Temps_Cairo.png",height=8,width=12,
    bg = "white",units='in', res = 600, family = "", restoreConsole = TRUE,
    type = "cairo-png")
 
# Plot hourly air temperatures using the 'strip' function
 
plot.air.temp <- strip(x = datsub$tempc,
                       date = datsub$dates,
                       cond = datsub$year,
                       arrange = "long",
                       colour = colorRampPalette(rev(brewer.pal(11, "Spectral"))),
                       main = "Daily Air Temperature in Penang (Bayan Station) on Selected Years\n\nTemperature [°C]",
                       sub="Data source: Integrated Surface Database (ISD) - National Climatic Data Centre (NCDC)",font.sub=2)
 
print(plot.air.temp)
 
 
dev.off()