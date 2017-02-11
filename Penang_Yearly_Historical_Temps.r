#### Long Term Monthly Average Temperature Graph using data from GHCNv3
#### Jason Benedict, Feb 13 2014
#####################################################################################

library(ggplot2)
library(spline)
library(scales)

#Setting work directory

setwd("d:\\ClimData")

list.files()

# Importing csv file into R

dataset = read.csv("CISL GHCN v3 50548601000 monthly.csv", header=TRUE,sep=",",skip=2,na.strings="")

head(dataset)

colnames(dataset)<-c("date","date1","temp","prcp")

# Reformatting data

temp<-as.numeric(dataset$temp)

year<-as.numeric(substr(dataset$date,1,4))
year[1]
min.year <- min(year)
max.year <- max(year)
max.year


mon<-as.numeric(substr(dataset$date,6,7))

day<-01

summary(mon)

date<-paste(year,mon,day,sep="/")

dataset$dates <- as.Date(date, "%Y/%m/%d") 

min.date <- min(dataset$dates)
max.date <- max(dataset$dates)

dataset$year <- as.numeric(format(dataset$dates,"%Y"))
dataset$month <- as.numeric(format(dataset$ndates,"%m"))

# Plotting monthly average temperatures with ggplot2 and saving to png format

plot.title = 'Monthly Average Temperatures in Penang (1934-2013)'
plot.subtitle = 'Data source : GCHN v3 temperature data version ghcnm.tavg.v3.2.2.20140107'

p<-ggplot(data=dataset, aes(x=dates, y=temp)) +
  geom_line(colour="bisque3",size=1) +
  geom_point(colour="bisque4",size=1) +
  scale_y_continuous() +
  scale_x_date(breaks = "10 year", minor_breaks = "10 year", labels=date_format("%Y"))  +
  xlab("") + ylab(as.expression(expression( paste("Temperature (", degree,"C)") ))) +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
  geom_smooth(aes(group = 1), method = "loess",span=0.75)

p

# Exporting file to png

ggsave(p, file="D:/ClimData/Penang_CISL_Data_Temperature_Plot.png", width=10, height=5)
