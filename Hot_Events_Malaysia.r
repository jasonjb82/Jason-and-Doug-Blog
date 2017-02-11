# Load required packages
 
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(grid)
library(zoo)
library(gridExtra)
 
# Setting work directory
 
setwd("d:\\ClimData")
 
# Reading and reformatting raw daily data downloaded from NCDC
 
dat<-read.table("CDO2812586929956.txt",header=F,skip=1)
 
colnames(dat)<-c("stn","wban","yearmoda","temp","tempc","dewp","dewpc","slp","slpc","stp","stpc","visib","visibc","wdsp","wdspc","mxspd","gust","maxtemp","mintemp","prcp","sndp","frshtt")
 
dat$yearmoda <- strptime(dat$yearmoda,format="%Y%m%d")
 
# Create date columns
 
dat$dates <- as.Date(dat$yearmoda)
 
dat$year <- as.numeric(format(dat$yearmoda,"%Y"))
dat$month <- as.numeric(format(dat$yearmoda,"%m"))
dat$day <- as.numeric(format(dat$yearmoda,"%d"))
dat$monthf <- factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
 
# Remove erronous values & convert units
 
dat$maxtemp[dat$maxtemp == 9999.9 ] <- NA
dat$mintemp[dat$mintemp == 9999.90 ] <- NA
 
dat$tempdc <- (dat$temp-32) * (5/9)
 
dat$maxtemp <- gsub("[*]","", dat$maxtemp) 
dat$mintemp <- gsub("[*]","", dat$mintemp) 
 
dat$maxtemp <- as.numeric(dat$maxtemp)
dat$mintemp <- as.numeric(dat$mintemp)
 
dat$maxtempc <- (dat$maxtemp-32) * (5/9)
dat$mintempc <- (dat$mintemp-32) * (5/9)
 
dat$maxtempc[is.na(dat$maxtempc)] <- as.numeric(dat$tempdc[is.na(dat$maxtempc)])
 
dat$mintempc[is.na(dat$mintempc)] <- as.numeric(dat$tempdc[is.na(dat$mintempc)])
 
str(dat)
 
dat<- dat[-c(3)]
 
# Subset data from 1975 onwards
 
dat <- subset(dat, year > 1975)
 
## Create a data frame table
 
dat = tbl_df(dat)
 
hd = dat %>% 
  group_by(year) %>%
  summarize(N35 = sum(maxtempc >= 35, na.rm = TRUE),
            N34 = sum(maxtempc >= 35, na.rm = TRUE),
            avgTmaxC = mean(maxtempc, na.rm = TRUE))
 
c1 <- ggplot(hd, aes(x = year, y = N35, fill = N35)) + 
      theme_bw() + 
      geom_bar(stat='identity') + 
      scale_fill_continuous(low='orange', high='red') +
      geom_text(aes(label = N35), vjust = 1.5, size = 3) +
      scale_x_continuous(breaks = seq(1975, 2015, 5)) +
      ylab("Days\n") +
      xlab("") +
      ggtitle("Number of days at or above 35°C\n")+
      theme(axis.text.x  = element_text(size=11), legend.position="none",
      panel.background=element_blank(),
      axis.title.x=element_text(size=7,colour="grey20"),
      panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1, face="bold",size = 12, colour = "grey20"))
 
c1
 
# Determine hot events
 
hotEvents = rle(dat$maxtempc >= 35)
eventLength = hotEvents$lengths[hotEvents$values]
eventNo = rep(1:length(eventLength), eventLength)
Events.df = subset(dat, maxtempc >= 35)
Events.df$eventNo = eventNo
 
# Determine the number of days between successive 35C days. Add this as another column.
 
t1 = Events.df$dates[-length(Events.df$dates)]
t2 = Events.df$dates[-1]
dd = difftime(t2, t1, units = "days")
Events.df$dbe = c(NA, dd)
Events.df = Events.df %>% select(maxtempc, mintempc, dates, year, month, eventNo, dbe)
 
# Length and intensity of events
 
LI.df = Events.df %>%
  group_by(eventNo) %>%
  summarize(eventLength = length(maxtempc),
            avgEventT = mean(maxtempc),
            maxEventT = max(maxtempc),
            whenMaxEvT = which.max(maxtempc),
            Year = year[1])
 
cor(LI.df$eventLength, LI.df$maxEventT)
 
ggplot(LI.df, aes(x = eventLength, y = whenMaxEvT)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Event Length (days)") +
  ylab("Day of Event When Maximum Occurs") +
  scale_x_continuous(breaks = 1:10) +
  theme_bw()
 
# Average length of hot events
 
LI.df2 = LI.df %>%
  group_by(Year) %>%
  summarize(count = length(Year),
            avgEL = mean(eventLength))
 
AllYears = data.frame(Year = 1975:2014)
LI.df3 = merge(AllYears, LI.df2, by = "Year", all.x = TRUE)
LI.df3$count[is.na(LI.df3$count)] = 0
 
# Number of hot events

suppressMessages(library(MASS))
 
ne <- ggplot(LI.df3, aes(x = Year, y = count)) +
      geom_bar(stat = "identity") +
      ylab("Count\n") +
      scale_x_continuous(breaks = seq(1975, 2015, 5)) +
      ggtitle("Number of Hot Events in Penang since 1975\n")+
      stat_smooth(method = "glm.nb",formula = y ~ x, data = LI.df3, se = TRUE) + 
      theme_bw()+
      theme(legend.position = "none",
      axis.text.x  = element_text(size=11), legend.position="none",
      panel.background=element_blank(),
      axis.title.x=element_text(size=12,colour="grey20"),
      panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
      axis.text.x=element_text(size=10,colour="grey20",face="bold"),
      plot.title = element_text(lineheight=1, face="bold",size = 12, colour = "grey20"))
 
ne
 
var(LI.df3$count)/mean(LI.df3$count)
 
summary(glm.nb(count ~ Year, data = LI.df3))
 
## Export file to png
 
png(file="Hot_Events_Penang_1975-2014.png",width = 12, height = 10, units = "in",
    bg = "white", res = 500, family = "", restoreConsole = TRUE,type = "cairo")
 
grid.arrange(c1,el,ne,ncol=1,main=textGrob("\nHot Events in Penang (Bayan Weather Station) from 1975 - 2014",gp=gpar(fontsize=18,col="grey20",fontface="bold")),sub=textGrob("Source: NOAA National Climatic Data Centre (NCDC)\n",gp=gpar(fontsize=9,col="grey20",fontface="italic")))
dev.off()