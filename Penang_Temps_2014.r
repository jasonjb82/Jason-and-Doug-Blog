# Code to produce Temperature Plot
 
# Load required libraries
 
library(dplyr)
library(tidyr)
library(ggplot2)
 
# Load font
 
windowsFonts(GraphBlack="TT Graph Black")
 
# Set working directory
 
setwd("D:/ClimData/")
 
# Read weather data downloaded from NOAA NCDC GSOD
 
dat<-read.table("CDO1553156579351.txt",header=F,skip=1)
 
# Rename columns
 
colnames(dat)<-c("stn","wban","yearmoda","temp","tempc","dewp","dewpc","slp","slpc","stp","stpc","visib","visibc","wdsp","wdspc","mxspd","gust","maxtemp","mintemp","prcp","sndp","frshtt")
 
# Reformat columns
 
dat$yearmoda <- strptime(dat$yearmoda,format="%Y%m%d")
dat$tempdc <- (dat$temp-32) * (5/9)
 
dat$year <- as.numeric(format(dat$yearmoda,"%Y"))
dat$month <- as.numeric(format(dat$yearmoda,"%m"))
dat$day <- as.numeric(format(dat$yearmoda,"%d"))
 
temp <- dat[c(23,24,25,26)]
 
names(temp) <- c("temp", "year", "month", "day")
 
temp <- temp %>% group_by(year) %>% mutate(daynum = seq_along(year))
 
# Set up plain chart with min-max range and 95% CI
 
(p <- ggplot(temp, aes(x = daynum, y = temp)) + 
      stat_summary(geom = "linerange", 
      fun.ymin = min, 
      fun.ymax = max, 
      color = "wheat2") +
      stat_summary(geom = "linerange", 
      fun.ymin = function(x) mean(x) - 2 * sd(x)/sqrt(length(x)),
      fun.ymax = function(x) mean(x) + 2 * sd(x)/sqrt(length(x)), 
      color = "wheat4") + 
      geom_line(data = filter(temp, year == 2014)))
 
# Data frame containing all days in 2014 with extreme weather
 
df_maxmin <- temp %>%
             group_by(daynum) %>%
             mutate(max_temp = max(temp), 
             min_temp = min(temp)) %>%
             filter(year == 2014, (temp %in% c(max_temp, min_temp))) %>%
             mutate(max_min = temp == max_temp) # Dummy variable to be mapped to color
 
head(df_maxmin)
 
# Data frame with x-axis breaks and labels
 
df_xaxis <- temp %>% filter(year == 2014, month != lead(month)) %>%     # Last day of month
            mutate(days_in_month = daynum - lag(daynum),                # Days in month
            midpoint = lag(daynum) + days_in_month / 2)                 # Month midpoints
 
df_xaxis$midpoint[1] <- 31 / 2
 
head(df_xaxis)
 
 
(p <- p  +
      geom_vline(xintercept = 0, color = "wheat4", size = 1) +
      geom_hline(yintercept = seq(22, 32, 2), color = "white") +
      geom_vline(xintercept = df_xaxis$daynum, 
      color = "wheat4", linetype = "dotted", size = 0.5) +
      geom_point(data = df_maxmin, aes(color = max_min), show_guide = FALSE))
 
 
(p <- p +
      scale_x_continuous(expand = c(0,0), labels = month.name,
      breaks = c(df_xaxis$midpoint, df_xaxis$daynum[11] + (31/2))) +
      scale_y_continuous(expand = c(0,0), breaks = seq(22, 32, 2),
      labels = function(x) parse(text = paste0(x, "*degree"))) +
      scale_color_manual(values = c("blue3", "firebrick3")))
 
 
(p <- p + theme(axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(color = "gray30"),
      plot.title = element_text(face = "bold", hjust = 0.012, 
      vjust = 0.8, color = "#3C3C3C", size = 25,family="Graph Black")) +
      labs(x = NULL, y = NULL, title = "Penang's Weather in 2014"))
 
 
 
desc <- "Data represents average daily temperatures. Temperature data used starts from 
January 1, 1975. Average temperature for the year was 28.4° making 2014
the 2nd warmest year since 1975" %>% 
 
strwrap(width = 0.75 * getOption("width")) %>% 
paste0(collapse = "\n")
 
# Data frame with annotations
 
df_annotate <- data_frame(
               daynum = c(17, 287), temp = c(24.5, 30.5), max_min = c(FALSE, TRUE), 
               label = c("We had 4 days that were the\ncoldest since 1975", 
               "We had 54 days that were\nthe hottest since 1975"))
 
(p <- p + 
      annotate("text", x = 5, y = 31.7, size = 4, fontface = "bold", 
      hjust = 0, vjust = 0, label = "Temperature",family="Clear Sans") +
      annotate("text", x = 5, y = 31.6, size = 3, color = "gray30", 
      hjust = 0, vjust = 1, label = desc,fontface = "bold",family="Clear Sans") +
      geom_segment(data = df_annotate,aes(x = c(15, 285), xend = c(10, 282), 
      y = c(24.5, 30.5), yend = c(25.5, 29.8),
      color = c(FALSE, TRUE)), show_guide = FALSE) + 
      geom_text(data = df_annotate, aes(color = max_min, label = label), 
      size = 3, hjust = 0, ,show_guide = FALSE,family="Clear Sans",fontface="bold"))
 
# Data frame with legend label coordinates
 
df_leg_text <- data_frame(daynum = c(186, 145, 184, 184), 
               temp = c(23.5, 23.5, 24,23), 
               label = c("NORMAL RANGE", "2014 TEMPERATURE", 
               "RECORD HIGH", "RECORD LOW"))
 
# Data frame with legend shape coordinates
 
df_leg_seg <- data_frame(daynum = c(181, 181, 183, 183, 185), 
              xend = c(181, 181, 185, 185, 185),
              temp = c(23, 23.25, 23.75, 23.25, 23.25),
              yend = c(24, 23.75, 23.75, 23.25, 23.75), 
              size = c(3, 3, 0.5, 0.5, 0.5), 
              color = c("wheat2", rep("wheat4", 4)))
 
p1 <- p + 
      geom_segment(data = df_leg_seg, aes(xend = xend, yend = yend), 
      size = df_leg_seg$size, color = df_leg_seg$color) +
      geom_line(data = data_frame(daynum = seq(175, 182), temp = rnorm(8,23.5,0.15))) +
      geom_text(data = df_leg_text, aes(label = label), hjust = 0, size = 2,fontface = "bold",family="Clear Sans")
 
p1
 
# Save plot to png
 
ggsave(p1,file="PenangTemps_1975-2014.png",dpi=500,w=12,h=6,unit="in",type="cairo-png")