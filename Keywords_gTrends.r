# Load required libraries
 
library(gtrend)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
 
# Define terms to use for trend searches
 
terms <- c("Climate Change","Ocean Acidification","Sea Level Rise","Global Warming")
 
out <- gtrend_scraper("youremail@gmail.com", "yourpassword", terms) ## replace with your own google username and password
 
out %>%  trend2long() %>% plot() 
 
# Get plot of trends
 
a <- out %>%
     trend2long() %>%
     ggplot(aes(x=start, y=trend, color=term)) +
     xlab("\nYear") + ylab("Trend\n")+
     geom_line() + theme_stata()+
     facet_wrap(~term) + ggtitle("Phrase Search Trends on Google\n")+
     guides(color=FALSE)
 
a
 
# Save file to png
 
ggsave(a,file="GoogleTrend_Climate.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")
 
 
# Extract just trends of 'climate change' search for years 2010 to 2014 and plot
# to observe seasonality
 
 
dat <- out[[1]][["trend"]]
colnames(dat)[3] <- "trend"
 
dat2 <- dat[dat[["start"]] > as.Date("2010-01-01"),]
 
rects <- dat2  %>% 
mutate(year=format(as.Date(start), "%y")) %>%
group_by(year) %>%
summarize(xstart = as.Date(min(start)), xend = as.Date(max(end)))
 
c <- ggplot() +
     geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, 
     ymax = Inf, fill = factor(year)), alpha = 0.4) + theme_bw()+
     ylab ("Trend\n") + xlab("\nDate")+
     ggtitle("Search Trends of 'Climate Change' on Google (2010-2014)\n")+
     geom_line(data=dat2, aes(x=start, y=trend), size=0.75,colour="grey20",type="dashed") + 
     geom_point(data=dat2, aes(x=start, y=trend), size=1.25,colour="grey20") + 
     scale_x_date(labels = date_format("%b-%Y"), 
     breaks = date_breaks("3 months"),
     expand = c(0,0), 
     limits = c(as.Date("2010-01-01"), as.Date("2014-12-31"))) +
     stat_smooth(data=dat2,aes(x=start, y=trend), col="blue",method = "loess",span=0.1,se=T,size=1,linetype='twodash',alpha=0.5,fill="grey60")+
     scale_fill_discrete(guide = FALSE)+
     theme(axis.text.x = element_text(angle = -30, hjust =0,size=10),
     plot.title = element_text(lineheight=1.2, face="bold",size = 14, colour = "grey20"),
     panel.border = element_rect(colour = "black",fill=F,size=0.5),
     panel.grid.major = element_line(colour = "grey",size=0.25,linetype='longdash'),
     panel.grid.minor = element_blank(),
     axis.title.y=element_text(size=10,colour="grey20"),
     axis.title.x=element_text(size=10,colour="grey20"),
     axis.text.y=element_text(size=10,colour="grey20"),
     panel.background = element_rect(fill = NA,colour = "black"))
 
c
 
# Save plot to file
 
ggsave(c,file="GoogleTrend_ClimateChange2010-14.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")