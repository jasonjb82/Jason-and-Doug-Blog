# Load required libraries
 
library(RCurl)
library(maps)
library(stringr)
library(tm)
library(twitteR)
library(streamR)
library(grid)
library(ggplot2)
library(rgdal)
library(ggmap)
 
# Set working directory
 
setwd("D:/ClimData/")
 
#### Fonts on Windows ####
windowsFonts(ClearSans="TT Clear Sans")
 
# Load Credentials
 
load("D:/ClimData/Twitter/twitter authentification.Rdata")
registerTwitterOAuth(twitCred)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
 
# Search term on twitter
 
searchTerm <- "#climatechange"
searchResults <- searchTwitter(searchTerm,n=1500,since='2014-12-11', until='2014-12-12')  
tweetFrame <- twListToDF(searchResults) 
 
userInfo <- lookupUsers(tweetFrame$screenName)  
userFrame <- twListToDF(userInfo)
 
locatedUsers <- !is.na(userFrame$location)
 
# Geocode locations using 'ggpmap' library
 
locations <- geocode(userFrame$location[locatedUsers])
 
locations_robin <- project(as.matrix(locations), "+proj=robin")
 
locations_robin_df <- as.data.frame(locations_robin)
 
# Import world boundaries
 
world <- readOGR(dsn="D:/Data/ne_10m_admin_0_countries", layer="ne_10m_admin_0_countries")
 
world_robin <- spTransform(world, CRS("+proj=robin"))
 
world_robin_df <- fortify(world_robin)
 
counts <- aggregate(locations_robin_df$V1,by=list(x=locations_robin_df$V1,y=locations_robin_df$V2),length)
names(counts)[3] <- "count"
 
# Theme options for Map
 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         plot.background = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position = "bottom",
                         legend.key = element_blank(),
                         legend.title = element_text(colour="black", size=12, face="bold",family="Clear Sans"),
                         legend.text = element_text(colour="black", size=10, face="bold",family="Clear Sans"),
                         plot.title = element_text(size=15,face="bold",lineheight=0.5,family="Clear Sans")))
 
# Plot map and tweet counts 
 
tp <- ggplot(world_robin_df)+
      geom_polygon(aes(x = long, y = lat, group = group), fill = "grey20")+
      geom_path(aes(x = long, y = lat, group = group),colour = "grey40", lwd = 0.2)+
      geom_point(data= counts, aes(x=x,y=y,size=count),color="#32caf6", alpha=I(8/10))+
      scale_size_continuous(name="Number of tweets")+
      ggtitle("Twitter Map of #climatechange\n")+
      xlab("")+ ylab("")+
      coord_equal()+
      theme_bw() + 
      guides(size = guide_legend(title.position = "top",title.hjust =0.5))+
      theme_opts
 
tp
 
# Save to png
 
ggsave(tp,file="D:/Twitter_ClimateChange_Map.png",dpi=500,w=10,h=6,unit="in",type="cairo-png")