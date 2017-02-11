### Box and Whisker Plot of Temperatures in Penang by Month

boxplot.title = 'Temperatures in Penang (1934-2013)'
boxplot.subtitle = 'Data source : GCHN v3 temperature data version ghcnm.tavg.v3.2.2.20140107'


b <- ggplot(dataset,aes(m,temp)) + geom_boxplot(outlier.shape = NA,fill="wheat1",range=0.5,col="wheat4")+ geom_smooth(aes(group = 1), method = "loess",span=0.75)
b <- b + ggtitle(bquote(atop(.(boxplot.title), atop(italic(.(boxplot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black"))
b <- b + ylab("Temperature (Degrees C)")+ xlab("Month") + stat_boxplot(geom ='errorbar',col="wheat3")
b <- b + theme_bw()
b

ggsave(b, file="D:/ClimData/GHCN_Penang_Temp_Monthly.png", width=10, height=5)

### Box and Whisker Plot of Precipitation in Penang by Month

boxplot.title = 'Precipitation in Penang (1934-2013)'
boxplot.subtitle = 'Data source : GCHN v3 precipitation data version ghcnm.tavg.v3.2.2.20140107'


c <- ggplot(dataset,aes(m,prcp)) + geom_boxplot(outlier.shape = NA,fill="paleturquoise1",range=0.5,col="turquoise4")+ geom_smooth(aes(group = 1), method = "loess",span=0.75,colour="red")
c <- c + ggtitle(bquote(atop(.(boxplot.title), atop(italic(.(boxplot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black"))
c <- c + ylab("Precipitation (mms)")+ xlab("Month") + stat_boxplot(geom ='errorbar',col="turquoise")
c <- c + theme_bw()
c

ggsave(c, file="D:/ClimData/GHCN_Penang_Prcp_Monthly.png", width=10, height=5)


### Temperature Matrix Plot in Penang by Month

png(filename = "GHCN_Penang_Temp_Matrix_Plot.png",height=5,width=15,
    bg = "white",units='in', res = 300, family = "", restoreConsole = TRUE,
    type = "windows")


mat <- tapply(dataset$temp,list(dataset$y,mon),mean,na.rm=T)

image(main='Penang / Bayan Weather Station - Monthly Precipitation',
      1934:2013,1:12,mat1,xlab="Year",ylab="Month",col=topo.colors(500))
contour(1934:2013,1:12,mat1,add=T,nlevels=5)

matr <- raster(mat)

matr3 <- focal(matr, w=matrix(1,5,5), pad=TRUE,fun=mean, na.rm=TRUE, NAonly=FALSE)

matr3 <- as.matrix(matr3)

image(1934:2013,1:12,as.matrix(matr3))

image(main='Monthly Temperatures at Penang / Bayan Weather Station',
      1934:2013,1:12,xaxt="n",yaxt="n",as.matrix(mat),xlab="Year",ylab="Month",col=rev(rainbow(100)))
contour(1934:2013,1:12,as.matrix(matr3),add=T,nlevels=10, method = "edge")

axis(2, at=1:12, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),las=1)
axis(1, at = seq(1930,2015, by = 5), las=1)

box()

dev.off()

