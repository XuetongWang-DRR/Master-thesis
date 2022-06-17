library(plyr)
library(dplyr)
library(gstat)
library(raster)
library(ggplot2)
library(car)
library(classInt)
library(RStoolbox)
library(spatstat)
library(dismo)
library(fields)
library(gridExtra)
library(Hmisc)
library(kriging)
library(scales)
library(lattice)



station = read.table("D:/ITC/thesis_Turkey/stationID.txt", sep = "\t", header = T)
rainfall = read.table("D:/ITC/thesis_Turkey/rainfallFr2015.txt", sep = "\t", header = F)
downscale = read.table("D:/ITC/thesis_Turkey/downscaled.txt", sep = "\t", header = F)
colnames(downscale) = c("Lon","Lat","Elevation")
rainfall = as.data.frame(t(rainfall))
rainfall$V1 = substr(rainfall$V1,1,5)
rainfall$V1 = as.numeric(rainfall$V1)
station$Station.ID = as.numeric(station$Station.ID)

for (i in 2:5){
name = paste0("test",i,".txt")
  
rainfall = rainfall[-1,c(1,i)]
colnames(rainfall) = c("V1","V2")
rainfall$V2 = as.numeric(rainfall$V2)
rainfall[which(rainfall$V2 == -999),2] <-NA
rainfall = na.omit(rainfall)
#rainfall$V2 = rainfall$V2 +1
rainfall$V2 = log(rainfall$V2 +2)
station$Elevation = log(station$Elevation)
downscale$Elevation = log(downscale$Elevation)


colnames(station)[1] <- c("ID")
colnames(rainfall) <- c("ID","rainfall")
combind = left_join(rainfall,station,by = c("ID"))
combind = combind[,-3]

#powerTransform(combind$rainfall)
#rainfall.bc <- bcPower(combind$rainfall, 0.4110187)
#combind$rainfall.bc <- bcPower(combind$rainfall,0.4110187)
#cor.matrix <- rcorr(as.matrix(combind))
coordinates(combind) = ~Lon+Lat
coordinates(downscale) = ~Lon+Lat
#v.rainfall <- variogram(rainfall.bc~1,data = combind, cloud = F)
v.rainfall <- variogram(rainfall~1,data = combind, cloud = F)
#rainfall.fit = fit.variogram(v.rainfall, vgm(c("Exp", "Mat", "Sph")))
#rainfall.fit = fit.variogram(v.rainfall, vgm(max(v.rainfall$gamma)*0.9,"Sph",max(v.rainfall$dist)/2,mean(v.rainfall$gamma)/4))

rainfall.fit = fit.variogram(v.rainfall, vgm(1, "Pow", 1))
p1 <- plot(v.rainfall,pl=F,model=rainfall.fit,main = "rainfall")

v.Elevation <- variogram(Elevation~1, data = combind, cloud = F)
#Elevation.fit = fit.variogram(v.Elevation, vgm(c("Exp", "Mat", "Sph")))

Elevation.fit = fit.variogram(v.Elevation, vgm(max(v.Elevation$gamma)*0.9,"Sph",max(v.Elevation$dist)/2,mean(v.Elevation$gamma)/4))
p2 <- plot(v.Elevation,pl=F, model = Elevation.fit, main = "Elevation")
grid.arrange(p1,p2,ncol = 2)

#Cross-Variogram
#g <- gstat(NULL,id = "rainfall",form = rainfall.bc~1, data = combind)
g <- gstat(NULL,id = "rainfall",form = rainfall~1, data = combind)

g <- gstat(g,id = "Elevation", form = Elevation~1, data = combind)

v.cross <- variogram(g)
plot(v.cross,pl = F)

g <- gstat(g, id = "rainfall", model = rainfall.fit, fill.all = T)
g <- fit.lmc(v.cross,g)
plot(variogram(g),model = g$model)

#Co-kriging prediction at grid locations
CK = predict(g,downscale)
summary(CK)
#k1 <- 1/ 0.4110187  
#CK$CK.pred <-((CK$rainfall.pred * 0.4110187  +1)^k1)
#CK$CK.var <-((CK$rainfall.var * 0.4110187  +1)^k1)

CK$CK.pred <-CK$rainfall.pred
CK$CK.var <- CK$rainfall.var
summary(CK)

CK.pred <- rasterFromXYZ(as.data.frame(CK)[,c("Lon","Lat","CK.pred")])
CK.var <- rasterFromXYZ(as.data.frame(CK)[,c("Lon","Lat","CK.var")])

CK.pred = as.data.frame(rasterToPoints(CK.pred))
CK.pred$CK.pred = exp(CK.pred$CK.pred)-2


setwd("D:/ITC/thesis_Turkey/kriging")
write.table(CK.pred,name, sep = "\t", col.names = F, row.names = F)

}





p3<-ggR(CK.pred, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("CK Predicted rainfall")+
  theme(plot.title = element_text(hjust = 0.5))
p4<-ggR(CK.var, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("blue",  "green","yellow", "orange"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("CK Predition Variance")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p3,p4, ncol = 2)  # Multiplot 

