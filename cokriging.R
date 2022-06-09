library(dplyr)
library(ggplot2)
library(kriging)
library(gstat)
library(scales)
library(lattice)
#Reading the data
station = read.table("D:/ITC/thesis_Turkey/stationID.txt", sep = "\t", header = T)
rainfall = read.table("D:/ITC/thesis_Turkey/rainfallFr2015.txt", sep = "\t", header = F)
downscale = read.table("D:/ITC/thesis_Turkey/downscaled.txt", sep = "\t", header = F)
colnames(downscale) = c("Lon","Lat","Elevation")
rainfall = as.data.frame(t(rainfall))
rainfall$V1 = substr(rainfall$V1,1,5)
rainfall$V1 = as.numeric(rainfall$V1)
station$Station.ID = as.numeric(station$Station.ID)
rainfall = rainfall[-1,c(1,8)]
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
#rainfall.bc <- bcPower(combind$rainfall, -0.8150619  )
#combind$rainfall.bc <- bcPower(combind$rainfall,-0.8150619  )
#cor.matrix <- rcorr(as.matrix(combind))
coordinates(combind) = ~Lon+Lat
coordinates(downscale) = ~Lon+Lat
#v.rainfall <- variogram(rainfall.bc~1,data = combind, cloud = F)
v.rainfall <- variogram(rainfall~1,data = combind, cloud = F)
#rainfall.fit = fit.variogram(v.rainfall, vgm(c("Exp", "Mat", "Sph")))
rainfall.fit = fit.variogram(v.rainfall, vgm(max(v.rainfall$gamma)*0.9,"Sph",max(v.rainfall$dist)/2,mean(v.rainfall$gamma)/4))

#rainfall.fit = fit.variogram(v.rainfall, vgm(1, "Pow", 1))
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
#k1 <- 1/ -0.8150619 
#CK$CK.pred <-((CK$rainfall.pred * -0.8150619 +1)^k1)
#CK$CK.var <-((CK$rainfall.var * -0.8150619 +1)^k1)

CK$CK.pred <-CK$rainfall.pred
CK$CK.var <- CK$rainfall.var
summary(CK)

CK.pred <- rasterFromXYZ(as.data.frame(CK)[,c("Lon","Lat","CK.pred")])
CK.var <- rasterFromXYZ(as.data.frame(CK)[,c("Lon","Lat","CK.var")])

CK.pred = as.data.frame(rasterToPoints(CK.pred))
CK.pred$CK.pred = exp(CK.pred$CK.pred)-2


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


#colnames(rainfall) = c("date",1:297)
rainfall = as.data.frame(t(rainfall))
rainfall$V1 = substr(rainfall$V1,1,5)
rainfall$V1 = as.numeric(rainfall$V1)
station$Station.ID = as.numeric(station$Station.ID)

#write.table(rainfall, "rainfallPY.txt", sep = "\t", col.names = F, row.names = F)
#write.table(station, "stationPY.txt", sep = "\t", col.names = T, row.names = F)


rainfall = rainfall[-1,c(1,2)]
colnames(rainfall) = c("V1","V2")
rainfall$V2 = as.numeric(rainfall$V2)
rainfall[which(rainfall$V2 == -999),2] <-NA
rainfall = na.omit(rainfall)
rainfall$V2 = rainfall$V2 +1

colnames(station)[1] <- c("ID")
colnames(rainfall) <- c("ID","rainfall")
combind = left_join(rainfall,station,by = c("ID"))
combine.1 = cbind(combind,
                  ltra = log(combind$rainfall),
                  ltel = log(combind$Elevation))

combine.1 %>% as.data.frame %>% 
  ggplot(aes(Lon, Lat)) + geom_point(aes(size=ltra), color="blue", alpha=3/4) + 
  ggtitle("Zinc Concentration (ppm)") + coord_equal() + theme_bw()

coordinates(combine.1) = ~Lon+Lat
TEM_v <-variogram(ltra~ 1, data= combine.1,cloud= FALSE)# cloud= F只显示各个区间数字
plot(TEM_v, plot.number= T)

TEM_v_fit <- fit.variogram(object= TEM_v,
                           model = vgm(1, "Pow", 1))
plot(TEM_v, TEM_v_fit) 

x <- seq(34.78,42.86,by=0.01)
m <- seq(39.58,41.67,by=0.01)
y = rep(x,times=210)
n <- rep(m,times=809)
new = cbind.data.frame(n,y)
colnames(new) = c("Lat","Lon")
coordinates(new) <- ~Lon+Lat
lzn.kriged <- krige(ltra ~ 1, combine.1, new, model=TEM_v_fit)

lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=Lon, y=Lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()
lzn = as.data.frame(lzn.kriged)
lzn$rainfall = exp(lzn$var1.pred)
lzn$rainfall = lzn$rainfall -1
lzn[which(lzn$rainfall <0),5] <- 0








lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=Lon, y=Lat)) + geom_tile(aes(fill=rainfall)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()
lzn = as.data.frame(lzn.kriged)
lzn$rainfall = exp(lzn$var1.pred)





library(raster)
R = raster("D:/ITC/thesis_Turkey/CHELSA_pr_31_12_1980_V.2.1.tif")

  ra = raster("E:/RadarData/2019/12/RAD_NL25_RAC_MFBS_01H_201912290100.nc")+
  

combine.1 = as.data.frame(combine.1)
#examples online
data(meuse)
meuse %>% as.data.frame %>% 
  ggplot(aes(x, y)) + geom_point(aes(size=zinc), color="blue", alpha=3/4) + 
  ggtitle("Zinc Concentration (ppm)") + coord_equal() + theme_bw()
coordinates(meuse) <- ~ x + y
lzn.vgm <- variogram(log(zinc)~1, meuse) # calculates sample variogram values 
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1)) # fit model
plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model
data("meuse.grid")
coordinates(meuse.grid) <- ~ x + y # step 3 above
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)
lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()















