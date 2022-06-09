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

dataFolder = "D:/ITC/thesis_Turkey/DATA_08/"
train = read.csv(paste0(dataFolder,"train_data.csv"), header = TRUE)
#state = shapefile(paste0(dataFolder,"GP_STATE.shp"))
grid = read.csv(paste0(dataFolder,"GP_prediction_grid_data.csv"), header = TRUE)
powerTransform(train$SOC)
SOC.bc <- bcPower(train$SOC,0.2523339)
train$SOC.bc <- bcPower(train$SOC,0.2523339)

#Correlation matrix
co.var <- train[,c(11:19)]
df.cor <- cbind(SOC.bc,co.var)
cor.matrix <- rcorr(as.matrix(df.cor))

#Variogram
coordinates(train) = ~x+y
coordinates(grid) = ~x+y
v.soc <- variogram(SOC.bc~1,data = train, cloud = F)

m.soc <- vgm(1.5,"Exp",400000,0.5)
m.f.soc <- fit.variogram(v.soc,m.soc)
p1 <- plot(v.soc,pl=F,model=m.f.soc,main = "SOC")


v.ndvi <- variogram(NDVI~1, data = train, cloud = F)
m.ndvi<- vgm(1.5,"Exp",40000,0.5)
m.f.ndvi <- fit.variogram(v.ndvi,m.ndvi)
p2 <- plot(v.ndvi,pl=F, model = m.f.ndvi, main = "NDVI")

grid.arrange(p1,p2,ncol = 2)

#Cross-Variogram
g <- gstat(NULL,id = "SOC",form = SOC.bc~1, data = train)
g <- gstat(g,id = "NDVI", form = NDVI~1, data = train)

v.cross <- variogram(g)
plot(v.cross,pl = F)

g <- gstat(g, id = "SOC", model = m.f.soc, fill.all = T)
g <- fit.lmc(v.cross,g)
plot(variogram(g),model = g$model)

#Co-kriging prediction at grid locations
CK = predict(g,grid)
summary(CK)
k1 <- 1/0.2523339
CK$CK.pred <- ((CK$SOC.pred * 0.25233339+1)^k1)
CK$CK.var <- ((CK$SOC.var * 0.25233339+1)^k1)
summary(CK)


CK.pred <- rasterFromXYZ(as.data.frame(CK)[,c("x","y","CK.pred")])
CK.var <- rasterFromXYZ(as.data.frame(CK)[,c("x","y","CK.var")])
p3<-ggR(CK.pred, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("CK Predicted SOC")+
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



library(sp)
demo(meuse,ask = FALSE,echo = FALSE)

v = variogram(log(zinc)~1,meuse)
v.fit = fit.variogram(v, vgm(1, "Sph", 900, 1))
v.fit
fit.variogram(v, vgm("Sph"))
fit.variogram(v, vgm(c("Exp", "Sph")))
fit.variogram(v, vgm(c("Exp", "Mat", "Sph")))

v0 = variogram(zinc~1, meuse)
fit.variogram(v0, vgm(c("Exp", "Mat", "Sph")))


v.fit = fit.variogram(v, vgm(c("Exp", "Mat", "Sph")))
v.fit



data(meuse)
str(meuse)
data(meuse.grid)
meuse.pb <- meuse[ seq(1, length(meuse$lead), by=3),
                   c("x", "y", "lead", "om", "zinc")]
meuse.pb <- cbind(meuse.pb,
                  ltpb = log10(meuse.pb$lead),
                  ltom = log10(meuse.pb$om),
                  ltzn = log10(meuse.pb$zinc))

meuse.extra <- meuse[setdiff(rownames(meuse), rownames(meuse.pb)),
                     c("x", "y", "lead")]
meuse.extra <- cbind(meuse.extra, ltpb = log10(meuse.extra$lead))

coordinates(meuse) <- ~ x + y
# alternate command format: coordinates(meuse) <- c("x", "y")
coordinates(meuse.pb) <- ~ x + y
coordinates(meuse.extra) <- ~ x + y
coordinates(meuse.grid) <- ~ x + y

xyplot(y ~ x, as.data.frame(meuse), asp="iso",
       panel = function(x, ...) {
         panel.points(coordinates(meuse),
                      cex=1.8*(log10(meuse$lead) - 1.3),
                      pch=1, col="blue");
         panel.points(coordinates(meuse.pb),
                      cex=1.8*(meuse.pb$ltpb - 1.3),
                      pch=20, col="red");
         panel.grid(h=-1, v=-1, col="darkgrey")
       })

plot(v.ltpb.c <- variogram(ltpb ~ 1, data=meuse.pb, cutoff=1800, cloud=T))
plot(v.ltpb <- variogram(ltpb ~ 1, data=meuse.pb, cutoff=1800, width=200), pl=T)







