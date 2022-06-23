library(gstat)
library(sp)
library(RStoolbox)

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)

P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=3.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


IDW.out <- vector(length = length(P))
for (i in 1:length(P)) {
  IDW.out[i] <- idw(Precip_in ~ 1, P[-i,], P[i,], idp=3.0)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ P$Precip_in, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ P$Precip_in), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - P$Precip_in)^2) / length(P))


img <- gstat::idw(Precip_in~1, P, newdata=grd, idp=3.0)
n   <- length(P)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(Precip_in~1, P[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)



















station = read.table("D:/ITC/thesis_Turkey/stationID.txt", sep = "\t", header = T)
rainfall = read.table("D:/ITC/thesis_Turkey/rainfallFr2015.txt", sep = "\t", header = F)
downscale = read.table("D:/ITC/thesis_Turkey/downscaled.txt", sep = "\t", header = F)
colnames(downscale) = c("Lon","Lat","Elevation")
rainfall = as.data.frame(t(rainfall))
rainfall$V1 = substr(rainfall$V1,1,5)
rainfall$V1 = as.numeric(rainfall$V1)
station$Station.ID = as.numeric(station$Station.ID)


rainfall = rainfall[-1,c(1,2)]
colnames(rainfall) = c("V1","V2")
rainfall$V2 = as.numeric(rainfall$V2)
rainfall[which(rainfall$V2 == -999),2] <-NA
rainfall = na.omit(rainfall)

colnames(station)[1] <- c("ID")
colnames(rainfall) <- c("ID","rainfall")

combind = left_join(rainfall,station,by = c("ID"))

combind %>% as.data.frame %>% 
  ggplot(aes(Lon, Lat)) + geom_point(aes(size=rainfall), color="blue", alpha=3/4) + 
  ggtitle("Station Rainfall") + coord_equal() + theme_bw()

coordinates(combind) = ~Lon+Lat
coordinates(downscale) = ~Lon+Lat
R.idw <- gstat::idw(rainfall ~ 1, combind, newdata=downscale, idp=2.0)


R.idw = as.data.frame(R.idw)
R.idw = rasterFromXYZ(as.data.frame(R.idw)[,c("Lon","Lat","var1.pred")])

p1<-ggR(R.idw, geom_raster = TRUE) +
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


IDW.out <- vector(length = length(combind))
for (i in 1:length(combind)) {
  IDW.out[i] <- idw(rainfall ~ 1, combind[-i,], combind[i,], idp=3.0)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ combind$rainfall, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ combind$rainfall), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - combind$rainfall)^2) / length(combind))



img <- gstat::idw(rainfall~1, combind, newdata=downscale, idp=3.0)
n   <- length(combind)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(rainfall~1, combind[-i,], newdata=downscale, idp=2.0)
  st <- addLayer(st,raster(Z1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig$v)
r.m <- mask(r, W)

# Plot the map
tm_shape(img.sig) + tm_raster(n=7,title="95% confidence interval \n(in inches)") +
  tm_shape(combind) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)









