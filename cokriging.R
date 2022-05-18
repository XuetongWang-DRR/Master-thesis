#Reading the data
station = read.table("D:/ITC/thesis_Turkey/stationID.txt", sep = "\t", header = T)
rainfall = read.table("D:/ITC/thesis_Turkey/rainfallFr2015.txt", sep = "\t", header = F)
#colnames(rainfall) = c("date",1:297)
rainfall = as.data.frame(t(rainfall))
rainfall$V1 = substr(rainfall$V1,1,5)
rainfall$V1 = as.numeric(rainfall$V1)
station$Station.ID = as.numeric(station$Station.ID)
rainfall = rainfall[-1,c(1,11)]
colnames(rainfall) = c("V1","V2")
rainfall$V2 = as.numeric(rainfall$V2)
rainfall[which(rainfall$V2 == -999),2] <-NA
rainfall = na.omit(rainfall)
rainfall[which(rainfall$V2 == 0),2] <- 0.0001
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















