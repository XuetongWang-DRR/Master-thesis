library(raster)
library(rgdal)


setwd("D:/ITC/thesis_Turkey/IMERG/Data/example")
name = list.files()
for (i in name){
  save = paste0("crop",substr(i,24,31),".tif")
  setwd("D:/ITC/thesis_Turkey/IMERG/Data/example")
  raster = raster(i)
  shape <- readOGR("D:/ITC/thesis_Turkey/Clip/clip.shp")
  raster_crop <- crop(raster,shape,snap="out")
  fr <- rasterize(shape, raster_crop)   
  lr <- mask(x=raster_crop, mask=fr)
  
  setwd("D:/ITC/thesis_Turkey/IMERG/output/example")
  writeRaster(lr,save,options=c('TFW=YES'))
  
}


#Some helpful codes
plot(crop,
     main = "Shapefile imported into R - crop",
     axes = TRUE,
     border = "blue")


raster_crop <- crop(raster, crop,snap="out")
fr <- rasterize(crop, raster_crop)   
lr <- mask(x=raster_crop, mask=fr)

plot(lr)
lr = rasterToPoints(lr)

plot(raster_crop, main = "Cropped raster")
plot(shape, add = TRUE)





