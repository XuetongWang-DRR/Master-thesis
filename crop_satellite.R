library(raster)
library(rgdal)

#Crop GPM data
setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_E_2015")
name = list.files()
for (i in name){
  save = paste0("crop",substr(i,24,31),"early",".tif")
  setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_E_2015")
  nc_file <- "IMERGE.nc"
  shp_file <- "D:/ITC/thesis_Turkey/Clip/new.shp"
  
  
  nc_data <- brick(i)
  raster_data <- flip(t(flip(nc_data, direction = "y")), direction = "y")
  
  shapefile_data <- read_sf(shp_file)
  
  # Extract the extent of the shapefile and transform it to the raster CRS
  shp_extent <- st_bbox(shapefile_data, crs = st_crs(raster_data))
  
  # Crop the raster data to the extent of the shapefile
  cropped_raster <- crop(raster_data, shp_extent)
  
  # Convert the shapefile to a raster with the same resolution and CRS as the cropped raster
  mask_raster <- rasterize(shapefile_data, cropped_raster, field = 1)
  
  # Mask the raster data using the shapefile
  clipped_raster <- mask(cropped_raster, mask_raster)
  
  
  setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_E_2015NEW")
  writeRaster(lr,save,options=c('TFW=YES'))
}

dir()
allfile = dir()
tfwfile <- grep("*.tfw",allfile)
file.remove(allfile[tfwfile])

#Crop 1km data
setwd("D:/ITC/thesis_Turkey/all10g/all10")
raster = raster("g10g.tif")
shape <- readOGR("D:/ITC/thesis_Turkey/Clip/new.shp")
raster_crop <- crop(raster,shape,snap="out")
fr <- rasterize(shape, raster_crop)   
lr <- mask(x=raster_crop, mask=fr)
setwd("D:/ITC/thesis_Turkey")
write.table(lrd,"new_downscale.txt", sep = "\t", col.names = T, row.names = F)



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




#对文件夹内文件进行改名
files<-list.files()
for (f in files){
  newname <- paste0(substr(f,1,12),".png")
#  newname <- paste0(substr(f,4,5),"png")
  file.rename(f,newname)
}

#批量删除同一种文件
allfile = dir()   ## 目录下的所有文件
allfile
txtfile <- grep("*.txt", allfile)  ## 查找txt文件，返回索引
txtfile
file.remove(allfile[txtfile])   ## 删除txt文件
dir()




