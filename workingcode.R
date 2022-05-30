str_name <- 'tur_clip1.tif'
setwd("D:/ITC/thesis_Turkey")
imported_raster=raster(str_name)
turkey = imported_raster
plot(turkey)
turkey = rasterToPoints(turkey)
turkey = as.data.frame(turkey)
write.table(turkey, "downscaled.txt", sep = "\t", col.names = F, row.names = F)
library(geosphere)
distGeo(c(34.92083,42.0375),c(34.92917,42.0375))

#数据来源： https://www.ngdc.noaa.gov/mgg/topo/globe.html




