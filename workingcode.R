str_name <- 'tur_clip1.tif'
setwd("D:/ITC/thesis_Turkey")
imported_raster=raster(str_name)
turkey = imported_raster
plot(turkey)
turkey = aggregate(turkey, fact=2, expand=FALSE, fun=mean, na.rm=TRUE)

turkey = rasterToPoints(turkey)
turkey = as.data.frame(turkey)
write.table(turkey, "downscaled.txt", sep = "\t", col.names = F, row.names = F)
library(geosphere)
distGeo(c(34.91667,42.03333),c(34.93333,42.03333))

write.table(turkey,"new.txt",sep = "\t", col.names = F, row.names = F)

#数据来源： https://www.ngdc.noaa.gov/mgg/topo/globe.html


write.table(county_region, "countyN.txt", sep = "\t", col.names = F, row.names = F)

Turkey = read.delim("D:/ITC/thesis_Turkey/new_downscale.txt", sep = "\t")
