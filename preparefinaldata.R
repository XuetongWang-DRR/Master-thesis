#####Some helpful codes###############################
#Move files to another folder
library(filesstrings)
setwd("/home/jovyan/Desktop/Xuetong/thesis/krigingR/IMERG/E/Nov-Feb")
files = list.files()
files = files[substr(files,6,9)=="2015"]

move_files(files,"Training",overwrite = F)

#change the name of data
files = list.files()
files = mixedsort(files)
setwd("/home/jovyan/Desktop/Xuetong/thesis/krigingR/IMERG/E/2020")
b = list.files()
a = c(a,b)

setwd("/home/jovyan/Desktop/Xuetong/thesis/krigingR/OCK-GPM")

i = 1
for (f in files){
  newname <- paste0(substr(a[i],5,12),"E.txt")
  file.rename(f,newname)
  i = i+1
}

######Making the model########
library(raster)


setwd("D:/ITC/thesis_Turkey/R/IMERG/Mar-Jun/Testing")
GPM1 = read.table("IMERG20190301E.txt",sep = "\t", header = T)
#GPM1 = rasterFromXYZ(as.data.frame(GPM1)[,c("Lon","Lat","Rainfall")])

setwd("D:/ITC/thesis_Turkey/R/IDW/Mar-Jun/Testing")
IDW1 = read.table("20190301E.txt",sep = "\t", header = T)
#IDW1 = rasterFromXYZ(as.data.frame(IDW1)[,c("Lon","Lat","Rainfall")])
#par(mfrow = c(1,2))
#plot(IDW1)
#plot(GPM1)

setwd("D:/ITC/thesis_Turkey/R")
cov = read.table("GPMcov.txt",sep = "\t", header = T)

library(geosphere)
d <- pointDistance(GPM1[,1:2],GPM1[,1:2],lonlat=T,allpairs = T)
df = as.data.frame(matrix(nrow = 1130,ncol = 20))
for (i in 1 :1130){
  data = d[i,]
  data = order(data)[1:20]
  df[i,]=data
}
GPM = as.data.frame(matrix(nrow = 1,ncol = 48))
colnames(GPM) = c("Lon","Lat","Elevation","Eastness","Northness","NDVI",
                   "min1","minP1","min2","minP2","min3","minP3","min4","minP4",
                   "min5","minP5","min6","minP6","min7","minP7","min8","minP8",
                   "min9","minP9","min10","minP10","min11","minP11","min12","minP12",
                   "min13","minP13","min14","minP14","min15","minP15","min16","minP16",
                   "min17","minP17","min18","minP18","min19","minP19","min20","minP20",
                   "IDW","OCK")


setwd("D:/ITC/thesis_Turkey/R/IMERG/Mar-Jun/Testing")
a = list.files()
#need change according to the situation
a = a[-c(1,124)]

for (i in a){
T = substr(i,6,13)
D = as_date(T)
DP = D-1
DP = as.numeric(format(DP,"%Y%m%d"))

setwd("D:/ITC/thesis_Turkey/R/IMERG/Mar-Jun/Testing")
GPM0 = read.table(paste0("IMERG",DP,"E.txt"),sep = "\t", header = TRUE)
GPM1 = read.table(paste0("IMERG",T,"E.txt"),sep = "\t", header = TRUE)


setwd("D:/ITC/thesis_Turkey/R/IDW/Mar-Jun/Testing")
IDW1 = read.table(paste0(T,"E.txt"),sep = "\t", header = TRUE)
setwd("D:/ITC/thesis_Turkey/R/OCK/Mar-Jun/Testing")
OCK1 = read.table(paste0(T,"E.txt"),sep = "\t", header = FALSE)
colnames(OCK1) = c("Lon","Lat","Rainfall")

GPM1$Elevation = cov$Elevation
GPM1$Eastness = cov$Eastness
GPM1$Northness = cov$Northness
setwd("D:/ITC/thesis_Turkey/R/NDVI/Mar-Jun/Testing")
NDVI = raster(paste0("NDVI",T,".tif"))
NDVI = as.data.frame(rasterToPoints(NDVI))
colnames(NDVI) = c("Lon","Lat","NDVI")

d <- pointDistance(GPM1[,1:2],NDVI[,1:2],lonlat=TRUE,allpairs = TRUE)
points <- apply(d,1,which.min)

GPM1$NDVI = NDVI$NDVI[points]

for (i in 1:20){
  GPM1[,i*2+6] = df[,i]
  GPM1[,i*2+7]= GPM0$Rainfall[GPM1[,i*2+6]]
  GPM1[,i*2+6]= GPM1$Rainfall[GPM1[,i*2+6]]
}

colnames(GPM1) = c("Lon","Lat","Rainfall","Elevation","Eastness","Northness","NDVI",
                   "min1","minP1","min2","minP2","min3","minP3","min4","minP4",
                   "min5","minP5","min6","minP6","min7","minP7","min8","minP8",
                   "min9","minP9","min10","minP10","min11","minP11","min12","minP12",
                   "min13","minP13","min14","minP14","min15","minP15","min16","minP16",
                   "min17","minP17","min18","minP18","min19","minP19","min20","minP20")
GPM1 = GPM1[,-3]
GPM1$IDW = IDW1$Rainfall
GPM1$OCK = OCK1$Rainfall
GPM = rbind.data.frame(GPM,GPM1)
}
GPM = GPM[-1,]

setwd("D:/ITC/thesis_Turkey/model/new")
write.table(GPM,"TestJulOct.txt", sep = "\t", col.names = TRUE, row.names = FALSE)

GPM = read.table("ValiJulOct.txt",sep = "\t", header = TRUE)
GPM$extra = rep(c(1:123),each = 1130)
library(dplyr)
set.seed(1)
GPM <- GPM %>% group_by(extra) %>% sample_n(size = 200)
GPM = as.data.frame(GPM)
#data_scaled = GPM
#vers2scale = c("Lon","Lat","Elevation","Eastness","Northness","NDVI",
#               "min1","minP1","min2","minP2","min3","minP3","min4","minP4",
#               "min5","minP5","min6","minP6","min7","minP7","min8","minP8",
#               "min9","minP9","min10","minP10","min11","minP11","min12","minP12",
#               "min13","minP13","min14","minP14","min15","minP15","min16","minP16",
#               "min17","minP17","min18","minP18","min19","minP19","min20","minP20")
#data_scaled[,vers2scale] = apply(data_scaled[,vers2scale],2,scale)
#GPM = data_scaled

library(quantregForest)

qrF_model <- quantregForest(x = GPM[,1:46],
                            y = GPM[,47],
                            nodesize = 5,
                            mtry = 5,
                            ntree = 500)
saveRDS(object = qrF_model,file = "JulOct_IDW_5_5_500_200sample.rds")
Sys.time()


Validation 


mod1 = readRDS (file = "NovFeb_IDW_5_5_500_500sample.rds")
qrF_model = mod1

qrF_prediction <- predict(qrF_model,
                          newdata = GPM[,!(names(GPM) %in% c("IDW","OCK","extra"))],
                          what = mean)


GPM2 = GPM[,-49]
GPM2$predict = qrF_prediction
GPM2$origin = GPM$min1
library(Metrics)
rmse(GPM2$IDW,GPM2$origin)
rmse(GPM2$IDW,GPM2$predict)