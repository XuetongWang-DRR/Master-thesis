#####Some helpful codes###############################
#Copy files
currentfiles =  "/Users/xuetongwang/Documents/thesis/model/OCK-GPM/Nov-Feb/Validation"
newlocation = "/Users/xuetongwang/Documents/thesis/model/OCK-GPM/Nov-Feb/Training2"
setwd("/Users/xuetongwang/Documents/thesis/model/OCK-GPM/Nov-Feb/Validation")
f = list.files()
f = f[substr(f,1,4) == "2016"]
file.copy(file.path(currentfiles,f), newlocation)


#Move files to another folder
library(filesstrings)
setwd("/Users/xuetongwang/Documents/thesis/model/modified_IMERG/Nov-Feb/TestTrain")
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


setwd("/Users/xuetongwang/Documents/thesis/model/modified_IMERG/Jul-Oct/TestTrain")
GPM1 = read.table("IMERG20150701E.txt",sep = "\t", header = T)
#GPM1 = rasterFromXYZ(as.data.frame(GPM1)[,c("Lon","Lat","Rainfall")])

setwd("/Users/xuetongwang/Documents/thesis/model/IDW-GPM/Jul-Oct/TestTrain")
IDW1 = read.table("20150701E.txt",sep = "\t", header = T)

setwd("/Users/xuetongwang/Documents/thesis/model/OCK-GPM/Jul-Oct/TestTrain")
OCK1 = read.table("20150701E.txt",sep = "\t", header = T)

#IDW1 = rasterFromXYZ(as.data.frame(IDW1)[,c("Lon","Lat","Rainfall")])
#par(mfrow = c(1,2))
#plot(IDW1)
#plot(GPM1)

setwd("/Users/xuetongwang/Documents/thesis/model")
cov = read.table("GPMcov.txt",sep = "\t", header = T)

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

setwd("/Users/xuetongwang/Documents/thesis/model/modified_IMERG/Jul-Oct/TestTrain")
a = list.files()
#need change according to the situation
#remove 20191117
#remove 20200404
a = a[-c(1,125,249,373)]
library(lubridate)

for (i in a){
  T = substr(i,6,13)
  D = as_date(T)
  DP = D-1
  DP = as.numeric(format(DP,"%Y%m%d"))
  TP = D-10
  TP = as.numeric(format(TP,"%Y%m%d"))
  
  
  setwd("/Users/xuetongwang/Documents/thesis/model/modified_IMERG/Jul-Oct/TestTrain")
  GPM0 = read.table(paste0("IMERG",DP,"E.txt"),sep = "\t", header = TRUE)
  GPM1 = read.table(paste0("IMERG",T,"E.txt"),sep = "\t", header = TRUE)
  
  
  setwd("/Users/xuetongwang/Documents/thesis/model/IDW-GPM/Jul-Oct/TestTrain")
  IDW1 = read.table(paste0(T,"E.txt"),sep = "\t", header = TRUE)
  setwd("/Users/xuetongwang/Documents/thesis/model/OCK-GPM/Jul-Oct/TestTrain")
  OCK1 = read.table(paste0(T,"E.txt"),sep = "\t", header = FALSE)
  colnames(OCK1) = c("Lon","Lat","Rainfall")
  
  GPM1$Elevation = cov$Elevation
  GPM1$Eastness = cov$Eastness
  GPM1$Northness = cov$Northness
  setwd("/Users/xuetongwang/Documents/thesis/model/NDVI/all")
  NDVI = raster(paste0("NDVI",TP,".tif"))
  
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
Sys.time()
GPM = GPM[-1,]

setwd("/Users/xuetongwang/Documents/thesis/model/modified")
write.table(GPM,"TestTrainJulOct.txt", sep = "\t", col.names = TRUE, row.names = FALSE)

GPM = read.table("TestTrainJulOct.txt",sep = "\t", header = TRUE)
GPM$extra = rep(c(1:492),each = 1130)
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
saveRDS(object = qrF_model,file = "ALLJulOct_IDW_5_5_500_200sample.rds")
Sys.time()    


Validation 
 
GPM = read.table("ALLTestJulOct.txt",sep = "\t", header = TRUE)
mod1 = readRDS (file = "ALLJulOct_IDW_5_5_500_200sample.rds")
qrF_model = mod1
 
qrF_prediction <- predict(qrF_model,
                          newdata = GPM[,!(names(GPM) %in% c("IDW","OCK","extra"))],
                          what = mean)


qrF_prediction = as.data.frame(qrF_prediction)
colnames(qrF_prediction) = c(1:50)
qrF_prediction$observation = GPM$IDW

qrF_prediction$crps = apply(qrF_prediction,1,function(x) crps_sample(as.numeric(x['observation']),as.numeric(x[1:50])))
clim = qrF_prediction$observation
qrF_prediction$crps_clim = apply(qrF_prediction,1,function(x) crps_sample(as.numeric(x['observation']),clim))

CDF = as.data.frame(matrix(NA,nrow = dim(GPM)[1], ncol = 15))
for (i in 1:239560){
  CDF[i,1] = condEcdf[[i]](0.05)
  CDF[i,2] = condEcdf[[i]](0.3)
  CDF[i,3] = condEcdf[[i]](0.5)
  CDF[i,4] = condEcdf[[i]](1)
  CDF[i,5] = condEcdf[[i]](2)
  CDF[i,6] = condEcdf[[i]](3)
  CDF[i,7] = condEcdf[[i]](5)
  CDF[i,8] = condEcdf[[i]](8)
  CDF[i,9] = condEcdf[[i]](10)
  CDF[i,10] = condEcdf[[i]](20)
  CDF[i,11] = condEcdf[[i]](30)
  CDF[i,12] = condEcdf[[i]](50)
  CDF[i,13] = condEcdf[[i]](80)
  CDF[i,14] = condEcdf[[i]](100)
  CDF[i,15] = condEcdf[[i]](150)
}  

write.table(CDF, "CDF_ALL_Mar-Jun.txt", sep = "\t", col.names = T, row.names = F)

CDF = read.table("CDF_ALL_Jul_Oct.txt",sep = "\t", header = TRUE)


CDF_JulOct = 1- CDF
GPM = na.omit(GPM)
GPM$IDW = as.numeric(GPM$IDW)
obs = GPM$IDW

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 0.05)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 0.05)] <- 1
df = cbind.data.frame(CDF_JulOct$V1,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 0.05mm threshold")
roc.plot(A, main = "ROC plot of qrf with 0.05mm threshold", binormal = TRUE, plot = "emp")
brier_0.05 = brier(df$obs,df$pred, bins=F)
brier.ss_0.05 = brier_0.05$ss
brier.bs_0.05 = brier_0.05$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 0.3)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 0.3)] <- 1
df = cbind.data.frame(CDF_JulOct$V2,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 0.3mm threshold")
roc.plot(A, main = "ROC plot of qrf with 0.3mm threshold", binormal = TRUE, plot = "emp")
brier_0.3 = brier(df$obs,df$pred, bins=F)
brier.ss_0.3 = brier_0.3$ss
brier.bs_0.3 = brier_0.3$bs

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 0.5)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 0.5)] <- 1
df = cbind.data.frame(CDF_JulOct$V3,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 0.5mm threshold")
roc.plot(A, main = "ROC plot of qrf with 0.5mm threshold", binormal = TRUE, plot = "emp")
brier_0.5 = brier(df$obs,df$pred, bins=F)
brier.ss_0.5 = brier_0.5$ss
brier.bs_0.5 = brier_0.5$bs

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 1)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 1)] <- 1
df = cbind.data.frame(CDF_JulOct$V4,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 1mm threshold")
roc.plot(A, main = "ROC plot of qrf with 1mm threshold", binormal = TRUE, plot = "emp")
brier_1 = brier(df$obs,df$pred, bins=F)
brier.ss_1 = brier_1$ss
brier.bs_1 = brier_1$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 2)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 2)] <- 1
df = cbind.data.frame(CDF_JulOct$V5,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 2mm threshold")
roc.plot(A, main = "ROC plot of qrf with 2mm threshold", binormal = TRUE, plot = "emp")
brier_2 = brier(df$obs,df$pred, bins=F)
brier.ss_2 = brier_2$ss
brier.bs_2 = brier_2$bs

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 3)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 3)] <- 1
df = cbind.data.frame(CDF_JulOct$V6,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 3mm threshold")
roc.plot(A, main = "ROC plot of qrf with 3mm threshold", binormal = TRUE, plot = "emp")
brier_3 = brier(df$obs,df$pred, bins=F)
brier.ss_3 = brier_3$ss
brier.bs_3 = brier_3$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 5)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 5)] <- 1
df = cbind.data.frame(CDF_JulOct$V7,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 5mm threshold")
roc.plot(A, main = "ROC plot of qrf with 5mm threshold", binormal = TRUE, plot = "emp")
brier_5 = brier(df$obs,df$pred, bins=F)
brier.ss_5 = brier_5$ss
brier.bs_5 = brier_5$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 8)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 8)] <- 1
df = cbind.data.frame(CDF_JulOct$V8,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 8mm threshold")
roc.plot(A, main = "ROC plot of qrf with 8mm threshold", binormal = TRUE, plot = "emp")
brier_8 = brier(df$obs,df$pred, bins=F)
brier.ss_8 = brier_8$ss
brier.bs_8 = brier_8$bs

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 10)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 10)] <- 1
df = cbind.data.frame(CDF_JulOct$V9,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 10mm threshold")
roc.plot(A, main = "ROC plot of qrf with 10mm threshold", binormal = TRUE, plot = "emp")
brier_10 = brier(df$obs,df$pred, bins=F)
brier.ss_10 = brier_10$ss
brier.bs_10 = brier_10$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 20)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 20)] <- 1
df = cbind.data.frame(CDF_JulOct$V10,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 20mm threshold")
roc.plot(A, main = "ROC plot of qrf with 20mm threshold", binormal = TRUE, plot = "emp")
brier_20 = brier(df$obs,df$pred, bins=F)
brier.ss_20 = brier_20$ss
brier.bs_20 = brier_20$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 30)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 30)] <- 1
df = cbind.data.frame(CDF_JulOct$V11,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 30mm threshold")
roc.plot(A, main = "ROC plot of qrf with 30mm threshold", binormal = TRUE, plot = "emp")
brier_30 = brier(df$obs,df$pred, bins=F)
brier.ss_30 = brier_30$ss
brier.bs_30 = brier_30$bs



obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 50)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 50)] <- 1
df = cbind.data.frame(CDF_JulOct$V12,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 50mm threshold")
roc.plot(A, main = "ROC plot of qrf with 50mm threshold", binormal = TRUE, plot = "emp")
brier_50 = brier(df$obs,df$pred, bins=F)
brier.ss_50 = brier_50$ss
brier.bs_50 = brier_50$bs


obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 80)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 80)] <- 1
df = cbind.data.frame(CDF_JulOct$V13,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 80mm threshold")
roc.plot(A, main = "ROC plot of qrf with 80mm threshold", binormal = TRUE, plot = "emp")
brier_80 = brier(df$obs,df$pred, bins=F)
brier.ss_80 = brier_80$ss
brier.bs_80 = brier_80$bs

obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 100)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 100)] <- 1
df = cbind.data.frame(CDF_JulOct$V14,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 100mm threshold")
roc.plot(A, main = "ROC plot of qrf with 100mm threshold", binormal = TRUE, plot = "emp")
brier_100 = brier(df$obs,df$pred, bins=F)
brier.ss_100 = brier_100$ss
brier.bs_100 = brier_100$bs



obs = as.data.frame(GPM$IDW)
obs$`GPM$IDW`[which(obs$`GPM$IDW`<= 150)] <- 0
obs$`GPM$IDW`[which(obs$`GPM$IDW` > 150)] <- 1
df = cbind.data.frame(CDF_JulOct$V15,obs$`GPM$IDW`)
colnames(df) = c("pred","obs")
A<- verify(df$obs, df$pred, frcst.type = "prob", obs.type = "binary")
reliability.plot(A, titl = "Reliability diagram of qrf with 150mm threshold")
roc.plot(A, main = "ROC.plot of qrf with 150mm threshold", binormal = TRUE, plot = "both")
brier_150 = brier(df$obs,df$pred, bins=F)
brier.ss_150 = brier_150$ss
brier.bs_150 = brier_150$bs

x3 = c(0.05,0.3,0.5,1,2,3,5,8,10,20,30,50,80,100,150)
y3 = c(brier.ss_0.05,brier.ss_0.3,brier.ss_0.5,brier.ss_1,brier.ss_2,brier.ss_3,brier.ss_5,brier.ss_8,brier.ss_10,brier.ss_20,brier.ss_30,brier.ss_50,brier.ss_80,brier.ss_100,brier.ss_150)

m1 = x
n1 = y
m1 = m1[-c(14,15)]
n1 = n1[-c(14,15)]

m2 = x2
n2 = y2
m2 = m2[-c(14,15)]
n2 = n2[-c(14,15)]

m3 = x3
n3 = y3
m3 = m3[-c(14,15)]
n3 = n3[-c(14,15)]

plot(m1,n1,main="BSS for different thresholds",xlab="threshold",ylab="BSS",col="blue",pch=3,cex=1.2,lwd=2,type='o')
lines(m2,n2,pch=2,cex=1.2,lwd=2,col="red",type='o')
lines(m3,n3,pch=5,cex=1.2,lwd=2,col="green",type='o')





#GPM2 = GPM[,-49]
GPM2 = GPM
GPM2$predict = qrF_prediction
GPM2$origin = GPM$min1
library(Metrics)
GPM2 = na.omit(GPM2)
rmse(GPM2$IDW,GPM2$origin)
rmse(GPM2$IDW,GPM2$predict)


cor.test(GPM2$IDW, GPM2$origin, 
                method = "pearson")

cor.test(GPM2$IDW, GPM2$predict, 
         method = "pearson")

dif1 = GPM2$origin-GPM2$IDW
dif2 = GPM2$predict-GPM2$IDW
mean(dif1)
mean(dif2)

name = names(GPM)
name = name[-c(47,48)]
name[order(mod1$importance)[c(46,45,44,43,42)]]


final = GPM2[,c(1,2,47,49,50)]
final$extra = rep(c(1:123),each = 1130)
setwd("/Users/xuetongwang/Documents/thesis/model/E/Jul-Oct/Testing")
a = list.files()
a = a[-1]

for (i in 1:123){
  setwd("/Users/xuetongwang/Documents/thesis/model/output/2019/Jul-Oct")
  save = paste0("output",substr(a[i],6,13),".txt")
  df = final[final$extra == i,]
  df = df[,-6]
  write.table(df,save, sep = "\t", col.names = TRUE, row.names = FALSE)
}

library(ggpubr)

setwd("/Users/xuetongwang/Documents/thesis/model/output/2015/Jul-Oct")
par(mfrow = c(1,3))
output = read.table("output20150824.txt",sep = "\t", header = TRUE)
output = na.omit(output)
max = max(max(output$IDW),max(output$predict),max(output$origin))
output1 = rasterFromXYZ(as.data.frame(output)[,c("Lon","Lat","IDW")])
output2 = rasterFromXYZ(as.data.frame(output)[,c("Lon","Lat","predict")])
output3 = rasterFromXYZ(as.data.frame(output)[,c("Lon","Lat","origin")])

library(ggplot2)
library(ggpubr)


p1 = ggR(output1, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,max), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("IDW")+
  theme(plot.title = element_text(hjust = 0.5))

p2 = ggR(output2, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,max), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("predict")+
  theme(plot.title = element_text(hjust = 0.5))

p3 = ggR(output3, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,max), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("IMERG")+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,ncol = 3,nrow=1,common.legend = T,legend = "right")


devtools::install_github(c("SantanderMetGroup/climate4R.value"))
