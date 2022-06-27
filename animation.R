
library(magick)
library(magrittr)
library(gifski)
library(gtools)  

#Making animation
## list file names and read in
setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM_png/extreme")
imgs <- list.files()

img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "test.gif")



#prepare for OCK/IDW

setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM/extreme")
s = list.files()
s = mixedsort(s)
name = c("2015-01-07","2015-01-14","2015-04-08","2015-10-08","2015-11-12",
         "2015-12-06","2016-12-07","2016-12-09","2016-12-14","2016-12-23",
         "2016-12-30","2017-01-24","2017-05-13","2017-10-02","2017-12-24",
         "2018-10-05","2018-10-26","2019-01-22","2019-11-02","2019-12-02")


t=1
for (i in s){
  
save = name[t]
setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM/extreme")
#OCK_GPM = read.table(i, sep = "\t", header = F)  #For OCK
OCK_GPM = read.table(i, sep = "\t", header = T)  #For IDW
colnames(OCK_GPM) = c("Lon","Lat","Rainfall")
OCK_GPM$Rainfall[which(OCK_GPM$Rainfall<0)] = 0
OCK_GPM <- rasterFromXYZ(as.data.frame(OCK_GPM)[,c("Lon","Lat","Rainfall")])  

p3<-ggR(OCK_GPM, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,80), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(as.Date(save))+
  theme(plot.title = element_text(hjust = 0.5))

setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM_png/extreme")
ggsave(paste0(save,".png"))  
t=t+1
}



######Prepare for GPM
setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_L_Test_Tur/extreme")
s = list.files()
name = c("2015-01-07","2015-01-14","2015-04-08","2015-10-08","2015-11-12",
         "2015-12-06","2016-12-07","2016-12-09","2016-12-14","2016-12-23",
         "2016-12-30","2017-01-24","2017-05-13","2017-10-02","2017-12-24",
         "2018-10-05","2018-10-26","2019-01-22","2019-11-02","2019-12-02")
t= 1
for (i in s){
save = name[t]  
setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_L_Test_Tur/extreme")

raster = raster(i)
raster = as.data.frame(rasterToPoints(raster))
colnames(raster) = c("Lon","Lat","Rainfall")

raster <- rasterFromXYZ(as.data.frame(raster)[,c("Lon","Lat","Rainfall")])

p3<-ggR(raster, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,80), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(as.Date(save))+
  theme(plot.title = element_text(hjust = 0.5))

setwd("D:/ITC/thesis_Turkey/animation/GPM_L_png/extreme")
ggsave(paste0(save,".png"))  
t=t+1
}


######Making plots for extreme days######################
setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM/extreme")
s = list.files()
s = mixedsort(s)
name = c("2015-01-07","2015-01-14","2015-04-08","2015-10-08","2015-11-12",
         "2015-12-06","2016-12-07","2016-12-09","2016-12-14","2016-12-23",
         "2016-12-30","2017-01-24","2017-05-13","2017-10-02","2017-12-24",
         "2018-10-05","2018-10-26","2019-01-22","2019-11-02","2019-12-02")



for (i in s){
  
  setwd("D:/ITC/thesis_Turkey/animation/IDW_GPM/extreme")
  IDW_GPM = read.table(i, sep = "\t", header = T)
  colnames(IDW_GPM) = c("Lon","Lat","Rainfall")
  IDW_GPM$Rainfall[which(IDW_GPM$Rainfall<0)] = 0
  IDW = cbind.data.frame(IDW,IDW_GPM$Rainfall)
  setwd("D:/ITC/thesis_Turkey/animation/OCK_GPM/extreme")
  OCK_GPM = read.table(i, sep = "\t", header = F)
  colnames(OCK_GPM) = c("Lon","Lat","Rainfall")
  OCK_GPM$Rainfall[which(OCK_GPM$Rainfall<0)] = 0
  OCK = cbind.data.frame(OCK,OCK_GPM$Rainfall)
  
}

OCK = OCK[,-3]
IDW = IDW[,-3]



setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_E_Test_Tur/extreme")
g = list.files()

for (i in g){
  save = name[t]  
  setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_E_Test_Tur/extreme")
  raster = raster(i)
  raster = as.data.frame(rasterToPoints(raster))
  colnames(raster) = c("Lon","Lat","Rainfall")
  ra = cbind.data.frame(ra,raster$Rainfall)
}



ra = ra[,-3]

raster = ra[,-c(1,2)]
raster = as.matrix(raster)
dim(raster) = c(1130*20,1)
raster = as.data.frame(raster)

ock = OCK[,-c(1,2)]
ock = as.matrix(ock)
dim(ock) = c(1130*20,1)
ock = as.data.frame(ock)

idw = IDW[,-c(1,2)]
idw = as.matrix(idw)
dim(idw) = c(1130*20,1)
idw = as.data.frame(idw)

final = cbind.data.frame(raster$V1,ock$V1,idw$V1)
colnames(final) = c("GPM","OCK","IDW")

smoothScatter(final1$IDW~final1$GPM,xlim = c(0,100),ylim = c(0,100),main = "IDW~GPM(mm)")
smoothScatter(final1$OCK~final1$GPM,xlim = c(0,100),ylim = c(0,100),main = "OCK~GPM(mm)")


130:2015-05-09
271:2015-09-27
300:2015-10-26
331:2015-11-26
472:2016-04-15
598:2016-08-19
680:2016-11-09
931:2017-07-18  
1018:2017-10-13
1212:2018-04-25
1302:2018-07-24
1332:2018-08-23
1519:2019-02-26
1534:2019-03-13
1616:2019-06-03
1750:2019-10-15  
1800:2019-12-04
1861:2020-02-03
1866:2020-02-08
1899:2020-03-12

(0,400)

#max
15:2015-01-14
99: 2015-04-08
341:2015-12-06
708:2016-12-07
710:2016-12-09
724:2016-12-23
731:2016-12-30
756:2017-01-24
865:2017-05-13
1484:2019-01-22
 




#mean
8:2015-01-07
282:2015-10-08
317:2015-11-12
715:2016-12-14
1007:2017-10-02
1090:2017-12-24
1375:2018-10-05
1396:2018-10-26
1768:2019-11-02
1798:2019-12-02





