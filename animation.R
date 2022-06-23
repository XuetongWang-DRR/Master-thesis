
library(magick)
library(magrittr)
library(gifski)
library(gtools)  

#Making animation
## list file names and read in
setwd("D:/ITC/thesis_Turkey/animation/GPM_L_png")
imgs <- list.files()
imgs <- mixedsort(imgs)

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
set.seed(1)
s = sample(2:1979,20)
setwd("D:/ITC/thesis_Turkey/animation/OCK_GPM")
s = list.files()
s = mixedsort(s)
name = c("2015-05-09","2015-09-27","2015-10-26","2015-11-26","2016-04-15",
         "2016-08-19","2016-11-09","2017-07-18","2017-10-13","2018-04-25",
         "2018-07-24","2018-08-23","2019-02-26","2019-03-13","2019-06-03",
         "2019-10-15","2019-12-04","2020-02-03","2020-02-08","2020-03-12")

t=1
for (i in s){
  
save = name[t]
setwd("D:/ITC/thesis_Turkey/animation/OCK_GPM")
OCK_GPM = read.table(i, sep = "\t", header = F)
colnames(OCK_GPM) = c("Lon","Lat","Rainfall")
OCK_GPM$Rainfall[which(OCK_GPM$Rainfall<0)] = 0
OCK_GPM <- rasterFromXYZ(as.data.frame(OCK_GPM)[,c("Lon","Lat","Rainfall")])  

p3<-ggR(OCK_GPM, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,50), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(as.Date(save))+
  theme(plot.title = element_text(hjust = 0.5))

setwd("D:/ITC/thesis_Turkey/animation/OCK_GPM_png")
ggsave(paste0(save,".png"))  
t=t+1
}



######Prepare for GPM
setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_L_Test_Tur")
s = list.files()
name = c("2015-05-09","2015-09-27","2015-10-26","2015-11-26","2016-04-15",
         "2016-08-19","2016-11-09","2017-07-18","2017-10-13","2018-04-25",
         "2018-07-24","2018-08-23","2019-02-26","2019-03-13","2019-06-03",
         "2019-10-15","2019-12-04","2020-02-03","2020-02-08","2020-03-12")
t= 1
for (i in s){
save = name[t]  
setwd("D:/ITC/thesis_Turkey/IMERG/IMERG_L_Test_Tur")

raster = raster(i)
raster = as.data.frame(rasterToPoints(raster))
colnames(raster) = c("Lon","Lat","Rainfall")

raster <- rasterFromXYZ(as.data.frame(raster)[,c("Lon","Lat","Rainfall")])

p3<-ggR(raster, geom_raster = TRUE) +
  scale_fill_gradientn("", limits = c(0,50), colours = c("orange", "yellow", "green",  "sky blue","blue"))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(as.Date(save))+
  theme(plot.title = element_text(hjust = 0.5))

setwd("D:/ITC/thesis_Turkey/animation/GPM_L_png")
ggsave(paste0(save,".png"))  
t=t+1
}




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
2016-12-30
2016-12-23
2016-12-09
2017-05-13
2019-01-22
2015-12-06
2015-01-14
2017-01-24
2016-12-07
2015-04-08

2018-10-26
2019-11-02
2019-12-02
2015-10-08
2015-11-12
2016-12-14
2017-12-24
2015-01-07
2018-10-05
2017-10-02