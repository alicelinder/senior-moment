
### Getting centroid for latitudinal range limits
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## set wd
setwd("C:/Users/Ignacio/Documents/MEGA/Work_Harvard_postdoc/SIDE PROJECTS/Alice Harvard")

## load in files
distribution.matrix<-read.csv("Nam_trees_incidence_matrix_I.csv")
IDs.env.matrix<-read.csv("Ids_lon_lats.csv")
dim(IDs.env.matrix)
dim(distribution.matrix)

## subset files acording to present IDS and target species
target.spp<-c("Acer.pensylvanicum","Betula.papyrifera","Cornus.alternifolia"
,"Fagus.grandifolia","Hamamelis.virginiana")

## subset columns (remove unwanted species)
distribution.matrix<-distribution.matrix[,c(1,which(colnames(distribution.matrix)%in%target.spp))]
dim(distribution.matrix)

## subset rows (unavailable sites)
distribution.matrix$FID<-distribution.matrix$FID+1
distribution.matrix<-subset(distribution.matrix,FID%in%IDs.env.matrix$Id)
IDs.env.matrix<-subset(IDs.env.matrix,Id%in%distribution.matrix$FID)
sum(IDs.env.matrix$Id==distribution.matrix$FID)


min.max.lats.each.sps<-as.data.frame(array(NA,dim=c(5,9)))
rownames(min.max.lats.each.sps)<-target.spp
colnames(min.max.lats.each.sps)<-c("minLat","maxLat","midLat","minTemp","maxTemp","midTemp",
                                   "minPrec","maxPrec","midPrec")

for(i in 1:length(target.spp)){
  spsi<-target.spp[i]
  sps.i.dist<-distribution.matrix[,spsi]
  sps.i.subset<-IDs.env.matrix[which(sps.i.dist==1),c("latitude","Avg_temper","Ann_precip")]
  min.max.lats.each.sps[i,c(1,4,7)]<-apply(sps.i.subset,2,min)
  min.max.lats.each.sps[i,c(2,5,8)]<-apply(sps.i.subset,2,max)
  min.max.lats.each.sps[i,c(3,6,9)]<-apply(apply(sps.i.subset,2,range),2,mean)
  
  }



