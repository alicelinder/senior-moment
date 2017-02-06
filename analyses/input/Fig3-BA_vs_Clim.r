
### Alice project
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## set wd
setwd("C:/Users/Ignacio/Documents/MEGA/Work_Harvard_postdoc/SIDE PROJECTS/Alice Harvard")

## read in Sorbus americana
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(sp)
library(rgdal)
library(spatstat)
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sorbus.amer=readShapePoly("sorbamer.shp",proj4string=crswgs84,verbose=TRUE)
plot(sorbus.amer)
points(IDs.env.matrix$longitude,IDs.env.matrix$latitude)
extent(sorbus.amer)
?SpatialPoints
coords<-cbind(IDs.env.matrix$longitude,IDs.env.matrix$latitude)
lonlats<-SpatialPoints(coords,proj4string=crswgs84)
overlap<-over(lonlats,sorbus.amer)
sorbus<-subset(overlap,!is.na(AREA))

## load in files
distribution.matrix<-read.csv("Nam_trees_incidence_matrix_I.csv")
IDs.env.matrix<-read.csv("Ids_lon_lats.csv")
dim(IDs.env.matrix)
dim(distribution.matrix)

## subset files acording to present IDS and target species
target.spp<-c("Acer.pensylvanicum","Betula.papyrifera","Cornus.alternifolia"
,"Fagus.grandifolia","Hamamelis.virginiana","Sorbus.americana")

## subset columns (remove unwanted species)
distribution.matrix<-distribution.matrix[,c(1,which(colnames(distribution.matrix)%in%target.spp))]
dim(distribution.matrix)

## subset rows (unavailable sites)
distribution.matrix$FID<-distribution.matrix$FID+1
distribution.matrix<-subset(distribution.matrix,FID%in%IDs.env.matrix$Id)
IDs.env.matrix<-subset(IDs.env.matrix,Id%in%distribution.matrix$FID)
sum(IDs.env.matrix$Id==distribution.matrix$FID)

##adding Sorbus
distribution.matrix$Sorbus.americana<-ifelse(!is.na(overlap$AREA),1,0)
head(distribution.matrix)
with(IDs.env.matrix,plot(longitude,latitude, 
                         col=distribution.matrix$Sorbus.americana+1))

min.max.lats.each.sps<-as.data.frame(array(NA,dim=c(6,9)))
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

## extract climatic envelope and distance to env.centroid for each species
head(IDs.env.matrix)
head(distribution.matrix)
coords.empirical<-read.csv("DBH.Lat.Long.csv") ## read from where you saved it
coords.empirical$species<-ifelse(grepl("ACEPEN",coords.empirical$Individual),"Acer.pensylvanicum",
                          ifelse(grepl("BETPAP",coords.empirical$Individual),"Betula.papyrifera",
                          ifelse(grepl("CORALT",coords.empirical$Individual),"Cornus.alternifolia",
                          ifelse(grepl("FAGGRA",coords.empirical$Individual),"Fagus.grandifolia",       
                          ifelse(grepl("HAMVIR",coords.empirical$Individual),"Hamamelis.virginiana",
                          ifelse(grepl("SORAME",coords.empirical$Individual),"Sorbus.americana","Other"))))))
coords.empirical<-subset(coords.empirical,!is.na(Lat))
head(coords.empirical)
coords.empirical$index.in.env.matrix<-rep(NA,nrow(coords.empirical))
index.in.Id<-NULL

for(i in 1:nrow(coords.empirical)){
  print(i)
  each.dist<-NULL 
  euc.dist<-function(x){sqrt(sum((x - coords.empirical[i,c("Long","Lat")])^2))}
  each.dist<-apply(IDs.env.matrix[,3:2],1,euc.dist)
  IDs.env.matrix[which.min(each.dist),]
  coords.empirical$index.in.env.matrix[i]<-which.min(each.dist)
  } 

coords.empirical$distance.to.climatic.centroid<-rep(NA,nrow(coords.empirical))  
climatic.envelope.each.sps<-list()

for(i in 1:6){
spsi<-target.spp[i]
print(spsi)
coords.empirical.sub<-subset(coords.empirical,species==spsi)
indices<-coords.empirical.sub$index.in.env.matrix
climate.env.i<-IDs.env.matrix[,4:8]*distribution.matrix[,spsi]
climate.env.i<-subset(climate.env.i,Avg_temper!=0)
pca.spsi <- prcomp(climate.env.i,center = T,scale. = T)
pca.spsi$rotation=-pca.spsi$rotation
pca.spsi$x=-pca.spsi$x

pca.axis1<- pca.spsi$x[,1]
pca.axis2<- pca.spsi$x[,2]
#plot(pca.axis1,pca.axis2)
environment.centroid<-c(mean(pca.axis1),mean(pca.axis2))


climate.env.isub<-IDs.env.matrix[indices,4:8]
predicted.spi<-predict(pca.spsi,newdata=climate.env.isub)[,1:2]

## compute euclidean distances to centroid from each point
dist.to.centroid<-NULL
for(j in 1:nrow(predicted.spi)){
  dist.to.centroid[j] <- sqrt(sum((predicted.spi[j,] - environment.centroid)^2))
  } 

## saving
climatic.envelope.each.sps[[i]]<-pca.spsi$x[,1:2]
coords.empirical[which(coords.empirical$species==spsi),"distance.to.climatic.centroid"]<-dist.to.centroid

}




