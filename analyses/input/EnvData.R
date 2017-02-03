# Getting Climate Data
## Alice Nov. 9

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")
#setwd("C:/Users/wiwit_000/Documents/MEGA/Work_Harvard_postdoc/")

library(sp)
library(spdep)
library(raster)
library (rgdal)
library (maptools)

# download raster information
clim <- getData('worldclim', var='bio', res=.5, lon=-71, lat=44)
#clim2 <- getData('worldclim', var='bio', res=.5, lon=-72, lat=42)
#clim3 <- getData('worldclim', var='bio', res=.5, lon=-74, lat=46)
#clim4 <- getData('worldclim', var='bio', res=.5, lon=-74, lat=45)
#clim5 <- getData('worldclim', var='bio', res=.5, lon=-71, lat=43)

# delineate extents on raster image
extent.NA <- extent(clim)

# separate out first bioclimatic variable

##' comments:
##' the following command would not be necessary as the extent of each layer in clim
##' is that of the rasterbrick/stack

## mean annual temperature
bio1<-clim[[1]]

# mean annual precipitation
bio12<-clim[[12]]

# check on data
plot(bio1)
plot(bio12)

# make new coordinates to crop map
## cbind() will bind lat and long into 2 col. matrix

##' comments:
##' note that the coords you have created can be undefined, this is, they go beyond the
##' extent of the rasters or fall within NA values. you can easily see that by plotting where those coords are:
##' 
##' 


##' try the following coordinates within North America
coords = cbind(-75:-67, 40:48)  
points(coords[,1],coords[,2],pch=19)
help(points)
coords=SpatialPoints(coords)

# zoom into map
extent.NE <- extent(coords)
bio1<-crop(clim[[1]], extent.NE)
plot(bio1)

bio11<-crop(clim[[11]], extent.NE)
plot(bio11)

bio12<-crop(clim[[12]], extent.NE)
plot(bio12)

# view points on the map
points(coords[,1],coords[,2],pch=19)

# load latitudes and longitudes in from data
TreeLatLongs = read.csv("DBH.Lat.Long.csv")
tree.coords.lat <- TreeLatLongs$Lat
tree.coords = cbind(TreeLatLongs$Long, TreeLatLongs$Lat) 
View(tree.coords)
points(tree.coords[,1],tree.coords[,2],pch=19)

# extract data from raster files
## multiply by 0.1 to 
env.data<-extract(bio1, tree.coords)*0.1

View(env.data)

# add env.data to TreeLatLongs
TreeLatLongs["AnnualTemp"] <- env.data

## remember: mean annual temperatures are multiplied by 10 in data points

# look into PCA with climatic variables
## characterize climatic space for each species based on the PCAs
## looking at relationship with climatic variables as a continuous variable
## MDS but for climatic variables
## PCA for each site, across all sites to visualize climatic space
## summarize competition across all species within each site

