# Getting Climate Data
## Alice Nov. 9

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard Semester 7/Thesis/Spatial")
library(sp)
library(spdep)
library(raster)
library (rgdal)
library (maptools)

# download raster information
clim <- getData('worldclim', var='bio', res=10)

# delineate extents on raster image
extent.NA <- extent(clim)

# separate out first bioclimatic variable
bio1<- crop(clim[[1]], extent.NA)

# check on data
plot(bio1)

# make new coordinates to crop map
## cbind() will bind lat and long into 2 col. matrix
coords = cbind(0:100, -150:-50)
coord <- SpatialPoints(coords)

# crop image TROUBLESHOOTING HELP HERE
extent.NE <- extent(coords)
bio1<-crop(clim[[1]], extent.NE)
plot(bio1)

# extract data from raster files
env.data<-extract(bio1, coord)


## remember: mean annual temperatures are multiplied by 10 in data points
