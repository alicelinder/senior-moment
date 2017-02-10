## plot convex hull volumes


rm(list = ls())
#options(stringsAsFactors = FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data")

# LIBRARIES HERE
library(geometry) # install.packages("vegan")
library(FD) # install.packages("FD")
library(plyr) # install.packages("plyr")
library(dplyr) # install.packages("dplyr")
library(reshape2) # install.packages("reshape2")
library(stringr) # install.packages("stringr")

source('~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/analyses/input/Fig4_Convex-Hull-source.R')

# find average functional richness at each site
chvols.mean <- aggregate(x$chvols.comm, list(Site = x$Site), FUN = mean, na.rm=TRUE)

# find proportion of average functional richness/convex hull for each species (esp. focal species)

head(chvols.mean)
chvols[chvols$site == "GR",]$vol/(chvols.mean[chvols.mean$Site == "GR", chvols.mean$x])

# for some reason getting numbers where there should be NAs...not sure why this is happening

for(site in unique(chvols$site)){
  for(sp in chvols$sp){
    if(is.na(chvols$vol)){
      chvols$relative.vol = NA
    }
    else (chvols$relative.vol = chvols[chvols$site == site,]$vol/(chvols.mean[chvols.mean$Site == site, chvols.mean$x]))
  }
}


