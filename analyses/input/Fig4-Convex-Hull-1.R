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
library(ggplot2)

# plot the relationship

# subset just to include focal individuals
load("CHVols.RData")
#chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA" | sp == "HAMVIR" | sp == "SORAME")

ggplot(chvols.focal,
       aes(site, relative.vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3) +
  xlab("Distance from Climatic Centroid") +
  ylab("Relative Basal Area")


