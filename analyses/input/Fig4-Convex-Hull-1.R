## plot convex hull volumes


rm(list = ls())
options(stringsAsFactors = FALSE)
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

chvols.mean <- aggregate(x$chvols.comm, list(Site = x$Site), FUN = mean, na.rm=TRUE)

head(chvols)


head(x)
