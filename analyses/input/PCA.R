# Principal Components Analaysis
## Alice -- February 27 2017

### Alice project
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## set wd
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# LIBRARIES HERE
library(geometry) # install.packages("vegan")
library(FD) # install.packages("FD")
library(plyr) # install.packages("plyr")
library(dplyr) # install.packages("dplyr")
library(reshape2) # install.packages("reshape2")
library(stringr) # install.packages("stringr")


# load data
tree.traits <- read.csv("tree-traits.csv")

# remove Distance, bottom.angle, top.angle, bottom.m, Top.m
tree.traits <- tree.traits[,-13:-17]

# remove DBH.1-DBH.5, notes, route, and date sampled
tree.traits <- tree.traits[,-14:-21]

# calculate stem density
tree.traits$Stem.density = tree.traits$Stem.mass/tree.traits$Stem.volume

# calculate SLA
tree.traits$SLA = tree.traits$Leaf.area / tree.traits$Dry.mass

# calculate C:N ratio
tree.traits$c.n = tree.traits$X.C / tree.traits$X.N
head(tree.traits)

### TO DO I'm getting an error on this for some reason -- formatted correctly?
## Problem is that DBH is always 0.5 for these small ones. So here I add a small amount of random noise to the 0.5 dbh individuals
zerofives <- tree.traits$DBH == 0.5 & !is.na(tree.traits$DBH) 
tree.traits$DBH[zerofives] = 0.5 + runif(length(tree.traits$DBH[zerofives]), max = 0.01)

# same for exactly 1 cm
ones <- tree.traits$DBH == 1 & !is.na(tree.traits$DBH) 
tree.traits$DBH[ones] = 1 + runif(length(tree.traits$DBH[ones]), max = 0.01)

#ex <- subset(tree.traits, Site == "GR" & Species == "KALANG")
#ex <- subset(tree.traits, Site == "GR" & Species == "MYRGAL")

# choose traits
tr <- c("Individual", "Species","SLA", "Stem.density", "DBH", "c.n")
#tr <- c("Individual", "Species","SLA", "Stem.density", "DBH", "c.n")

tr <- tree.traits[tr]

# use only complete cases
ex <- tr[complete.cases(tr),]

# log transform 
log.tr <- log(ex[, 3:6])
tr.species <- ex[, 2]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
tr.pca <- prcomp(log.tr,
                 center = TRUE,
                 scale. = TRUE)

biplot(tr.pca)

print(tr.pca)
summary(tr.pca)
tr.pca$x

# find function to plot polygon around points




## TO DO: this is taken from Nacho's code. 
## Why does he reverse the sign of the roation and the PCA? Also how do you explain these numbers?
tr.pca$rotation=-tr.pca$rotation
tr.pca$x=-tr.pca$x


pca.axis1<- tr.pca$x[,1]
pca.axis2<- tr.pca$x[,2]
#plot(pca.axis1,pca.axis2)
# environment.centroid<-c(mean(pca.axis1),mean(pca.axis2))

#TO DO: copy Nacho's code.

target.spp<-c("ACEPEN","BETPAP","CORALT"
              ,"FAGGRA","HAMVIR","SORAME")

pca.sps <- vector()



for(i in 1:6){
  spsi<-target.spp[i]
  print(spsi)
  log.tr.i <- log(ex[ex$Species == spsi, 3:7])
  
  pca.spsi <- prcomp(log.tr.i,center = T,scale. = T)
  pca.spsi$rotation=-pca.spsi$rotation
  pca.spsi$x=-pca.spsi$x
  
  
  pca.axis1<- pca.spsi$x[,1]
  pca.axis2<- pca.spsi$x[,2]
  
  # TO DO: save
  pca.sps[[i]] <- pca.spsi

}

# ACEPEN

log.tr <- log(ex[ex$Species == "ACEPEN", 3:6])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

# BETPAP
log.tr <- log(ex[ex$Species == "BETPAP", 3:8])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

# CORALT
log.tr <- log(ex[ex$Species == "CORALT", 3:8])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

# FAGGRA
log.tr <- log(ex[ex$Species == "FAGGRA", 3:8])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

# HAMVIR
log.tr <- log(ex[ex$Species == "HAMVIR", 3:8])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

# SORAME
log.tr <- log(ex[ex$Species == "SORAME", 3:8])

pca.sps <- prcomp(log.tr,center = T,scale. = T)
pca.sps$rotation=-pca.sps$rotation
pca.sps$x=-pca.sps$x

summary(pca.sps)
biplot(pca.sps)

pca.axis1<- pca.sps$x[,1]
pca.axis2<- pca.sps$x[,2]

plot(pca.axis1,pca.axis2)
