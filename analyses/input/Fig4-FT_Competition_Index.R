# Functional traits and competitiveness at each site and across latitudes
## by Alice Linder
### Jan. 31, 2017

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# LIBRARIES HERE
library(dplyr)
library(devtools)
library(ggplot2)
library(ggfortify)

# Principal components analysis of functional traits to plot against competitiveness index
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

# choose traits
tr <- c("SLA", "Stem.density", "DBH", "c.n")
traits <- tree.traits[complete.cases(tree.traits[,tr]),]

# apply skewness transformation, center and scale the variables prior to the application of PCA
# log transform 
log.traits <- log(traits[tr])

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
traits.pca <- prcomp(log.traits,
                 center = TRUE,
                 scale = TRUE)

# view standard deviation and PC via print method and summary method
print(traits.pca)
summary(traits.pca)

autoplot(prcomp(log.traits))
traits.pca

# view on graph -- HOW TO DO THIS?

