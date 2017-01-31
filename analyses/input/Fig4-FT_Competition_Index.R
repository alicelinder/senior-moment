# Functional traits and competitiveness at each site and across latitudes
## by Alice Linder
### Jan. 31, 2017

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

library(dplyr)

# Principal components analysis of functional traits
tree.traits <- read.csv("tree-traits.csv")

# calculate mass
tree.traits["leaf.mass"] <- tree.traits$Fresh.mass - tree.traits$Dry.mass
tree.traits <- tree.traits[,-13:-17]
tree.traits <- tree.traits[,-14:-21]

# clean up data and subset it based on species
traits <- select(tree.traits, Site, Species, Leaf.area, Stem.volume, 
                 Height, DBH, X.N, X.C, Stomatal.Length, Stomatal.Density, 
                 leaf.mass)
traits <- na.omit(traits)

# apply skewness transformation, center and scale the variables prior to the application of PCA
# log transform 
log.traits <- log(traits[, 3:11])
species <- traits[, 2]

## GETTING ERROR HERE -- "INFINITE OR MISSING VALUES IN X" (something wrong with na.omit?)
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
traits.pca <- prcomp(log.traits,
                 center = TRUE,
                 scale = TRUE)
