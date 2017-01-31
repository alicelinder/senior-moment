# Functional traits and competitiveness at each site and across latitudes
## by Alice Linder
### Jan. 31, 2017

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# Principal components analysis of functional traits
tree.traits <- read.csv("tree-traits.csv")

# calculate mass
tree.traits["leaf.mass"] <- tree.traits$Fresh.mass - tree.traits$Dry.mass
tree.traits <- tree.traits[,-13:-17]
tree.traits <- tree.traits[,-14:-21]

# apply skewness transformation, center and scale the variables prior to the application of PCA
# log transform 
log.traits <- log(traits[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE)