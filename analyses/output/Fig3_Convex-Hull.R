# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017
### used "Functional and Phylogenetic Ecology in R" by Nathan G. Swenson

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# LIBRARIES HERE
library(geometry)
library(FD)
library(dplyr)
library(reshape2)
library(plyr)

# load data
tree.traits <- read.csv("tree-traits.csv")
class(tree.traits$Fresh.mass)

# calculate mass
tree.traits["leaf.mass"] <- tree.traits$Fresh.mass - tree.traits$Dry.mass
tree.traits <- tree.traits[,-13:-17]
tree.traits <- tree.traits[,-14:-21]

# clean up data and subset it based on species
traits <- select(tree.traits, Site, Species, Leaf.area, Stem.volume, 
                 Height, DBH, X.N, X.C, Stomatal.Length, Stomatal.Density, 
                 leaf.mass)

class(traits)

## DO I NEED TO DO THIS?
#traits <- na.omit(traits)

# create data frame to store convex hull values with 176 rows x 9 columns
con.hull <- data.frame(matrix(NA, nrow = 176, ncol = 10))
rownames(con.hull) <- NULL 
colnames(con.hull) <- c("Species", "Leaf.area", "Stem.volume", "Height", 
                        "DBH", "X.N", "X.C", "Stomatal.Length", 
                        "Stomatal.Density", "leaf.mass")



# find max and min values of each trait within each species
range.function <- function(x) {
  com.names <- unique(as.data.frame(traits$Species))
  
  apply(traits[com.names, ], MARGIN = 2, max) -
  apply(traits[com.names, ], MARGIN = 2, min)
}

apply()


?apply
acepen <- max(traits[traits$Species == i, 2])

# create matrix containing abundances of the species or presence/absence with 
## rows = sites and species = columns
convhulln(traits)

#convhulln(tree.traits$)

range.function <- function(x) {
  ## Get names of the species present in our community
  com.names <- unique(as.data.frame(traits$Species))
  
  ## Calculate the range for each trait
  apply(traits[com.names, ], MARGIN = 2, max)
  
}

max(traits[com.names, ])
