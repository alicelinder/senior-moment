# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# LIBRARIES HERE
library(geometry)
library(FD)
library(dplyr)

# load data
tree.traits <- read.csv("tree-traits.csv")
class(tree.traits$Fresh.mass)

# calculate mass
tree.traits["leaf.mass"] <- tree.traits$Fresh.mass - tree.traits$Dry.mass
tree.traits <- tree.traits[,-13:-17]
tree.traits <- tree.traits[,-14:-21]

# clean up data and subset it
traits <- select(tree.traits, Site, Species, Leaf.area, Stem.volume, 
                 Height, DBH, X.N, X.C, Stomatal.Length, Stomatal.Density, 
                 leaf.mass)

# create matrix containing abundances of the species or presence/absence with 
## rows = sites and species = columns

#convhulln(tree.traits$)
