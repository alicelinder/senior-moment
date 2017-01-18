# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# LIBRARIES HERE

# load data
tree.traits <- read.csv("tree-traits.csv")
class(tree.traits$Fresh.mass)

# calculate mass
tree.traits["leaf.mass"] <- tree.traits$Fresh.mass - tree.traits$Dry.mass
tree.traits <- tree.traits[,-13:-17]
tree.traits <- tree.traits[,-14:-21]

