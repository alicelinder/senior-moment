# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017
### used "Functional and Phylogenetic Ecology in R" by Nathan G. Swenson

# Seems that convex hull (functional richness) only works for communities in which all 
## individuals have been sampled for traits. Any way around this? I'm thinking at the
## moment that I should just complete a PCA with functional traits and plot this
## or coefficient of variance across ranges.

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data")

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
colnames(con.hull) <- c("Species", "Site", "Leaf.area", "Stem.volume", "Height", 
                        "DBH", "X.N", "X.C", "Stomatal.Length", 
                        "Stomatal.Density", "leaf.mass")

?convhulln

# find max and min values of each trait within each species
range.function <- function(x) {
  com.names <- unique(as.data.frame(traits$Species))
  
  apply(traits[com.names, ], MARGIN = 2, max) -
  apply(traits[com.names, ], MARGIN = 2, min)
}


# create matrix containing traits of the species with rows = species and columns = traits for one species, at one site
head(traits)

traits$SLA = traits$Leaf.area / traits$leaf.mass

ex <- subset(traits, Site == "GR" & Species == "ACEPEN")

# choose traits
tr <- c("SLA", "Stem.volume", "DBH", "X.N") 

# Find complete cases for this set
ex <- ex[complete.cases(ex[tr]),]

vol = convhulln(ex[tr], "FA")$vol

# now apply this across all species and sites
chvols = vector()

for(site in unique(traits$Site)){
  for(sp in unique(traits$Species)){
    
    ex <- subset(traits, Site == site & Species == sp)
    
    # Find complete cases for this set
    ex <- ex[complete.cases(ex[tr]),]
    
    if(nrow(ex) < length(tr)) vol = NA
    else  vol = convhulln(ex[tr], "FA")$vol
    
    chvols = rbind(chvols, data.frame(site, sp, vol, n = nrow(ex)))
  }
}
    
    



