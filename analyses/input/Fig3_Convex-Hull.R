# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017
### used "Functional and Phylogenetic Ecology in R" by Nathan G. Swenson

# Seems that convex hull (functional richness) only works for communities in which all 
## individuals have been sampled for traits. Any way around this? I'm thinking at the
## moment that I should just complete a PCA with functional traits and plot this
## or coefficient of variance across ranges.

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

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

# remove Distance, bottom.angle, top.angle, bottom.m, Top.m
tree.traits <- tree.traits[,-13:-17]

# remove DBH.1-DBH.5, notes, route, and date sampled
tree.traits <- tree.traits[,-14:-21]

# calculate stem density
Stem.density <- tree.traits$Stem.mass/tree.traits$Stem.volume

# calculate SLA
SLA <- tree.traits$Leaf.area / tree.traits$Dry.mass

# calculate ratio C:N
c.n <- tree.traits$X.C / tree.traits$X.N
tree.traits <- data.frame(tree.traits, Stem.density, SLA, c.n)

# clean up data and subset it based on species
traits <- select(tree.traits, Site, Species, SLA, Stem.density, 
                 Height, DBH, c.n)

# remove all empty values
stopifnot(complete.cases(traits) != is.na(traits))
ok <- complete.cases(traits)
sum(!ok) # how many are not "ok" ?
traits <- traits[ok,]

# create data frame to store convex hull values with 176 rows x 9 columns
# con.hull <- data.frame(matrix(NA, nrow = 176, ncol = 10))
# rownames(con.hull) <- NULL 
# colnames(con.hull) <- c("Species", "Site", "Leaf.area", "Stem.volume", "Height", 
                        # "DBH", "X.N", "X.C", "Stomatal.Length", 
                        # "Stomatal.Density", "leaf.mass")

# create matrix containing traits of the species with rows = species and columns = traits for one species, at one site
head(traits)

ex <- subset(traits, Site == "GR" & Species == "ACEPEN")

# choose traits
tr <- c("SLA", "Stem.density", "DBH", "X.N") 

# Find complete cases for this set
ex <- ex[complete.cases(ex[tr]),]

vol = convhulln(ex, "FA")$vol

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
    
    



