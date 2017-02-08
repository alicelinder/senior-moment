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
library(plyr)
library(dplyr)
library(reshape2)


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

# clean up data and subset it based on species
#traits <- select(tree.traits, Site, Species, SLA, Stem.density, 
                 #Height, DBH, X.N, X.C, Stomatal.Length, Stomatal.Density)

### TO DO I'm getting an error on this for some reason -- formatted correctly?
## Problem is that DBH is always 0.5 for these small ones. So here I add a small amount of random noise to the 0.5 dbh individuals
zerofives <- tree.traits$DBH == 0.5 & !is.na(tree.traits$DBH) 
tree.traits$DBH[zerofives] = 0.5 + runif(length(tree.traits$DBH[zerofives]), max = 0.01)

# same for exactly 1 cm
ones <- tree.traits$DBH == 1 & !is.na(tree.traits$DBH) 
tree.traits$DBH[ones] = 1 + runif(length(tree.traits$DBH[ones]), max = 0.01)

ex <- subset(tree.traits, Site == "GR" & Species == "KALANG")
ex <- subset(tree.traits, Site == "GR" & Species == "MYRGAL")

# choose traits
tr <- c("SLA", "Stem.density", "DBH", "c.n") 

# getting error with DBH (causing loop to stop) so trying it w/o DBH
#tr.2 <- c("SLA", "Stem.density", "c.n")

# Find complete cases for this set
ex <- ex[complete.cases(ex[,tr]),]

vol = convhulln(ex[,tr], "FA")$vol

# select only species of interest because KALANG and MYRGAL are being problematic
#speciestokeep <- as.factor(c("ACEPEN", "BETPAP", "CORALT", "FAGGRA", "SORAME"))
#tree.traits.interest <- tree.traits[which(tree.traits$Species %in% speciestokeep),]

#tr.species <- c("Site", "Species", "SLA", "Stem.density", "c.n")
#tree.traits.tr <- tree.traits.interest[,tr.species]

#coralt <- subset(tree.traits.tr, Site == "HF" & Species == "CORALT")
#vol = convhulln(coralt[,tr.2], "FA")$vol

# now apply this across all species and sites
# TO DO still getting error with species of interest with CORALT -- DBHs all 1 causing this

chvols = vector()

for(site in unique(tree.traits$Site)){
  for(sp in unique(tree.traits$Species)){
    
    ex <- subset(tree.traits, Site == site & Species == sp)
    
    # Find complete cases for this set
    ex <- ex[complete.cases(ex[tr]),]
    
    if(nrow(ex) <= length(tr)) vol = NA
    else  vol = convhulln(ex[,tr], "FA")$vol
    
    chvols = rbind(chvols, data.frame(site, sp, vol, n = nrow(ex)))
  }
}



save(chvols, file = "Species Level CHV.csv", row.names=F)

# Now community level

sp.tr <- c("Species", "Site", "SLA", "Stem.density", "DBH", "c.n")
sp.tr <- tree.traits[,sp.tr]
trait.means <- aggregate(sp.tr, list(Species = sp.tr$Species, Site = sp.tr$Site), mean)
