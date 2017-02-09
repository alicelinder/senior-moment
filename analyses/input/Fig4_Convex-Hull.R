# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017
### used "Functional and Phylogenetic Ecology in R" by Nathan G. Swenson

# Seems that convex hull (functional richness) only works for communities in which all 
## individuals have been sampled for traits. Any way around this? I'm thinking at the
## moment that I should just complete a PCA with functional traits and plot this
## or coefficient of variance across ranges.

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data")

# LIBRARIES HERE
library(geometry)
library(FD)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)


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

#coralt <- subset(tree.traits.tr, Site == "hf" & Species == "CORALT")
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

sp.tr <- c("SLA", "Stem.density", "DBH", "c.n")
sp.tr <- tree.traits[,sp.tr]
sp.tr
trait.means <- aggregate(sp.tr, list(Species = tree.traits$Species, Site = tree.traits$Site), FUN = mean, na.rm=TRUE)
trait.means[is.nan(trait.means$SLA), ]$SLA <- NA
trait.means[is.nan(trait.means$DBH), ]$DBH <- NA
trait.means[is.nan(trait.means$Stem.density), ]$Stem.density <- NA
trait.means[is.nan(trait.means$c.n), ]$c.n <- NA

chvols.comm = vector()

for(site in unique(trait.means$Site)){
  for(sp in unique(trait.means$Species)){
    
    ex <- subset(trait.means, Site == site & Species == sp)
    
    # Find complete cases for this set
    ex <- trait.means[complete.cases(trait.means),]
    

    
    chvols.comm = rbind(chvols.comm, data.frame(site, sp, vol, n = nrow(ex)))
  }
}

ex <- trait.means[complete.cases(trait.means),]


d <- read.csv("all.species.dbh.csv", row.names = NULL)
d <- d[,1:3]
#d <- d2[,-2]


# put data into correct format
overstory <- distinct(d)
overstory <- rename(overstory, Species = Comp.Species)

d2 <- melt(overstory, id = "Individual", measure.vars = "Species" )

over.all <- as.data.frame(acast(d2, Individual ~ value, length))

head(over.all)
over.all <- t(over.all)
head(over.all)

# get sites of each individual for presence/absence matrix
x <- colnames(over.all)

x.site <- str_sub(x,-2,-1)
over.all <- t(over.all)
rownames(over.all) <- x.site
d.hf <- over.all[x.site == "HF",]
d.gr <- over.all[x.site == "GR",]
d.hf <- over.all[x.site == "WM",]
d.sh <- over.all[x.site == "SH",]

head(over.all)
over.all




# find species present in each
m.hf <- trait.means[trait.means$Site == "HF",]
m.gr <- trait.means[trait.means$Site == "GR",]
m.wm <- trait.means[trait.means$Site == "WM",]
m.sh <- trait.means[trait.means$Site == "SH",]

# check for species that are missing
# harvard forest
sp.t <- colSums(d.hf) != 0
d.hf <- d.hf[,sp.t]
d.hf.sp <- colnames(d.hf)
indata.hf <- m.hf$Species[m.hf$Species %in% d.hf.sp]
d.hf <- subset(d.hf, select = c(indata.hf))
rownames(m.hf) <- m.hf$Species
m.hf <- t(m.hf)
m.hf <- subset(m.hf, select = c(indata.hf))
m.hf <- t(m.hf)



# grant site
sp.t <- colSums(d.gr) != 0
d.gr <- d.gr[,sp.t]

d.gr.sp <- colnames(d.gr)
indata.gr <- m.gr$Species[m.gr$Species %in% d.gr.sp]
d.gr <- subset(d.gr, select = c(indata.gr))
rownames(m.gr) <- m.gr$Species
m.gr <- t(m.gr)
m.gr <- subset(m.gr, select = c(indata.gr))
m.gr <- t(m.gr)

# white mountains
d.wm.sp <- colnames(d.wm)
indata.wm <- m.wm$Species[m.wm$Species %in% d.wm.sp]
d.wm <- subset(d.wm, select = c(indata.wm))
rownames(m.wm) <- m.wm$Species
m.wm <- t(m.wm)
m.wm <- subset(m.wm, select = c(indata.wm))
m.wm <- t(m.wm)


# saint hippolyte
d.sh.sp <- colnames(d.sh)
indata.sh <- m.sh$Species[m.sh$Species %in% d.sh.sp]
d.sh <- subset(d.sh, select = c(indata.sh))
rownames(m.sh) <- m.sh$Species
m.sh <- t(m.sh)
m.sh <- subset(m.sh, select = c(indata.sh))
m.sh <- t(m.sh)


# functional richness -- getting an error here "x must be atomic for 'sort.list'"
dbFD(m.hf, d.hf)$FRic
