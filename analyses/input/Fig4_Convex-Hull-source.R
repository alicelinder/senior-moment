# Functional traits convex hull value
## by Alice Linder
### written Jan. 16, 2017
### used "Functional and Phylogenetic Ecology in R" by Nathan G. Swenson

rm(list = ls())
#options(stringsAsFactors = FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data")

source("Fig2-source.R")

# LIBRARIES HERE
library(geometry) # install.packages("vegan")
library(FD) # install.packages("FD")
library(plyr) # install.packages("plyr")
library(dplyr) # install.packages("dplyr")
library(reshape2) # install.packages("reshape2")
library(stringr) # install.packages("stringr")
library(ggplot2)

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

# save tree traits
tree.traits.focal <- filter(tree.traits, Species == "ACEPEN" | Species == "BETPAP" | Species == "CORALT" | Species == "FAGGRA" | Species == "HAMVIR" | Species == "SORAME")

# remove wonky BETPAP value
tree.traits <-  tree.traits[-which(tree.traits$Individual == "BETPAP04_GR"),]



save(tree.traits.focal, file = "Species-Traits.RData")

# try removing BETPAP 03 GR because of huge trait values
#tree.traits <- tree.traits[-which(tree.traits$Individual == "BETPAP03_GR"),]

# same for FAGGRA 08 SH
tree.traits <- tree.traits[-which(tree.traits$Individual == "FAGGRA10_HF"),]

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

#ex <- subset(tree.traits, Site == "GR" & Species == "KALANG")
#ex <- subset(tree.traits, Site == "GR" & Species == "MYRGAL")

# choose traits
tr <- c("SLA", "Stem.density", "DBH", "c.n") 

# getting error with DBH (causing loop to stop) so trying it w/o DBH
#tr.2 <- c("SLA", "Stem.density", "c.n")


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


chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA" | sp == "HAMVIR" | sp == "SORAME")


#save(chvols, file = "Species Level CHV.csv", row.names=F)

# Now community level

sp.tr <- c("SLA", "Stem.density", "DBH", "c.n")
sp.tr <- tree.traits[,sp.tr]
sp.tr

#save(sp.tr, file= "Species-Traits.RData")


trait.means <- aggregate(sp.tr, list(Species = tree.traits$Species, Site = tree.traits$Site), FUN = mean, na.rm=TRUE)
trait.means[is.nan(trait.means$SLA), ]$SLA <- NA
trait.means[is.nan(trait.means$DBH), ]$DBH <- NA
trait.means[is.nan(trait.means$Stem.density), ]$Stem.density <- NA
trait.means[is.nan(trait.means$c.n), ]$c.n <- NA





d <- read.csv("all.species.dbh.csv", row.names = NULL)
d <- d[,1:3]
#d <- d2[,-2]


# put data into correct format
overstory <- distinct(d)
names(overstory)[3] = "Species"
#overstory <- rename(overstory, Comp.Species = "Species")

d2 <- melt(overstory, id = "Individual", measure.vars = "Species" )

over.all <- as.data.frame(acast(d2, Individual ~ value, length))

#head(over.all)
#over.all <- t(over.all)

# add in focal individual into matrix manually
acepen <- substr(rownames(over.all), 1, 6) == "ACEPEN"
acepen.col <- colnames(over.all) == "ACEPEN"
over.all[acepen, acepen.col] <- 1

betpap <- substr(rownames(over.all), 1, 6) == "BETPAP"
betpap.col <- colnames(over.all) == "BETPAP"
over.all[betpap, betpap.col] <- 1

coralt <- substr(rownames(over.all), 1, 6) == "CORALT"
coralt.col <- colnames(over.all) == "CORALT"
over.all[coralt, coralt.col] <- 1

faggra <- substr(rownames(over.all), 1, 6) == "FAGGRA"
faggra.col <- colnames(over.all) == "FAGGRA"
over.all[faggra, faggra.col] <- 1

hamvir <- substr(rownames(over.all), 1, 6) == "HAMVIR"
hamvir.col <- colnames(over.all) == "HAMVIR"
over.all[hamvir, hamvir.col] <- 1

sorame <- substr(rownames(over.all), 1, 6) == "SORAME"
sorame.col <- colnames(over.all) == "SORAME"
over.all[sorame, sorame.col] <- 1

head(over.all)
# get sites of each individual for presence/absence matrix
# x <- colnames(over.all)
# 
# x.site <- xstr_sub(x,-2,-1)
# rownames(over.all) <- x.site
#head(over.all)
 
# Updated upstream
d.hf <- over.all[grep("_HF", rownames(over.all)),]
d.gr <- over.all[grep("_GR", rownames(over.all)),]
d.wm <- over.all[grep("_WM", rownames(over.all)),]
d.sh <- over.all[grep("_SH", rownames(over.all)),]

#x.site <- str_sub(x,-2,-1)
#over.all <- t(over.all)
#rownames(over.all) <- x.site
#d.hf <- over.all[x.site == "HF",]
#d.gr <- over.all[x.site == "GR",]
#d.wm <- over.all[x.site == "WM",]
#d.sh <- over.all[x.site == "SH",]
# Stashed changes

head(over.all)
over.all


# find species present in each
m.hf <- trait.means[trait.means$Site == "HF",]
m.hf <- m.hf[complete.cases(m.hf),]
m.gr <- trait.means[trait.means$Site == "GR",]
m.gr <- m.gr[complete.cases(m.gr),]
#m.wm <- trait.means[trait.means$Site == "WM",]
m.sh <- trait.means[trait.means$Site == "SH",]

m.wm <- read.csv("m.wm.csv")



# check for species that are missing
# harvard forest

# check that there are observations in some and remove all columns with 0 species
x <- colSums(d.hf) != 0
d.hf <- subset(d.hf, select = x)
colSums(d.hf)


sp.t.hf <- colSums(d.hf) != 0
d.hf <- d.hf[,sp.t.hf]
d.hf.sp <- colnames(d.hf)
indata.hf <- m.hf$Species[m.hf$Species %in% d.hf.sp]

rownames(m.hf) <- m.hf$Species
d.hf <- d.hf[,colnames(d.hf) %in% indata.hf]
m.hf <- m.hf[rownames(m.hf) %in% indata.hf,]

# grant site
d.gr <- d.gr[,colSums(d.gr) != 0]
colSums(d.gr)

sp.t.gr <- colSums(d.gr) != 0
d.gr <- d.gr[,sp.t.gr]
d.gr.sp <- colnames(d.gr)
indata.gr <- m.gr$Species[m.gr$Species %in% d.gr.sp]
#d.gr <- subset(d.gr, select = c(indata.gr))

rownames(m.gr) <- m.gr$Species
d.gr <- d.gr[,colnames(d.gr) %in% indata.gr]
m.gr <- m.gr[rownames(m.gr) %in% indata.gr,]

colnames(d.gr)
rownames(m.gr)

# white mountains

d.wm <- d.wm[,colSums(d.wm) != 0]
colSums(d.wm)

sp.t.wm <- colSums(d.wm) != 0
d.wm <- d.wm[,sp.t.wm]
d.wm.sp <- colnames(d.wm)
indata.wm <- m.wm$Species[m.wm$Species %in% d.wm.sp]
#d.wm <- subset(d.wm, select = c(indata.wm))

rownames(m.wm) <- m.wm$Species
d.wm <- d.wm[,colnames(d.wm) %in% indata.wm]
m.wm <- m.wm[rownames(m.wm) %in% indata.wm,]


# saint hippolyte

d.sh <- d.sh[,colSums(d.sh) != 0]
colSums(d.sh)

sp.t.sh <- colSums(d.sh) != 0
d.sh <- d.sh[,sp.t.sh]
d.sh.sp <- colnames(d.sh)
indata.sh <- m.sh$Species[m.sh$Species %in% d.sh.sp]
#d.sh <- subset(d.sh, select = c(indata.sh))

rownames(m.sh) <- m.sh$Species
d.sh <- d.sh[,colnames(d.sh) %in% indata.sh]
m.sh <- m.sh[rownames(m.sh) %in% indata.sh,]

#write.csv(m.hf, file="m.wm.csv")
#save(list = c("m.hf", "m.wm", "m.gr", "m.sh", "d.hf", "d.wm", "d.gr", "d.sh"), file="trait.data.Rdata")

# functional richness
d.hf <- d.hf[rowSums(d.hf) != 0,]
d.hf
dim(d.hf)
head(d.hf)
dim(m.hf)
chvols.comm.hf <- dbFD(m.hf[3:6], d.hf, corr = 'none')$FRic
hf <- as.data.frame(chvols.comm.hf)




d.wm <- d.wm[rowSums(d.wm) != 0,]


chvols.comm.wm <- dbFD(m.wm[3:6], d.wm, corr = 'none')$FRic
wm <- as.data.frame(chvols.comm.wm)

#d.gr <- d.gr[rowSums(d.gr) != 0,]
dim(d.gr)
head(d.gr)
dim(m.gr)
m.gr
d.gr
chvols.comm.gr <- dbFD(m.gr[3:6], d.gr, corr = 'none')$FRic
gr <- as.data.frame(chvols.comm.gr)

# saint hippolyte
write.csv(m.sh, file= "m.sh.csv")
write.csv(d.sh, file= "d.sh.csv")
chvols.comm.sh <- dbFD(m.sh[3:6], d.sh, corr = 'none')$FRic
sh <- as.data.frame(chvols.comm.sh)

# # white mountains troubles
# myproblemspecies <- unique(m.wm$Species)[-1]
# # note: irun <- unique(m.wm$Species)[-c(1:4)]
# try <- m.wm[3:6][which(m.wm$Species %in% myproblemspecies),]
# d.wm.try <- d.wm[,which(colnames(d.wm) %in% row.names(try))]
# chvols.comm <- dbFD(try, d.wm.try, corr = 'none')$FRic
# 
# chvols.comm <- dbFD(m.wm[3:6], d.wm, corr = 'none')$FRic
# wm <- as.data.frame(chvols.comm)

## TO DO: getting error with White Mountain data for some reason.
#rbind(hf, wm, gr, sh)

load("chvols.comm.Rdata")

chvols.comm$Species = substr(rownames(x), 1, 6)

# find average functional richness at each site
chvols.mean <- aggregate(chvols.comm$chvols.comm, list(Site = chvols.comm$site), FUN = mean, na.rm=TRUE)


# find proportion of average functional richness/convex hull for each species (esp. focal species)
chvols$relative.vol <- NA
chvols[chvols$site == "HF",]$relative.vol <- chvols[chvols$site == "HF",]$vol/(chvols.mean[2, 2])
chvols[chvols$site == "GR",]$relative.vol <- chvols[chvols$site == "GR",]$vol/(chvols.mean[1, 2])
chvols[chvols$site == "WM",]$relative.vol <- chvols[chvols$site == "WM",]$vol/(chvols.mean[4, 2])
chvols[chvols$site == "SH",]$relative.vol <- chvols[chvols$site == "SH",]$vol/(chvols.mean[3, 2])





# find lat longs of each site
focal.centroid$Site <- unlist(
  lapply(strsplit(as.character(focal.centroid$Individual), "_"),
         function(x) x[[2]]))
lat.mean <- aggregate(focal.centroid$Lat, list(Site = focal.centroid$Site), FUN = mean, na.rm=TRUE)
long.mean <- aggregate(focal.centroid$Long, list(Site = focal.centroid$Site), FUN = mean, na.rm=TRUE)
lat.long.mean <- cbind(lat.mean, long.mean)
lat.long.mean <- lat.long.mean[,-3]
colnames(lat.long.mean) <- c("Site", "Lat", "Long")
chvols$lat[chvols$site == "GR"] <- lat.mean[lat.mean$Site == "GR",]$x
chvols$lat[chvols$site == "WM"] <- lat.mean[lat.mean$Site == "WM",]$x
chvols$lat[chvols$site == "HF"] <- lat.mean[lat.mean$Site == "HF",]$x
chvols$lat[chvols$site == "SH"] <- lat.mean[lat.mean$Site == "SH",]$x

chvols$long[chvols$site == "GR"] <- long.mean[long.mean$Site == "GR",]$x
chvols$long[chvols$site == "WM"] <- long.mean[long.mean$Site == "WM",]$x
chvols$long[chvols$site == "HF"] <- long.mean[long.mean$Site == "HF",]$x
chvols$long[chvols$site == "SH"] <- long.mean[long.mean$Site == "SH",]$x



chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA" | sp == "HAMVIR" | sp == "SORAME")

#save(chvols.focal, file="CHVols.RData")




# plot numerator in ratio to visualize what the data looks like
chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA")


ggplot(chvols.focal,
       aes(lat, vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Latitude") +
  ylab("Convex Hull of Focal Species")

plot(chvols.focal.num$lat , chvols.focal.num$vol, type="n", xlab="Latitude", ylab="Convex Hulls of Species")

myspecieslist <- unique(chvols.focal.num$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!

for (i in c(1:length(myspecieslist))){
  subby <- subset(chvols.focal.num, sp==myspecieslist[i])
  points(subby$lat, subby$vol, col=mycolors[i], pch=16)
}


# visualize denominator
ggplot(x,
       aes(site, chvols.comm, color = Species)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Site (not ordered)") +
  ylab("Convex Hull of individual plots")







