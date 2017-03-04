#rm(list = ls())
#options(stringsAsFactors = FALSE)
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")
#rm(list = ls())

m.wm <- read.csv("m.wm.csv")

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
#save(tree.traits.focal, file = "Species-Traits.RData")

# try removing BETPAP 03 GR because of huge trait values
#tree.traits <- tree.traits[-which(tree.traits$Individual == "BETPAP04_GR"),]

# same for FAGGRA 08 SH
#tree.traits <- tree.traits[-which(tree.traits$Individual == "FAGGRA08_SH"),]

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


m.wm <- m.wm[-which(m.wm$Species == "ACESPI"),]
m.wm <- m.wm[-which(m.wm$Species == "QUERUB"),]
rownames(m.wm) <- m.wm$Species
m.wm <- m.wm[,-1]

m.sh <- read.csv("m.sh.csv")
rownames(m.sh) <- m.sh$X
m.sh <- m.sh[,-1]
d.sh <- read.csv("d.sh.csv")
rownames(d.sh) <- d.sh$X
d.sh <- d.sh[,-1]
chvols.comm.sh <- dbFD(m.sh[3:6], d.sh, corr = 'none')$FRic
chvols.comm.sh

d.wm <- d.wm[,colSums(d.wm) != 0]
colSums(d.wm)

sp.t.wm <- colSums(d.wm) != 0
d.wm <- d.wm[,sp.t.wm]
d.wm.sp <- colnames(d.wm)
indata.wm <- m.wm$Species[m.wm$Species %in% d.wm.sp]



rownames(m.wm) <- m.wm$Species
d.wm <- d.wm[,colnames(d.wm) %in% indata.wm]
m.wm <- m.wm[rownames(m.wm) %in% indata.wm,]

chvols.comm.wm <- dbFD(m.wm[3:6], d.wm, corr = 'none')$FRic
wm <- as.data.frame(chvols.comm.wm)

myspecieslist <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA", "HAMVIR", "SORAME")

hf$site <- c("HF")
colnames(hf) <- c("chvols.comm", "site")
wm$site <- c("WM")
colnames(wm) <- c("chvols.comm", "site")
gr$site <- c("GR")
colnames(gr) <- c("chvols.comm", "site")
sh$site <- c("SH")
colnames(sh) <- c("chvols.comm", "site")

chvols.comm <- rbind(hf, wm, gr, sh)

save(chvols.comm, file="chvols.comm.Rdata")

#chvols[chvols$site == "WM",]$vol/(chvols.mean[chvols.mean$Site == "WM", chvols.mean$x])






load("CHVols.RData")
chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA" | sp == "HAMVIR" | sp == "SORAME")

# ggplot(chvols.focal,
#        aes(lat, relative.vol, color = sp)) +
#   geom_point() + 
#   geom_smooth(method="lm", se=F) +
#   facet_wrap(~sp, ncol = 3) +
#   xlab("Latitude") +
#   ylab("Relative Convex Hull Volume")


myspecieslist <- unique(chvols.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!

chvols.focal[which(chvols.focal$relative.vol > 50),]

# plot in base package
plot(chvols.focal$lat, chvols.focal$relative.vol, type="n", main="Relative Convex Hull Volume across Latitudes", xlab="Latitude", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(chvols.focal, sp==myspecieslist[i])
  points(subby$lat, subby$relative.vol, col=mycolors[i], pch=16)
  # pch is symbol shape
}

# rescale the volume
chvols.focal$scaled.relative.vol <- scale(chvols.focal$relative.vol, center=TRUE)
hist(chvols.focal$relative.vol, breaks=30)

# How much data?
range(chvols.focal$relative.vol, na.rm=TRUE)
chvols.nona <- subset(chvols.focal, is.na(relative.vol)==FALSE)
table(chvols.nona$sp)

# So, only use the species with more than one data point
spwithsomedata <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA")
chvols.focal.formodel <- chvols.focal[which(chvols.focal$sp %in% spwithsomedata),]

# plotting linear mixed effects model
lme1 <- lmer(scaled.relative.vol~ lat + (lat| sp), data = chvols.focal.formodel)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)
sjt.lmer(lme1)

mod.here <- lm(scaled.relative.vol~ lat, data = subset(chvols.focal, sp=="CORALT"))

anova(mod.here)
summary(mod.here)

load("CHVols.RData")
load("Focal-Centroid.RData")

focal.centroid$Site <- unlist(
  lapply(strsplit(focal.centroid$Individual, "_"),
         function(x) x[[2]]))

names(focal.centroid)[names(focal.centroid) == "Site"] <- "site"

ba.chvols <- merge(chvols.focal, focal.centroid, by = c("sp", "site"))


myspecieslist <- unique(ba.chvols$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


# plot in base package

for (i in c(1:length(myspecieslist))){
  subby <- subset(ba.chvols, sp==myspecieslist[i])
  points(subby$centroiddiff, subby$relative.vol, col=mycolors[i], pch=16)
  
}

hist(chvols$vol, breaks = 30)
chvols[which(chvols$vol > 100),]

# scale volumes
chvols.focal$scaled.vol <- scale(chvols.focal$vol, center=TRUE)
hist(chvols.focal$scaled.vol, breaks=30)

spwithsomedata <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA")
chvols.focal <- chvols.focal[which(chvols.focal$sp %in% spwithsomedata),]


ggplot(chvols.focal,
       aes(lat, scaled.vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Site (not ordered)") +
  ylab("Convex Hull of individual plots")

ggplot(chvols.focal.num,
       aes(lat, vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Latitude") +
  ylab("Convex Hull of Focal Species")

