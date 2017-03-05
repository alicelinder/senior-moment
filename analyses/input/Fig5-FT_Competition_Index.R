# Functional traits and competitiveness at each site and across latitudes
## by Alice Linder
### Jan. 31, 2017

rm(list = ls())

#options(stringsAsFactors=FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# LIBRARIES HERE
library(dplyr)
library(devtools)
library(ggplot2)
library(ggfortify) #install.packages("ggfortify")

load("CHVols.centroid.Rdata")
load("CHVols.RData")
load("BA-CHVols.RData")
load("Clim.Focal.RData")

clim.focal <- subset(clim.focal, select = c("Individual", "sp", "distance.to.climatic.centroid"))
#x <- subset(chvols.focal, sp == "ACEPEN")

clim.focal$site <- unlist(
  lapply(strsplit(clim.focal$Individual, "_"),
         function(x) x[[2]]))

clim.focal$relative.vol <- NA

hf <- chvols.focal[chvols.focal$site == "HF",]
acepen <- clim.focal[chvols.focal$site]

#clim.focal[which(c(clim.focal$sp == "SORAME" & clim.focal$site == "SH")),]$relative.vol <- chvols.focal[which(c(chvols.focal$site == "SH" & chvols.focal$sp == "FAGGRA")),]$relative.vol

  

clim.focal <- clim.focal[-which(clim.focal$sp == "HAMVIR"),]
clim.focal <- clim.focal[-which(clim.focal$sp == "SORAME"),]

save(clim.focal, file = "CHVol.Clim.RData")


clim.vol <- merge(ba.chvols, clim.focal, by = c("Individual"))

ba.chvols <- ba.chvols[-which(ba.chvols$sp == "SORAME"),]

myspecieslist <- unique(ba.chvols$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 10) # need 6 really!

head(ba.chvols)
# plot in base package
plot(ba.chvols$relative.vol, ba.chvols$relative.BA, type="n", ylab="Relative Basal Area", xlab="Relative Convex Hull Volume")
?plot

for (i in c(1:length(myspecieslist))){
  subby <- subset(ba.chvols, sp==myspecieslist[i])
  points(subby$relative.vol, subby$relative.BA,  col=mycolors[i], pch="O")
}

  lm(ba.chvols$relative.BA[ba.chvols$sp == "FAGGRA"] ~ ba.chvols$relative.vol[ba.chvols$sp == "FAGGRA"])

# ACEPEN
abline(0.0254792, -0.0002011, col = "#1B9E77", lwd = 2)

# BETPAP
abline(0.255235, 0.007697, col = "#D95F02", lwd = 2)

#CORALT
abline(-0.0001574, 0.0000892  , col = "#7570B3", lwd = 2)

#FAGGRA
abline(0.083677, -0.007733, col = "#E7298A", lwd = 2)

legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifola"), 
       lty=1, col=mycolors, bty='n', cex=.75)




















load("Focal-Centroid.RData")

focal.centroid$Site <- unlist(
  lapply(strsplit(focal.centroid$Individual, "_"),
         function(x) x[[2]]))

names(focal.centroid)[names(focal.centroid) == "Site"] <- "site"


comp.index.mean <- aggregate(focal.centroid$relative.BA, list(Species = focal.centroid$sp, Site = focal.centroid$Site), FUN = mean, na.rm=TRUE)

ba.chvols <- merge(chvols.focal, focal.centroid, by = c("sp", "site"))
ba.chvols <- ba.chvols[-which(ba.chvols$sp == "HAMVIR"),]
ba.chvols <- ba.chvols[-which(ba.chvols$sp == "SORAME"),]

save(ba.chvols, file = "BA-CHVols.RData")


summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "ACEPEN",]))
summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "BETPAP",]))
summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "CORALT",]))
summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "FAGGRA",]))
summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "HAMVIR",]))
summary(lm1 <- lm(relative.BA ~ vol, data = ba.chvols[ba.chvols$sp == "SORAME",]))

acepen <- ba.chvols[ba.chvols$sp == "ACEPEN",]
betpap <- ba.chvols[ba.chvols$sp == "BETPAP",]
coralt <- ba.chvols[ba.chvols$sp == "CORALT",]
faggra <- ba.chvols[ba.chvols$sp == "FAGGRA",]

ba.chvols.focal <- rbind(acepen, betpap, coralt, faggra)
ba.chvols.focal$vol <- log(ba.chvols.focal$vol)
ba.chvols.focal$relative.BA <- log(ba.chvols.focal$relative.BA)


#lme1 <- lmer(relative.BA ~ vol + (relative.BA | sp), data = ba.chvols.focal)
ranef(lme1)

ggplot(ba.chvols,
       aes(vol,relative.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Convex Hull Volume") +
  ylab("Relative Basal Area")


# Principal components analysis of functional traits to plot against competitiveness index
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

# choose traits
tr <- c("SLA", "Stem.density", "DBH", "c.n")
traits <- tree.traits[complete.cases(tree.traits[,tr]),]

# apply skewness transformation, center and scale the variables prior to the application of PCA
# log transform 
log.traits <- log(traits[tr])

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
traits.pca <- prcomp(log.traits,
                 center = TRUE,
                 scale = TRUE)

# view standard deviation and PC via print method and summary method
print(traits.pca)
summary(traits.pca)

autoplot(prcomp(log.traits))

# view on graph -- HOW TO DO THIS?
