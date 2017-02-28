# Functional traits and competitiveness at each site and across latitudes
## by Alice Linder
### Jan. 31, 2017

rm(list = ls())
options(stringsAsFactors=FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

load("CHVols.RData")
# LIBRARIES HERE
library(dplyr)
library(devtools)
library(ggplot2)
library(ggfortify) #install.packages("ggfortify")



load("Focal-Centroid.RData")

focal.centroid$Site <- unlist(
  lapply(strsplit(focal.centroid$Individual, "_"),
         function(x) x[[2]]))

names(focal.centroid)[names(focal.centroid) == "Site"] <- "site"


comp.index.mean <- aggregate(focal.centroid$relative.BA, list(Species = focal.centroid$sp, Site = focal.centroid$Site), FUN = mean, na.rm=TRUE)
?rename
ba.chvols <- merge(chvols.focal, focal.centroid, by = c("sp", "site"))



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
