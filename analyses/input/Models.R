# Statistical tests for theses

library(lme4)
library(sjPlot)
library(tidyr)
library(reshape)
library(plyr)
library(reshape2)
library(ggplot2)

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# Model for first figure on competition vs. latitude

source("Fig2-source.R")

# ignore extra large DBH for FAGGUS value
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "FAGGRA" & focal.centroid$sum.BA > 20000),] 

# ignore QUEALB for graphing purposes
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "QUEALB"),]

lme1 <- lmer(relative.BA ~ minLatdiff + (minLatdiff | sp), data = focal.centroid)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)

sjt.lmer(lme1)

# Model for second figure on competition vs. climatic centroid
load("Clim.Focal.RData")

lme1 <- lmer(relative.BA ~ distance.to.climatic.centroid + (distance.to.climatic.centroid | sp), data = clim.focal)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)

sjt.lmer(lme1)

# linear model for convex hull volumes

load("CHVols.RData")

chvols.focal <- chvols.focal[-which(chvols.focal$sp == "HAMVIR"),]
chvols.focal <- chvols.focal[-which(chvols.focal$sp == "SORAME"),]

summary(lm(chvols.focal$lat ~ chvols.focal$relative.vol))
acepen <- chvols.focal[chvols.focal$sp == "ACEPEN",]$relative.vol
betpap <- chvols.focal[chvols.focal$sp == "BETPAP",]$relative.vol
coralt <- chvols.focal[chvols.focal$sp == "CORALT",]$relative.vol
faggra <- chvols.focal[chvols.focal$sp == "FAGGRA",]$relative.vol
summary(lm(chvols.focal[chvols.focal$sp == "FAGGRA",]$lat ~ faggra))

# linear model for convex hull volumes and climatic centroid

load("CHVol.Clim.RData")

clim.focal <- clim.focal[-which(clim.focal$sp == "CORALT"),]

summary(lm(clim.focal[clim.focal$sp == "BETPAP",]$distance.to.climatic.centroid ~ betpap))
acepen <- clim.focal[clim.focal$sp == "ACEPEN",]$relative.vol
betpap <- clim.focal[clim.focal$sp == "BETPAP",]$relative.vol
#coralt <- clim.focal[clim.focal$sp == "CORALT",]$relative.vol
faggra <- clim.focal[clim.focal$sp == "FAGGRA",]$relative.vol

# linear model for convex hull volumes and competition
load("CHVols.BA.RData")

# plot in base package
ba.chvols <- ba.chvols[-which(ba.chvols$sp == "CORALT"),]

summary(lm(ba.chvols[ba.chvols$sp == "BETPAP",]$relative.BA ~ betpap))
acepen <- ba.chvols[ba.chvols$sp == "ACEPEN",]$relative.vol
betpap <- ba.chvols[ba.chvols$sp == "BETPAP",]$relative.vol
#coralt <- ba.chvols[ba.chvols$sp == "CORALT",]$relative.vol
faggra <- ba.chvols[ba.chvols$sp == "FAGGRA",]$relative.vol

# Principal component analysis



