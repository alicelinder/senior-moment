# MDS Plot of Species Composition
## written by Alice Linder and Dan Flynn
### updated by Alice on 22 Dec. 2016

library(vegan)
library(dplyr)
library(tidyr)
library(reshape)
library(plyr)
library(reshape2)
library(ggplot2)

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data") # For Dan

# MDS overstory
d <- read.csv("all.species.dbh.csv", row.names = NULL)
d <- d[,1:3]
#d <- d2[,-2]

# put data into correct format
overstory <- distinct(d)
overstory <- rename(overstory, c("Comp.Species" = "Species"))

# check
names(overstory)

# SOMETHING WRONG HERE
d <- melt(overstory, id = "Individual", measure.vars = "Species" )

over.all <- as.data.frame(acast(d, Individual ~ value, length))

head(over.all)
over.all <- t(over.all)
head(over.all)

# Analysis and summarizing richness of the overstory
richness <- apply(over.all, 2, sum)
?metaMDS

mds1 <- metaMDS(t(over.all), try = 100) # use t() to change it so that the communities are rows, and species are columns, which is the format that vegan uses

plot(mds1) # ok, lots of scatter, good ordination

overcomp <- data.frame(mds1$points)

overcomp$s <- richness # add our species richness calculations to this data frame

overcomp$sp <- substr(rownames(overcomp), 1, 6)  

# Get the site by getting the last two characters of the overcomp rownames
overcomp$site <- unlist(
  lapply(strsplit(rownames(overcomp), "_"),
         function(x) x[[2]]))

# For each species, plot the species richness by site. Order sites by south -> north
overcomp$site <- as.factor(overcomp$site)
levels(overcomp$site) <- c(3, 1, 4, 2)
overcomp$site <- factor(as.numeric(as.character(overcomp$site)), labels = c("HF", "WM", "GR", "SH"))

# Clear differences with site, changing space along MDS1
colz = alpha(c("#E7298A", "#1B9E77", "#D95F02", "#7570B3"), 0.5)

# plot MDS overstory
plot(mds1, type = "n",
     xlim = c(-2, 2),
     ylim = c(-1.2, 2),
     cex.lab = 2)

count = 1
for(i in unique(overcomp$site)){
  ordihull(mds1, group = overcomp$site, label = F,
           draw = "polygon", col = colz[count],
           show.groups = i)
  count = count + 1
}

legend("topleft",
       fill = colz,
       legend = c("Harvard Forest", "White Mountains", "Grant", "St. Hippolyte"),
       bty = "n",
       cex = 2)
title("Overstory", cex.main = 3)
?'x.lab'

rm(list = ls())

# plot MDS understory

d2 <- read.csv("understory.csv")

head(d2)

# Data cleaning
rownames(d2) = d2[,1] # move species names into rows
d2 <- d2[,-1]

head(d2)

# Analysis
# Summarizing the richness of the understory 
summary(d2)

richness <- apply(d2, 2, sum)

mds2 <- metaMDS(t(d2), try = 100) # use t() to change it so that the communities are rows, and species are columns, which is the format that vegan uses

plot(mds2) # ok, lots of scatter, good ordination

undercomp <- data.frame(mds2$points)

undercomp$s <- richness # add our species richness calculations to this data frame

undercomp$sp <- substr(rownames(undercomp), 1, 6)  

# Get the site by getting the last two characters of the undercomp rownames
undercomp$site <- unlist(
  lapply(strsplit(rownames(undercomp), "_"),
         function(x) x[[2]]))


# For each species, plot the species richness by site. Order sites by south -> north
undercomp$site <- as.factor(undercomp$site)
levels(undercomp$site) <- c(3, 1, 4, 2)
undercomp$site <- factor(as.numeric(as.character(undercomp$site)), labels = c("HF", "WM", "GR", "SH"))


# Clear differences with site, changing space along MDS1
colz = alpha(c("#E7298A", "#1B9E77", "#D95F02", "#7570B3"), 0.5)


plot(mds2, type = "n",
     xlim = c(-1.5, 1.5),
     ylim = c(-1.2, 2)
)

count = 1
for(i in unique(undercomp$site)){
  ordihull(mds2, group = undercomp$site, label =F,
           draw = "polygon", col = colz[count],
           show.groups = i)
  count = count + 1
}

legend("topleft",
       fill = colz,
       legend = c("Harvard Forest", "White Mountains", "Grant", "St. Hippolyte"),
       bty = "n",
       cex = 1.2)

title("Overstory", cex.main = 1.5)      