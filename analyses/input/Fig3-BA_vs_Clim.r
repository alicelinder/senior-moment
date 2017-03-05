## Figure code for basal area and climatic range

# by Alice Linder
## written Feb. 6 2017

# DBH Community composition script to compare focal individuals with surrounding DBH to find competitiveness index

## July 28, 2016

## TO DO: scale center of basal areas to account for trend of decreasing DBHs as you increase latitude
### command for this: scale(x, center = TRUE, scale = FALSE)

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data") # For Dan

# set libraries
library(vegan) # install.packages("vegan")
library(lme4)# install.packages("lme4")
library(scales)# install.packages("scales")
library(ggplot2) # install.packages("ggplot2")
library(plyr) # install.packages("plyr")
library(reshape) # install.packages("reshape")
library(sjPlot) # install.packages("sjPlot")
# detach("package:dplyr", unload=TRUE)

# load all data from source code
source("Fig2-source.R")

clim <- read.csv("climatic_data.csv")
clim <- subset(clim, select = c("Individual", "distance.to.climatic.centroid"))

# plot intraspecific competition
# ignore extra large DBH for FAGGUS value
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "FAGGRA" & focal.centroid$sum.BA > 20000),] 

# ignore QUEALB for graphing purposes
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "QUEALB"),]

clim.focal <- merge(focal.centroid, clim, by = "Individual")

save(clim.focal, file="Clim.Focal.RData")

# ggplot(clim.focal,
#        aes(distance.to.climatic.centroid, relative.BA, color = sp)) +
#   geom_point() + 
#   geom_smooth(method="lm", se=F) +
#   facet_wrap(~sp, ncol = 3, scales = "free") +
#   xlab("Distance from Climatic Centroid") +
#   ylab("Relative Basal Area")


# Analysis. Single linear models, very simple analysis here.
# ?lme
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "ACEPEN",]))
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "BETPAP",]))
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "CORALT",]))
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "FAGGRA",]))
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "HAMVIR",]))
# summary(lm1 <- lm(relative.BA ~ distance.to.climatic.centroid, data = clim.focal[clim.focal$sp == "SORAME",]))

# Mixed effect model to use all species in single analysis
lme1 <- lmer(relative.BA ~ distance.to.climatic.centroid + (distance.to.climatic.centroid | sp), data = clim.focal)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)
sjt.lmer(lme1)



clim <- read.csv("climatic_data.csv")
clim <- subset(clim, select = c("Individual", "distance.to.climatic.centroid"))

# plot intraspecific competition
# ignore extra large DBH for FAGGUS value
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "FAGGRA" & focal.centroid$sum.BA > 20000),] 

# ignore QUEALB for graphing purposes
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "QUEALB"),]

clim.focal <- merge(focal.centroid, clim, by = "Individual")

#ggplot(clim.focal,
#aes(distance.to.climatic.centroid, relative.BA, color = sp)) +
#geom_point() + 
#geom_smooth(method="lm", se=F) +
#facet_wrap(~sp, ncol = 3, scales = "free") +
#xlab("Distance from Climatic Centroid") +
#ylab("Relative Basal Area")

myspecieslist <- unique(clim.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!

plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.BA, type="n", main="Competitiveness Index across Climatic Envelope", xlab="Distance to Climatic Centroid", ylab="Relative Basal Area")

for (i in c(1:length(myspecieslist))){
  subby <- subset(clim.focal, sp==myspecieslist[i])
  points(subby$distance.to.climatic.centroid, subby$relative.BA, col=mycolors[i], pch=16)
  # pch is symbol shape
}
#Overall trend
abline(0.09936796, -0.01446959 , col="black", lwd=3) # overall mean

#ACEPEN
abline(-0.08582783, 0.019607063, col="#1B9E77", lwd=2)

#BETPAP
abline(0.40367686, -0.092218542, col="#D95F02", lwd=2)

#CORALT
abline(-0.11111040, 0.025382776, col="#7570B3", lwd=2)

#FAGGRA
abline(-0.06564860, 0.014997190, col="#E7298A", lwd=2)

#HAMVIR
abline(-0.04179071, 0.009546939, col="#66A61E", lwd=2)

#SORAME
abline(-0.09929931, 0.022684574, col="#E6AB02", lwd=2)
?legend
legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifola", "H. virginiana", "S. americana"), 
       lty=1, col=mycolors, bty='n', cex=.75)

load("BA-CHVols.RData")

