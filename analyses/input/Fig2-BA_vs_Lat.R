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
library(plyr)
library(reshape)

library(sjPlot) # install.packages("sjPlot")

# load all data from source code
source("Fig2-source.R")



ggplot(focal.centroid,
       aes(minLatdiff, relative.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Distance from Min. Latitude") +
  ylab("Relative Basal Area")

focal.centroid$relative.BA
minLatdiff
# model intraspecific competition
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "ACEPEN",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "BETPAP",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "CORALT",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "FAGGRA",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "HAMVIR",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "SORAME",]))

lme1 <- lmer(relative.BA ~ minLatdiff + (minLatdiff | sp), data = focal.centroid)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)

lme1 <- lmer(relative.BA ~ Lat + (Lat | sp), data = focal)

sjt.lmer(lme1)

# plotting lines (works in base, not sure about ggplot)
myspecieslist <- unique(focal.centroid$sp)
mycolors <- rep(c("green", "red", "blue"), 10) # need 6 really!

plot(focal.centroid$minLatdiff,focal.centroid$relative.BA, type="n")

for (i in c(1:length(myspecieslist))){
  subby <- subset(focal.centroid, sp==myspecieslist[i])
  points(subby$minLatdiff, subby$relative.BA, col=mycolors[i], pch=16)
  # pch is symbol shape
}

abline(0.129447941, -0.006064695, col="black", lwd=3) # overall mean
abline(-0.107918109, 0.0065249743, col="green", lwd=1) # here's ACEPEN, change for each ranef (you'll need 6 total)


# model competition
#summary(lm1 <- lm(relative.BA ~ Lat, data = focal[focal$sp == "ACEPEN",]))

#fixef(lme1)
#ranef(lme1)
#summary(lme1)

#ranef <- ranef(lme1)
