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

ggplot(focal.centroid,
       aes(minLatdiff, relative.BA, color = sp)) +
  geom_point() + 
  geom_abline(0.129447941, -0.006064695, col="black", lwd=3) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Distance from Min. Latitude") +
  ylab("Relative Basal Area")




# model intraspecific competition
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "ACEPEN",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "BETPAP",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "CORALT",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "FAGGRA",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "HAMVIR",]))
#summary(lm1 <- lm(relative.BA ~ minLatdiff, data = focal.centroid[focal.centroid$sp == "SORAME",]))

focal.centroid <- focal.centroid[-which(focal.centroid$sp == "QUEALB"),]

# ignore extra large DBH for FAGGUS value
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "FAGGRA" & focal.centroid$sum.BA > 20000),] 


# corrected HAMVIR 11 value to 8.1
focal.centroid[which(focal.centroid$sp == "HAMVIR" & focal.centroid$relative.BA > 10),]
# ignore extra large DBH for HAMVIR value

lme1 <- lmer(relative.BA ~ minLatdiff + (minLatdiff | sp), data = focal.centroid)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)

ranef.df <- c("ACEPEN", -0.107918109,  0.0065249743, 
            "BETPAP",  0.381783875, -0.0230835213,
            "CORALT", -0.126421505,  0.0076437317,
            "FAGGRA", -0.017713339,  0.0010709888,
            "HAMVIR",  0.005347238, -0.0003233062,
            "SORAME", -0.135078160,  0.0081671327)

#lme1 <- lmer(relative.BA ~ Lat + (Lat | sp), data = focal.centroid)

sjt.lmer(lme1)

# plotting lines (works in base, not sure about ggplot)
myspecieslist <- unique(focal.centroid$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!

plot(focal.centroid$minLatdiff,focal.centroid$relative.BA, type="n")

for (i in c(1:length(myspecieslist))){
  subby <- subset(focal.centroid, sp==myspecieslist[i])
  points(subby$minLatdiff, subby$relative.BA, col=mycolors[i], pch=16)
  # pch is symbol shape
}
#Overall trend
abline(0.129447941, -0.006064695, col="black", lwd=3) # overall mean

#ACEPEN
abline(-0.107918109, 0.0065249743, col="#7FC97F", lwd=1.5)

#BETPAP
abline(0.381783875, -0.0230835213, col="#BEAED4", lwd=1.5)

#CORALT
abline(-0.126421505,  0.0076437317, col="#FDC086", lwd=1.5)

#FAGGRA
abline(-0.017713339,  0.0010709888, col="#FFFF99", lwd=1.5)

#HAMVIR
abline(0.005347238, -0.0003233062, col="#386CB0", lwd=1.5)

#SORAME
abline(-0.135078160,  0.0081671327, col="#F0027F", lwd=1.5)


ggplot(focal.centroid,
       aes(minLatdiff, relative.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Distance from Min. Latitude") +
  ylab("Relative Basal Area")

# model competition
#summary(lm1 <- lm(relative.BA ~ Lat, data = focal[focal$sp == "ACEPEN",]))

#fixef(lme1)
#ranef(lme1)
#summary(lme1)

#ranef <- ranef(lme1)
