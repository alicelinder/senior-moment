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

#library(sjPlot) # install.packages("sjPlot")
# detach("package:dplyr", unload=TRUE)

# load all data from source code
#source("Fig2-source.R")


# plot intraspecific competition
ggplot(focal,
       aes(Lat, intra.comp.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)

# model intraspecific competition
summary(lm1 <- lm(intra.comp.BA ~ Lat, data = focal[focal$sp == "ACEPEN",]))
summary(lm2 <- lm(intra.comp.BA ~ Lat, data = focal[focal$sp == "BETPAP",]))
lme1 <- lmer(intra.comp.BA ~ Lat + (Lat | sp), data = focal)

# graph based on latitude
ggplot(focal,
       aes(Lat, competing.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)

ggplot(focal,
       aes(Lat, log.cBA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)

# Why excluding this individual Too large?
ggplot(focal[-42,], 
       aes(Site, Height, color = sp)) + 
  geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 4) +
  ylab("Height (m)") +
  ggtitle("Shifts in Performance")

# grah shows numbers (possibly due to infinite log values?)
# not sure what you are expecting, this looks correct now

ggplot(focal.small, 
       aes(Site, log.cBA, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area")

# no points appear on this graph
# Site was not a factor previously, so as.numeric just makes a vector of NA -- can turn into a factor and then numeric, but not sure why you want this!
ggplot(focal,
       aes(as.numeric(as.factor(Site)), as.numeric(competing.BA), color = sp)) +
  geom_point() + xlab("Site") +
  facet_wrap(~sp, ncol=4)

# Better, just use the factor Site
ggplot(focal,
       aes(Site, log.cBA, color = sp)) +
         geom_point() + xlab("Site") +
         facet_wrap(~sp, ncol=4)

ggplot(focal.small, 
       aes(Site, log.cBA, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  #scale_x_continuous(labels = 
                       #c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area") 
#ggtitle("Dominance of Small Woody Species")

ggplot(focal.large, 
       aes(Site, BA.Percentage, color = sp)) +  
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area") 
#ggtitle("Dominance of Large Woody Species")

# relationship between height and BA.percentage
## do it for 4 different sites for each species

ggplot(focal,
       aes(Site, Height, color = sp)) +
  geom_point() + 
  # geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)
which(focal$Height>100)
ggplot(focal[-42,], 
       aes(as.numeric(Site), Height, color = sp)) + 
  geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 4) +
  ylab("Height (m)") +
  ggtitle("Shifts in Performance")

# Analysis. Single linear models, very simple analysis here.

summary(lm1 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "ACEPEN",]))
summary(lm2 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "BETPAP",]))
summary(lm3 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "CORALT",]))
summary(lm4 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "FAGGRA",]))
summary(lm5 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "HAMVIR",]))
summary(lm6 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "QUEALB",]))
summary(lm7 <- lm(log.cBA ~ Lat, data = focal[focal$sp == "SORAME",]))


# Mixed effect model to use all species in single analysis
lme1 <- lmer(competing.BA ~ Lat + (Lat | sp), data = focal)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)
