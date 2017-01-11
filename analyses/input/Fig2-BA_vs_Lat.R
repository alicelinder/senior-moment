# DBH Community composition script to compare focal individuals with surrounding DBH to find competitiveness index

## July 28, 2016

rm(list = ls())
setwd("~/GitHub/senior-moment/data")


library(vegan) # install.packages("vegan")
library(lme4)# install.packages("lme4")
library(scales)# install.packages("scales")
library(ggplot2) # install.packages("ggplot2")
library(plyr)
library(reshape)
#library(sjPlot) # install.packages("sjPlot")
detach("package:dplyr", unload=TRUE)
focal.dbh <- read.csv("focal.species.dbh.csv")
focal.dbh <- rename(focal.dbh, c("DBH" = "fDBH"))

head(focal.dbh)

other.dbh <- read.csv("all.species.dbh.csv")
other.dbh["index"] <- 1:3893
head(other.dbh)

# merge datasets
all.dbh <- merge(focal.dbh, other.dbh, by = "Individual")

head(all.dbh)

# Clean up DBH measures
all.dbh$DBH <- as.character(all.dbh$DBH) # convert from factor to character
all.dbh$fDBH <- as.character(all.dbh$fDBH)

all.dbh$DBH[all.dbh$DBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$fDBH[all.dbh$fDBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$DBH[all.dbh$DBH == "less.than.2"] = 1 # make all <2 measures into 1

all.dbh$DBH <- as.numeric(as.character(all.dbh$DBH))
all.dbh$fDBH <- as.numeric(as.character(all.dbh$fDBH))# convert from factor to numeric

summary(all.dbh$DBH) # should not be any NA
#View(all.dbh)

# say whether focal DBH is smaller than competing individual
all.dbh$greater <- all.dbh$fDBH < all.dbh$DBH
all.dbh <- as.data.frame(all.dbh)
#View(all.dbh)

# filter  data so only individiuals with DBHs greater than the focal DBH exist
library(dplyr)
compet <- filter(all.dbh, DBH > fDBH)

# create basal area variables of only those individuals
compet["BA"] <- .5*pi*(compet$DBH)^2
compet["fBA"] <- .5*pi*(compet$fDBH)^2


# sum the basal area for each individual
focal.dbh["competing.BA"] <- data.frame(tapply(compet$BA, compet$Individual, sum))
focal.dbh["log.cBA"] <- log(focal.dbh$competing.BA)
focal.dbh$log.cBA <- as.integer(as.factor(as.character(focal.dbh$log.cBA)))
focal.dbh$competing.BA <- as.integer(as.factor(as.character(focal.dbh$competing.BA)))
class(focal.dbh$log.cBA)

# Site and species information based on last 2 letters of individuals
focal.dbh$Individual <- as.character(focal.dbh$Individual)

focal.dbh$Site <- unlist(
  lapply(strsplit(focal.dbh[,1], "_"),
         function(x) x[[2]]))
focal.dbh$sp <- substr(focal.dbh$Individual, 1, 6)

class(focal.dbh$Site)
class(focal.dbh)
class(focal.dbh$log.cBA)

# looking at data summaries, species at each site across gradient
#levels(focal.dbh$Site) <- c(3, 1, 4, 2)
#focal.dbh$Site <- factor(as.numeric(focal.dbh$Site), levels = c("HF", "WM", "GR", "SH"))

coralt <- focal.dbh[focal.dbh$sp == "CORALT",]
hamvir <- focal.dbh[focal.dbh$sp == "HAMVIR",]
sorame <- focal.dbh[focal.dbh$sp == "SORAME",]
acepen <- focal.dbh[focal.dbh$sp == "ACEPEN",]
focal.small <- rbind(coralt, sorame, hamvir, acepen)
#focal.medium <- rbind(hamvir, acepen)

betpap <- focal.dbh[focal.dbh$sp == "BETPAP",]
faggra <- focal.dbh[focal.dbh$sp == "FAGGRA",]
quealb <- focal.dbh[focal.dbh$sp == "QUEALB",]
focal.large <- rbind(betpap, faggra, quealb)

# grah shows numbers (possibly due to infinite log values?)
ggplot(focal.small, 
       aes(as.numeric(Site), log.cBA, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area")

# no points appear on this graph
ggplot(focal.dbh,
       aes(as.numeric(Site), as.numeric(competing.BA), color = sp)) +
  geom_point() + xlab("Site") +
  facet_wrap(~sp, ncol=4)

ggplot(focal.dbh,
       aes(as.numeric(Site), log.cBA, color = sp)) +
         geom_point() + xlab("Site") +
         facet_wrap(~sp, ncol=4)

ggplot(focal.small, 
       aes(as.numeric(Site), log.cBA, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  #scale_x_continuous(labels = 
                       #c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area") 
#ggtitle("Dominance of Small Woody Species")

ggplot(focal.large, 
       aes(as.numeric(Site), BA.Percentage, color = sp)) +  
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area") 
#ggtitle("Dominance of Large Woody Species")

# relationship between height and BA.percentage
## do it for 4 different sites for each species

ggplot(focal.dbh,
       aes(Site, Height, color = sp)) +
  geom_point() + 
  # geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)
which(focal.dbh$Height>100)
ggplot(focal.dbh[-42,], 
       aes(as.numeric(Site), Height, color = sp)) + 
  geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 4) +
  ylab("Height (m)") +
  ggtitle("Shifts in Performance")

# Analysis. Single linear models, very simple analysis here.

summary(lm1 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "ACEPEN",]))

summary(lm2 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "BETPAP",]))

summary(lm3 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "CORALT",]))

summary(lm4 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "FAGGRA",])) 

summary(lm5 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "HAMVIR",]))

summary(lm6 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "SORAME",]))

# Mixed effect model to use all species in single analysis
lme1 <- lmer(BA.Percentage ~ as.numeric(Site) + (as.numeric(Site) | sp), data = focal.dbh)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)

# There is no overall effect, because species are doing different things: Notably, Bet pap becomes proprotionally larger with increasing latitude (towards the center of its distribution, away from edge) while all others at multiple sites become proportionally larger members of their communities with decreasing latitudes (away from edge).

# height analysis

summary(lm7 <- lm(Height ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "ACEPEN",]))

summary(lm8 <- lm(Height ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "BETPAP",]))

summary(lm9 <- lm(Height ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "CORALT",]))

summary(lm10 <- lm(Height ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "FAGGRA",])) 

summary(lm11 <- lm(Height ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "HAMVIR",]))

summary(lm12 <- lm(BA.Percentage ~ as.numeric(Site), data = focal.dbh[focal.dbh$sp == "SORAME",]))


lme2 <- lmer(Height ~  as.numeric(Site) +  (1 | sp), data = focal.dbh)

# as factor, can see SH is really different from other sites
lme2 <- lmer(Height ~  as.numeric(Site) + (as.numeric(Site) | sp), data = focal.dbh[!is.na(focal.dbh$Height),])

fixef(lme2)
ranef(lme2)
summary(lme2)


