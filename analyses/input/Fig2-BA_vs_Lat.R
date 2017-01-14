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
focal <- read.csv("focal.species.dbh.csv")
focal <- rename(focal, c("DBH" = "fDBH"))
lat.long <- read.csv("DBH.Lat.Long.csv")
focal <- merge(lat.long, focal, by = "Individual")
focal <- focal[,-2:-3]
focal <- focal[, -4:-10]
focal <- rename(focal, c("Height.y" = "Height"))

head(focal)

other.dbh <- read.csv("all.species.dbh.csv")
other.dbh["index"] <- 1:3893
head(other.dbh)

# merge datasets
all.dbh <- merge(focal, other.dbh, by = "Individual")

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
focal["competing.BA"] <- data.frame(tapply(compet$BA, compet$Individual, sum))
focal["log.cBA"] <- log(focal$competing.BA)
focal$log.cBA <- as.integer(as.factor(as.character(focal$log.cBA)))
focal$competing.BA <- as.integer(as.factor(as.character(focal$competing.BA)))
class(focal$log.cBA)

# Site and species information based on last 2 letters of individuals
focal$Individual <- as.character(focal$Individual)

focal$Site <- unlist(
  lapply(strsplit(focal[,1], "_"),
         function(x) x[[2]]))
focal$sp <- substr(focal$Individual, 1, 6)

class(focal$Site)
class(focal)
class(focal$log.cBA)

# looking at data summaries, species at each site across gradient
#levels(focal$Site) <- c(3, 1, 4, 2)
#focal$Site <- factor(as.numeric(focal$Site), levels = c("HF", "WM", "GR", "SH"))

coralt <- focal[focal$sp == "CORALT",]
hamvir <- focal[focal$sp == "HAMVIR",]
sorame <- focal[focal$sp == "SORAME",]
acepen <- focal[focal$sp == "ACEPEN",]
focal.small <- rbind(coralt, sorame, hamvir, acepen)
#focal.medium <- rbind(hamvir, acepen)

betpap <- focal[focal$sp == "BETPAP",]
faggra <- focal[focal$sp == "FAGGRA",]
quealb <- focal[focal$sp == "QUEALB",]
focal.large <- rbind(betpap, faggra, quealb)

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

ggplot(focal[-42,], 
       aes(as.numeric(Site), Height, color = sp)) + 
  geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
  facet_wrap(~sp, ncol = 4) +
  ylab("Height (m)") +
  ggtitle("Shifts in Performance")

# grah shows numbers (possibly due to infinite log values?)
ggplot(focal.small, 
       aes(as.numeric(Site), log.cBA, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  facet_wrap(~sp, ncol = 2) +
  ylab("Fraction of total Basal Area")

# no points appear on this graph
ggplot(focal,
       aes(as.numeric(Site), as.numeric(competing.BA), color = sp)) +
  geom_point() + xlab("Site") +
  facet_wrap(~sp, ncol=4)

ggplot(focal,
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
