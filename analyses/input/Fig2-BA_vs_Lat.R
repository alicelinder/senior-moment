# DBH Community composition script to compare focal individuals with surrounding DBH to find competitiveness index

## July 28, 2016

setwd("~/Dropbox/Summer 2016 Forest/Data") # setwd("~/Dropbox/Work/Harvard/Summer 2016 Forest/Data") 


library(vegan) # install.packages("vegan")
library(lme4)# install.packages("lme4")
library(scales)# install.packages("scales")
library(ggplot2) # install.packages("ggplot2")
library(sjPlot) # install.packages("sjPlot")

focal.dbh <- read.csv("Individual.DBH.Test.csv")

other.dbh <- read.csv("Trees.DBH.test.csv")

# sum up other basal areas
head(other.dbh)

other.dbh$Individual <- as.character(other.dbh$Individual) # convert from factor to character
focal.dbh$Individual <- as.character(focal.dbh$Individual)

# Clean up DBH measures
other.dbh$DBH <- as.character(other.dbh$DBH) # convert from factor to character
focal.dbh$DBH <- as.character(focal.dbh$DBH)

other.dbh$DBH[other.dbh$DBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
focal.dbh$DBH[focal.dbh$DBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
other.dbh$DBH[other.dbh$DBH == "less.than.2"] = 1 # make all <2 measures into 1

other.dbh$DBH <- as.numeric(as.character(other.dbh$DBH)) # convert from factor to numeric

summary(other.dbh$DBH) # should not be any NA

# convert dbh to basal area and dbh to m from cm
other.dbh$BA <- (other.dbh$DBH/200)^2*pi
summary(other.dbh$BA)

# Summing
data.frame(tapply(other.dbh$BA, other.dbh$Individual, sum))

# focal dbh to basal area
focal.dbh$DBH <- as.numeric(as.character(focal.dbh$DBH)) # convert from factor to numeric
focal.dbh$BA <- (focal.dbh$DBH/200)^2*pi

# check any individuals that are missing
(notinfocal <- other.dbh$Individual[!other.dbh$Individual %in% focal.dbh$Individual])
(notinother <- focal.dbh$Individual[!focal.dbh$Individual %in% other.dbh$Individual])

# sort out individuals to compare DBHs
focal.dbh <- focal.dbh[order(focal.dbh$Individual),]

# add column in focal.dbh to compare BAs
focal.dbh["BAother"] <- data.frame(tapply(other.dbh$BA, other.dbh$Individual, sum))
focal.dbh["total.BA"] <- focal.dbh$BA + focal.dbh$BAother
focal.dbh["BA.Percentage"] <- focal.dbh$BA/focal.dbh$total.BA

# Site and species information based on last 2 letters of individuals
focal.dbh$Site <- unlist(
  lapply(strsplit(focal.dbh[,1], "_"),
         function(x) x[[2]]))
focal.dbh$sp <- substr(focal.dbh$Individual, 1, 6)

# looking at data summaries, species at each site across gradient
boxplot(focal.dbh$BA.Percentage ~ focal.dbh$sp)

boxplot(BA.Percentage ~ sp, focal.dbh)

boxplot(BA.Percentage ~ Site, focal.dbh)

focal.dbh$Site <- as.factor(focal.dbh$Site)
levels(focal.dbh$Site) <- c(3, 1, 4, 2)
focal.dbh$Site <- factor(as.numeric(as.character(focal.dbh$Site)), labels = c("HF", "WM", "GR", "SH"))

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

ggplot(focal.small, 
       aes(as.numeric(Site), BA.Percentage, color = sp)) + 
  #geom_smooth( se = F, aes(color = sp)) +
  geom_point()  + xlab("Site") + 
  scale_x_continuous(labels = 
                       c("HF","WM","GR","SH")) +
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


