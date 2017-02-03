# DBH Community composition script to compare focal individuals with surrounding DBH to find competitiveness index

## July 28, 2016

rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

# setwd("~/Documents/git/senior-moment/data") # For Dan

library(vegan) # install.packages("vegan")
library(lme4)# install.packages("lme4")
library(scales)# install.packages("scales")
library(ggplot2) # install.packages("ggplot2")
library(plyr)
library(reshape)
#library(sjPlot) # install.packages("sjPlot")
detach("package:dplyr", unload=TRUE)

# load data from centroid and latitudinal data with focal individuals

centroid <- read.csv("centroid_data.csv")
centroid$sp = c("ACEPEN", "BETPAP", "CORALT", "FAGGRA", "HAMVIR")
centroid <- subset(centroid, select = c("sp","minLat", "maxLat", "midLat", "minTemp", "maxTemp", "midTemp", 
                       "minPrec", "maxPrec", "midPrec"))

focal <- read.csv("focal.species.dbh.csv")
names(focal)
focal$Height <- NULL
focal$DBH <- NULL

lat.long <- read.csv("DBH.Lat.Long.csv")
lat.long <- subset(lat.long, select = c("Individual", "DBH", "Height", "Lat", "Long"))

focal <- merge(lat.long, focal, by = "Individual")
names(focal)

other.dbh <- read.csv("all.species.dbh.csv")

# merge datasets -- NAs appearing in CORALTs so check this
setdiff(unique(other.dbh$Individual), unique(focal$Individual))
all.dbh <- merge(focal, other.dbh, by = "Individual", all.x = TRUE, all.y = TRUE, suffixes = c(".focal", ".other"))

head(all.dbh)

#index
all.dbh[which(is.na(all.dbh$DBH.focal) == TRUE), ]

# Clean up DBH measures
all.dbh$DBH.other <- as.character(all.dbh$DBH.other) # convert from factor to character
all.dbh$DBH.focal <- as.character(all.dbh$DBH.focal)

all.dbh$DBH.other[all.dbh$DBH.other == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$DBH.focal[all.dbh$DBH.focal == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$DBH.other[all.dbh$DBH.other == "less.than.2"] = 1 # make all <2 measures into 1

all.dbh$DBH.other <- as.numeric(as.character(all.dbh$DBH.other))
all.dbh$DBH.focal <- as.numeric(as.character(all.dbh$DBH.focal))# convert from factor to numeric

summary(all.dbh$DBH.other) # should not be any NA

# say whether focal DBH is smaller than competing individual
all.dbh["greater"] <- all.dbh$DBH.focal < all.dbh$DBH.other
#View(all.dbh)

# filter  data so only individiuals with DBHs greater than the focal DBH exist
compet <- all.dbh[which(all.dbh$DBH.other > all.dbh$DBH.focal),]

dim(all.dbh[which(all.dbh$greater == TRUE) ,])

# separate out same species to measure intra-specific competition
compet$sp = substr(compet$Individual, 1, 6)
compet$intra.sp <- compet$Comp.Species == compet$sp
head(compet)
intra.compet <- compet[which(compet$intra.sp == TRUE & compet$DBH.focal < compet$DBH.other),]
intra.compet$BA = .5*pi*(intra.compet$DBH.other)^2
intra.compet$fBA = .5*pi*(intra.compet$DBH.focal)^2
intra.comp.BA <- data.frame(tapply(intra.compet$BA, intra.compet$Individual, sum))

# create basal area variables of only those individuals
compet$BA = .5*pi*(compet$DBH.other)^2
compet$fBA = .5*pi*(compet$DBH.focal)^2

# sum the basal area for each individual

# This is very problematic... made a 'numeric' data frame composed of just the level values
# focal["competing.BA"] <- data.frame(tapply(compet$BA, compet$Individual, sum))
# This is what you wanted to do
# focal <- data.frame(focal, competing.BA = tapply(compet$BA, compet$Individual, sum))

sum.BA <- tapply(compet$BA, compet$Individual, sum)
sum.BA.df <- data.frame(Individual = names(sum.BA), sum.BA = sum.BA)
focal.ba <- merge(focal, sum.BA.df, by = "Individual", all.y = TRUE, all.x = TRUE)
head(focal.ba)

focal.ba$log.cBA <- log(focal.ba$sum.BA) # what are NAs from? Some have no competing BA values apparently

head(focal.ba)

# create new species variable
focal.ba$sp = substr(focal.ba$Individual, 1, 6)

# merge focal.ba with centroid data
head(centroid)

coralt <- focal.ba[focal.ba$sp == "CORALT",]
hamvir <- focal.ba[focal.ba$sp == "HAMVIR",]
sorame <- focal.ba[focal.ba$sp == "SORAME",]
acepen <- focal.ba[focal.ba$sp == "ACEPEN",]
focal.small <- rbind(coralt, sorame, hamvir, acepen)

betpap <- focal.ba[focal.ba$sp == "BETPAP",]
faggra <- focal.ba[focal.ba$sp == "FAGGRA",]
quealb <- focal.ba[focal$sp == "QUEALB",]
focal.large <- rbind(betpap, faggra, quealb)



# plot competition based on centroid
ggplot(focal.ba,
       aes(, intra.comp.BA, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 4)

### earlier version of figure without centroid data

# Site and species information based on last 2 letters of individuals

focal$Site <- unlist(
  lapply(strsplit(focal[,1], "_"),
         function(x) x[[2]]))
focal$sp <- substr(focal$Individual, 1, 6)

typeof(focal$Site)
typeof(focal)
typeof(focal$log.cBA)

# Better is to leave it as a factor, and re-order in a logical way
focal$Site <- factor(focal$Site, labels = c("3GR", "1HF", "4SH", "2WM"))
focal$Site <- as.factor(as.character(focal$Site))
levels(focal$Site) = c("HF", "WM", "GR", "SH")

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
