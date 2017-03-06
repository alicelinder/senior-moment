load("CHVols.RData")
chvols.focal <- subset(chvols.focal, select = c("sp", "site", "relative.vol"))
head(clim.focal)

clim.focal <- subset(clim.focal, select = c("Individual", "sp", "Site", "distance.to.climatic.centroid"))
clim.focal$relative.vol <- NA

clim.focal[clim.focal$Site == "HF",]$relative.vol[clim.focal$sp == "ACEPEN"] <- chvols.focal$relative.vol[chvols.focal$sp == "ACEPEN"]
head(chvols.focal)
myspecieslist <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA", "HAMVIR", "SORAME")
mysitelist <- c("HF", "WM", "GR", "SH")


for (i in c(1:length(myspecieslist))){
  for (site in c(1:length(mysitelist))){
    subby <- subset(chvols.focal, sp == myspecieslist[i], site == mysitelist[site])
    clim.focal[clim.focal$sp == i]$relative.vol[clim.focal$Site == i] <- subby
  }
  subby <- subset(clim.focal, sp==myspecieslist[i])
  
}

load("Focal-Centroid.RData")

load("Clim.Focal.RData")

focal.centroid$Site <- unlist(
  lapply(strsplit(as.character(focal.centroid$Individual), "_"),
         function(x) x[[2]]))

clim.focal$Site <- unlist(
  lapply(strsplit(as.character(clim.focal$Individual), "_"),
         function(x) x[[2]]))

names(focal.centroid)[names(focal.centroid) == "Site"] <- "site"

head(chvols.focal)
head(clim.focal)
clim.focal <- subset(clim.focal, select = c("sp", "Site", "distance.to.climatic.centroid"))
clim.chvols <- merge(chvols.focal, clim.focal, by = c("sp", "site"))

ba.chvols <- ba.chvols[-which(ba.chvols$sp == "HAMVIR"),]
ba.chvols <- ba.chvols[-which(ba.chvols$sp == "SORAME"),]


save(ba.chvols, file = "CHVols.BA.Rdata")

rm(list = ls())

load("CHVols.centroid.Rdata")


myspecieslist <- unique(ba.chvols$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 10) # need 6 really!

View(ba.chvols)

# plot in base package
plot(ba.chvols$centroiddiff, ba.chvols$relative.vol, type="n", main="Relative Convex Hull Volume across Climatic Envelope", xlab="Distance from Climatic Centroid", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(ba.chvols, sp==myspecieslist[i])
  points(subby$centroiddiff, subby$relative.vol, col=mycolors[i], pch="O")
}

# ACEPEN
abline(52.2, -15.14, col = "#1B9E77", lwd = 2)

# BETPAP
abline(41.635, 4.179, col = "#D95F02", lwd = 2)

#CORALT
abline(-25.406, 7.162, col = "#7570B3", lwd = 2)

#FAGGRA
abline(15.559, -1.973, col = "#E7298A", lwd = 2)

legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifola"), 
       lty=2, col=mycolors, bty='n', cex=1)


load("Clim.Focal.RData")

myspecieslist <- unique(clim.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.BA, type="n", main="Competitiveness Index across Climatic Envelope", xlab="Distance from Climatic Centroid", ylab="Relative Basal Area")

for (i in c(1:length(myspecieslist))){
  subby <- subset(clim.focal, sp==myspecieslist[i])
  points(subby$distance.to.climatic.centroid, subby$relative.BA, col=mycolors[i], pch=16)
  # pch is symbol shape
}



load("CHVol.Clim.RData")


myspecieslist <- unique(clim.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 10) # need 6 really!


plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.vol, type="n", xlim<-c(0,5), ylim<-c(0,60), xlab="Distance from Climatic Centroid", ylab="Relative Convex Hull Volume")

# plot in base package
plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.vol, type="n", main="Relative Convex Hull Volume across Climatic Envelope", xlab="Distance from Climatic Centroid", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(clim.focal, sp==myspecieslist[i])
  points(subby$distance.to.climatic.centroid, subby$relative.vol, col=mycolors[i], pch="O")
}





load("CHVol.Clim.RData")


myspecieslist <- unique(clim.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 10) # need 6 really!


# plot in base package
plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.vol, type="n", xlab="Distance from Climatic Centroid", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(clim.focal, sp==myspecieslist[i])
  points(subby$distance.to.climatic.centroid, subby$relative.vol, col=mycolors[i], pch="O")
}
# ACEPEN
abline(41.98, -12.81, col = "#1B9E77", lwd = 2)

# BETPAP
abline(41.36, -13.37, col = "#D95F02", lwd = 2)

#CORALT
abline(-81.39, 41.88, col = "#7570B3", lwd = 2)

#FAGGRA
abline(9.507, -1.678, col = "#E7298A", lwd = 2)

lm(clim.focal[clim.focal$sp == "FAGGRA",]$relative.vol ~ clim.focal[clim.focal$sp == "FAGGRA",]$distance.to.climatic.centroid )


load("Clim.Focal.RData")

myspecieslist <- unique(clim.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


plot(clim.focal$distance.to.climatic.centroid, clim.focal$relative.BA, type="n", main="Competitiveness Index across Climatic Envelope", xlab="Distance from Climatic Centroid", ylab="Relative Basal Area")

for (i in c(1:length(myspecieslist))){
  subby <- subset(clim.focal, sp==myspecieslist[i])
  points(subby$distance.to.climatic.centroid, subby$relative.BA, col=mycolors[i], pch=16)
  # pch is symbol shape
}

#ACEPEN
abline(-0.1990938, 0.06159846, col="#1B9E77", lwd=2)

#BETPAP
abline(0.9400260, -0.29083851, col="#D95F02", lwd=2)

#CORALT
abline(-0.1887005, 0.05838284, col="#7570B3", lwd=2)

#FAGGRA
abline(-0.2029749, 0.06279922, col="#E7298A", lwd=2)

#HAMVIR
abline(-0.1362191, 0.04214539, col="#66A61E", lwd=2)

#SORAME
abline(-0.2130377, 0.06591260, col="#E6AB02", lwd=2)

#Overall trend
abline(0.20681811, -0.05411618, col="black", lwd=3) # overall mean

legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifola", "H. virginiana", "S. americana"), 
       lty=1, col=mycolors, bty='n', cex=.75)


load("CHVols.RData")

chvols.focal <- chvols.focal[-which(chvols.focal$sp == "HAMVIR"),]
chvols.focal <- chvols.focal[-which(chvols.focal$sp == "SORAME"),]

myspecieslist <- unique(chvols.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


# plot in base package
plot(chvols.focal$lat, chvols.focal$relative.vol, type="n", main="Relative Convex Hull Volume across Latitudes", xlab="Latitude", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(chvols.focal, sp==myspecieslist[i])
  points(subby$lat, subby$relative.vol, col=mycolors[i], pch="O")
  
}

ggplot(chvols.focal,
       aes(lat, vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Convex Hull Volume") +
  ylab("Relative Basal Area")

chvols.comm$sp <- substr(rownames(chvols.comm), 1, 6)

chvols.comm$site <- as.factor(chvols.comm$site)
levels(chvols.comm$site) <- c(3, 1, 4, 2)
chvols.comm$site <- factor(as.numeric(as.character(chvols.comm$site)), labels = c("HF", "WM", "GR", "SH"))

lm(chvols.comm[chvols.comm$sp == "ACEPEN",]$chvols.comm ~ chvols.comm$site)

chvols.comm <- chvols.comm[-which(chvols.comm$sp == "HAMVIR"),]

ggplot(chvols.comm,
       aes(site, chvols.comm, color = sp)) +
  geom_point()  +
  geom_smooth(method="lm", se=F) +
  facet_wrap(~sp, ncol = 3, scales = "free") +
  xlab("Convex Hull Volume") +
  ylab("Relative Basal Area")


load("CHVols.RData")

chvols.focal <- chvols.focal[-which(chvols.focal$sp == "HAMVIR"),]
chvols.focal <- chvols.focal[-which(chvols.focal$sp == "SORAME"),]

myspecieslist <- unique(chvols.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


# plot in base package
plot(chvols.focal$lat, chvols.focal$relative.vol, type="n", main="Relative Convex Hull Volume across Latitudes", xlab="Latitude", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(chvols.focal, sp==myspecieslist[i])
  points(subby$lat, subby$relative.vol, col=mycolors[i], pch="O")
  
}

#ACEPEN
abline(638.557, -14.02, col="#1B9E77", lwd =2)

#BETPAP
abline(-175.71, 4.19, col="#D95F02", lwd = 2)

#CORALT
abline(-144.277, 3.375, col="#7570B3", lwd = 2)

#FAGGRA
abline(93.3, -2, col="#E7298A", lwd = 2)


legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifola"), 
       lty=1, col=mycolors, bty='n', cex=1)
lm(chvols.focal[chvols.focal$sp == "ACEPEN",]$relative.vol ~ chvols.focal[chvols.focal$sp == "ACEPEN",]$lat)


head(chvols.focal)
