#rm(list = ls())
#options(stringsAsFactors = FALSE)
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")
#rm(list = ls())

m.wm <- read.csv("m.wm.csv")




d.wm <- d.wm[,colSums(d.wm) != 0]
colSums(d.wm)

sp.t.wm <- colSums(d.wm) != 0
d.wm <- d.wm[,sp.t.wm]
d.wm.sp <- colnames(d.wm)
indata.wm <- m.wm$Species[m.wm$Species %in% d.wm.sp]

rownames(m.wm) <- m.wm$Species
d.wm <- d.wm[,colnames(d.wm) %in% indata.wm]
m.wm <- m.wm[rownames(m.wm) %in% indata.wm,]

chvols.comm.wm <- dbFD(m.wm[3:6], d.wm, corr = 'none')$FRic


chvols[chvols$site == "WM",]$vol/(chvols.mean[chvols.mean$Site == "WM", chvols.mean$x])






load("CHVols.RData")
#chvols.focal <- filter(chvols, sp == "ACEPEN" | sp == "BETPAP" | sp == "CORALT" | sp == "FAGGRA" | sp == "HAMVIR" | sp == "SORAME")

# ggplot(chvols.focal,
#        aes(lat, relative.vol, color = sp)) +
#   geom_point() + 
#   geom_smooth(method="lm", se=F) +
#   facet_wrap(~sp, ncol = 3) +
#   xlab("Latitude") +
#   ylab("Relative Convex Hull Volume")


myspecieslist <- unique(chvols.focal$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


# plot in base package
plot(chvols.focal$lat, chvols.focal$relative.vol, type="n", main="Relative Convex Hull Volume across Latitudes", xlab="Latitude", ylab="Relative Convex Hull Volume")

for (i in c(1:length(myspecieslist))){
  subby <- subset(chvols.focal, sp==myspecieslist[i])
  points(subby$lat, subby$relative.vol, col=mycolors[i], pch=16)
  # pch is symbol shape
}

# rescale the volume
chvols.focal$scaled.relative.vol <- scale(chvols.focal$relative.vol, center=TRUE)
hist(chvols.focal$relative.vol, breaks=30)

# How much data?
range(chvols.focal$relative.vol, na.rm=TRUE)
chvols.nona <- subset(chvols.focal, is.na(relative.vol)==FALSE)
table(chvols.nona$sp)

# So, only use the species with more than one data point
spwithsomedata <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA")
chvols.focal.formodel <- chvols.focal[which(chvols.focal$sp %in% spwithsomedata),]

# plotting linear mixed effects model
lme1 <- lmer(scaled.relative.vol~ lat + (lat| sp), data = chvols.focal.formodel)

fixef(lme1)
ranef(lme1)
summary(lme1)

ranef <- ranef(lme1)
sjt.lmer(lme1)

mod.here <- lm(scaled.relative.vol~ lat, data = subset(chvols.focal, sp=="CORALT"))

anova(mod.here)
summary(mod.here)

load("CHVols.RData")
load("Focal-Centroid.RData")

focal.centroid$Site <- unlist(
  lapply(strsplit(focal.centroid$Individual, "_"),
         function(x) x[[2]]))

names(focal.centroid)[names(focal.centroid) == "Site"] <- "site"

ba.chvols <- merge(chvols.focal, focal.centroid, by = c("sp", "site"))


myspecieslist <- unique(ba.chvols$sp)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!


# plot in base package

for (i in c(1:length(myspecieslist))){
  subby <- subset(ba.chvols, sp==myspecieslist[i])
  points(subby$centroiddiff, subby$relative.vol, col=mycolors[i], pch=16)
  
}

hist(chvols$vol, breaks = 30)
chvols[which(chvols$vol > 100),]

# scale volumes
chvols.focal$scaled.vol <- scale(chvols.focal$vol, center=TRUE)
hist(chvols.focal$scaled.vol, breaks=30)

spwithsomedata <- c("ACEPEN", "BETPAP", "CORALT", "FAGGRA")
chvols.focal <- chvols.focal[which(chvols.focal$sp %in% spwithsomedata),]


ggplot(chvols.focal,
       aes(lat, scaled.vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Site (not ordered)") +
  ylab("Convex Hull of individual plots")

ggplot(chvols.focal.num,
       aes(lat, vol, color = sp)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  xlab("Latitude") +
  ylab("Convex Hull of Focal Species")