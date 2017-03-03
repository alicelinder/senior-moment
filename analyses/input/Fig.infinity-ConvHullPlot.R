# trying to plot 3D/2D convex hull
## run Fig 4 Convex Hull source before this
rm(list = ls())
#options(stringsAsFactors = FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/senior-moment/data")

library(vegan)
library(dplyr)
library(tidyr)
library(reshape)
library(plyr)
library(reshape2)
library(ggplot2)

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(mydata) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS",	type="n")
text(x, y, labels = row.names(mydata), cex=.7)



load("Species-Traits.RData")
tr <- tree.traits.focal
cols <- c("Individual", "SLA", "DBH", "Stem.density", "c.n")
traits.of.interest <- c("SLA", "DBH", "Stem.density", "c.n")

# use only complete cases and select only cols
tr <- tr[,cols]
ex <- tr[complete.cases(tr),]

tr = subset(ex[,traits.of.interest])
rownames(tr) = ex$Individual

fir <- cmdscale(tr, eig = TRUE, k=4)

# log transform -- IS THIS NECESSARY?
log.tr <- log(ex[, 3:6])

# creat principal components analysis with just species of interest
tr.pca <- prcomp(log.tr,
                 center = TRUE,
                 scale. = TRUE)

tr.pca <- prcomp(ex[, 3:6],
                 center = TRUE,
                 scale. = TRUE)

biplot(tr.pca)

print(tr.pca)
summary(tr.pca)

# find min and max for plotting the traits on same scales
ex$PC1 <- tr.pca$x[,1]
min.x <- min(ex$PC1)
max.x <- max(ex$PC1)
xlim <- c(min.x,max.x)
min.y <- min(ex$PC2)
max.y <- max(ex$PC2)
ylim <- c(min.y,max.y)


ex$PC2 <- tr.pca$x[,2]
head(ex)

# subset out each species at each site
myspecieslist <- unique(ex$Species)
mycolors <- rep(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10) # need 6 really!

plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")


# plot all species on the same plot
for (i in c(1:length(myspecieslist))){
  subby <- subset(ex, Species==myspecieslist[i])
  points(subby$PC1, subby$PC2, col=mycolors[i], pch=16)
  # pch is symbol shape
}

legend('topright', legend=c("A. pensylvanicum", "B. papyrifera", "C. alternifolia", "F. grandifolia", "H. virginiana", "S. americana"), 
       lty=1, col=mycolors, bty='n', cex=1)

site.colors <- rep(c("#E7298A", "#1B9E77", "#D95F02", "#7570B3"), 10)

# plot each species individually 
plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")

site.list <- c("HF", "WM", "GR", "SH")

# make coordinates -- ACEPEN HF ----------------------------------------------------------------------------------
coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- ACEPEN WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- ACEPEN GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- ACEPEN SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)
title("A. pensylvanicum")

#colz = alpha(c("#E7298A", "#1B9E77", "#D95F02", "#7570B3"), 0.5) --------------------------------------------------
plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")
# make coordinates -- BETPAP HF


coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "BETPAP"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "BETPAP"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- ACEPEN WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "BETPAP"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "BETPAP"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- ACEPEN GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- ACEPEN SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "BETPAP"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "BETPAP"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)
title("B. papyrifera")


#------------------------------------------------------------------------------------------------------------------------------------------------------

plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")
# make coordinates -- BETPAP HF


coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- ACEPEN WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- ACEPEN GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "ACEPEN"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "ACEPEN"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- ACEPEN SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)

title("C. alternifolia")

#------------------------------------------------------------------------------------------------------------------------------------------------------

plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")
# make coordinates -- BETPAP HF


coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- CORALT WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- CORALT GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- CORALT SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "CORALT"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "CORALT"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)
title("C. alternifolia")

#------------------------------------------------------------------------------------------------------------------------------------------------------

plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")
# make coordinates -- BETPAP HF


coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "FAGGRA"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "FAGGRA"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- FAGGRA WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "FAGGRA"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "FAGGRA"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- FAGGRA GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "FAGGRA"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "FAGGRA"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- FAGGRA SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "FAGGRA"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "FAGGRA"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)
title("F. grandifolia")


#------------------------------------------------------------------------------------------------------------------------------------------------------

plot(ex$PC1, ex$PC2, type="n", xlab="PC1", ylab="PC2")
# make coordinates -- BETPAP HF


coord <- c(ex[ex$Site=="HF",]$PC1[ex$Species == "SORAME"])
coord2 <- c(ex[ex$Site=="HF",]$PC2[ex$Species == "SORAME"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#E7298A"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#E7298A"))

# Make coordinates -- SORAME WM

coord <- c(ex[ex$Site=="WM",]$PC1[ex$Species == "SORAME"])
coord2 <- c(ex[ex$Site=="WM",]$PC2[ex$Species == "SORAME"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#1B9E77"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#1B9E77"))

# Make coordinates -- SORAME GR

coord <- c(ex[ex$Site=="GR",]$PC1[ex$Species == "SORAME"])
coord2 <- c(ex[ex$Site=="GR",]$PC2[ex$Species == "SORAME"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#D95F02"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#D95F02"))

# Make coordinates -- SORAME SH

coord <- c(ex[ex$Site=="SH",]$PC1[ex$Species == "SORAME"])
coord2 <- c(ex[ex$Site=="SH",]$PC2[ex$Species == "SORAME"])
coords <- cbind(coord, coord2)
coords <- coords[complete.cases(coords),]
hpts <- chull(coords)

points(coords, cex = .5, col=rep("#7570B3"))
hpts <- chull(coords)
hpts <- c(hpts, hpts[1])
lines(coords[hpts, ], col=rep("#7570B3"))

legend('topright', legend=c("Harvard Forest", "White Mountains", "Dartmouth Grant", "St. Hippolyte"), 
       lty=1, col=site.colors, bty='n', cex=1)
title("S. americana")

