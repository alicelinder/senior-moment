

source("Fig2-source.R")

# ignore extra large DBH for FAGGUS value
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "FAGGRA" & focal.centroid$sum.BA > 20000),] 

# ignore QUEALB for graphing purposes
focal.centroid <- focal.centroid[-which(focal.centroid$sp == "QUEALB"),]

#myspecieslist <- unique(focal.centroid$sp)
plot(focal.centroid$minLatdiff,focal.centroid$relative.BA, type="n", xlim <- c(7, 12), ylim <- c(-0.1,1), xlab="Distance from Min. Latitudes (degrees)", ylab="Relative Basal Area")

# subset my species list so that BETPAP and SORAME are in separate places
s.sp <- c("BETPAP", "SORAME")

s.mycolors <- rep(c("#1B9E77", "#D95F02"), 10) # need 6 really!

for (i in c(1:length(s.sp))){
  subby <- subset(focal.centroid, sp==s.sp[i])
  points(subby$minLatdiff, subby$relative.BA, col=s.mycolors[i], pch=16)
  # pch is symbol shape
}
#Overall trend
abline(-0.06413290, 0.01428613, col="black", lwd=3) # overall mean

#BETPAP
abline(-0.24810866,  0.055499823, col="#1B9E77", lwd=2)

#SORAME
abline(0.05205775, -0.011644882, col="#D95F02", lwd=2)

legend('topright', legend=c("B. papyrifera", "S. americana"), 
       lty=1, col=s.mycolors, bty='n', cex=1)


n.sp <- c("ACEPEN", "CORALT", "FAGGRA", "HAMVIR")

n.mycolors <- rep(c("#7570B3", "#E7298A", "#66A61E", "#E6AB02"), 10)

plot(focal.centroid$minLatdiff,focal.centroid$relative.BA, type="n", xlab="Distance from Min. Latitudes (degrees)", ylab="Relative Basal Area",
          xlim <- c(7, 17), ylim <- c(-0.2,0.4))

for (i in c(1:length(n.sp))){
  subby <- subset(focal.centroid, sp==n.sp[i])
  points(subby$minLatdiff, subby$relative.BA, col=n.mycolors[i], pch=16)
  # pch is symbol shape
}

#ACEPEN
abline(0.04023972, -0.009001288, col="#7570B3", lwd=2)

#CORALT
abline(0.06201265, -0.013871708, col="#E7298A", lwd=2)

#FAGGRA
abline(0.04033458, -0.009022507, col="#66A61E", lwd=2)

#HAMVIR
abline(0.05346396, -0.011959439, col="#E6AB02", lwd=2)


legend('topright', legend=c("A. pensylvanicum", "C. alternifolia", "F. grandifola", "H. virginiana"), 
       lty=1, col=n.mycolors, bty='n', cex=1)
