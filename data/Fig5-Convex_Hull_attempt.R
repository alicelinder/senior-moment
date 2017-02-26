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
