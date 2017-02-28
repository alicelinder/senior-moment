
## source code for Fig. 2 Basal Area vs. Competition Index

centroid <- read.csv("centroid_data.csv")
centroid$sp = c("ACEPEN", "BETPAP", "CORALT", "FAGGRA", "HAMVIR", "SORAME")
centroid <- subset(centroid, select = c("sp","minLat", "maxLat", "midLat", "minTemp", "maxTemp", "midTemp", 
                                        "minPrec", "maxPrec", "midPrec"))

focal <- read.csv("focal.species.dbh.csv")
focal$Height <- NULL
focal$DBH <- NULL

lat.long <- read.csv("DBH.Lat.Long.csv")
lat.long <- subset(lat.long, select = c("Individual", "DBH", "Height", "Lat", "Long"))

focal <- merge(lat.long, focal, by = "Individual")

focal$DBH[focal$DBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
focal$DBH[focal$DBH == "less.than.1"] = 0.5 # make all less.than.1 into .5
focal$DBH[focal$DBH == "less.than.2"] = 1 # make all <2 measures into 1

other.dbh <- read.csv("all.species.dbh.csv")

# merge datasets -- NAs appearing in CORALTs so check this
#setdiff(unique(other.dbh$Individual), unique(focal$Individual))
all.dbh <- merge(focal, other.dbh, by = "Individual", all.x = TRUE, all.y = TRUE, suffixes = c(".focal", ".other"))

#index
#all.dbh[which(is.na(all.dbh$DBH.focal) == TRUE), ]

# Clean up DBH measures
all.dbh$DBH.other <- as.character(all.dbh$DBH.other) # convert from factor to character
all.dbh$DBH.focal <- as.character(all.dbh$DBH.focal)

all.dbh$DBH.other[all.dbh$DBH.other == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$DBH.focal[all.dbh$DBH.focal == "less.than.1"] = 0.5 # make all less.than.1 into .5
all.dbh$DBH.other[all.dbh$DBH.other == "less.than.2"] = 1 # make all <2 measures into 1

all.dbh$DBH.other <- as.numeric(as.character(all.dbh$DBH.other))
all.dbh$DBH.focal <- as.numeric(as.character(all.dbh$DBH.focal))# convert from factor to numeric

#summary(all.dbh$DBH.other) # should not be any NA

# say whether focal DBH is smaller than competing individual
all.dbh["greater"] <- all.dbh$DBH.focal < all.dbh$DBH.other
#View(all.dbh)

# filter  data so only individiuals with DBHs greater than the focal DBH exist
compet <- all.dbh[which(all.dbh$DBH.other > all.dbh$DBH.focal),]

#dim(all.dbh[which(all.dbh$greater == TRUE) ,])

# separate out same species to measure intra-specific competition
compet$sp = substr(compet$Individual, 1, 6)
compet$inter.sp <- compet$Comp.Species != compet$sp
#head(compet)
inter.compet <- compet[which(compet$inter.sp == TRUE & compet$DBH.focal < compet$DBH.other),]
inter.compet$BA = .5*pi*(inter.compet$DBH.other)^2
inter.compet$fBA = .5*pi*(inter.compet$DBH.focal)^2
inter.comp.BA <- data.frame(tapply(inter.compet$BA, inter.compet$Individual, sum))

# create basal area variables of only those individuals
inter.compet$BA = .5*pi*(inter.compet$DBH.other)^2
inter.compet$fBA = .5*pi*(inter.compet$DBH.focal)^2

# sum the basal area for each individual

# This is very problematic... made a 'numeric' data frame composed of just the level values
# focal["competing.BA"] <- data.frame(tapply(compet$BA, compet$Individual, sum))
# This is what you wanted to do
# focal <- data.frame(focal, competing.BA = tapply(compet$BA, compet$Individual, sum))

sum.BA <- tapply(inter.compet$BA, inter.compet$Individual, sum)
sum.BA.df <- data.frame(Individual = names(sum.BA), sum.BA = sum.BA)
focal.ba <- merge(focal, sum.BA.df, by = "Individual", all.y = TRUE, all.x = TRUE)
focal.ba$DBH = as.numeric(as.character(focal.ba$DBH))
focal.ba$BA = .5*pi*(focal.ba$DBH)^2

focal.ba$log.cBA = log(focal.ba$sum.BA) # what are NAs from? Some have no competing BA values apparently
focal.ba$relative.BA = focal.ba$BA / focal.ba$sum.BA

# create new species variable
focal.ba$sp = substr(focal.ba$Individual, 1, 6)

# merge focal.ba with centroid data
focal.centroid <- merge(focal.ba, centroid, by = "sp", all.x = TRUE, all.y = TRUE)
# create new variable with difference between centroid and latitude of site
focal.centroid$minLatdiff = focal.centroid$Lat - focal.centroid$minLat

focal.centroid$centroiddiff = focal.centroid$Lat - focal.centroid$midLat

#Save
#save(focal.centroid, file = "Focal-Centroid.RData")

