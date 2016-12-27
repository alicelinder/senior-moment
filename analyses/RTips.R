# use stringsAsFactors and set to FALSE to prevent character columns from being converted to factor columnsre

## use facet_wrap in ggplot2 to separate out all species

## to search for something, use apropos()

## use the "apply" functions to apply functions across data frames, matrices, etc.
help(apply)

rm(list = ls())
setwd("~/GitHub/senior-moment/data")

# use apply to get species names
d <- read.csv("all.species.dbh.csv", row.names = NULL)
d["Species"] <- sapply(d$Individual, substr, 1, 6)



