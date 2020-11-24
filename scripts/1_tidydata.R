#### Created: Nov. 24, 2020
#### Updated: Nov. 24, 2020

### This script will be used to create the datasets used in the PRESENCE analyses (including FIRE).

######## IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

#Packages needed - modify these if you don't need em

library(reshape2)
library(rms)
library(plyr)
library(survival)
library(MuMIn)

setwd("data")

### STEP 1: Import data. In Understory_All.csv, L = Legacy and R = Resurvey

und.cover <- read.csv("Understory_All.csv", header=TRUE, na.strings="")
und.cover$Elevation.m <- as.numeric(as.character(und.cover$Elevation.m)) #gives warning - no worries

lat.long <- read.csv("Lat.Long.csv", header=TRUE, na.strings="")
plot.names <- lat.long[, c(2, 3, 6)]
names(plot.names) <- c("Plot.2015", "Plot.1980", "Elevation.m")

load("Species.List.Rda")

fires <- read.csv("All_Plots_Wildfire_Join.csv", header=TRUE, na.strings="")








