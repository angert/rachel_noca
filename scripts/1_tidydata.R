#### Created: Nov. 24, 2020
#### Updated: Dec. 1, 2020

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

### STEP 2: Removing any plots not suitable for analysis
#List of plots to be removed and reasons why:
# --> HB5144 (5144): Latitude/longitude was not recorded.
# --> Dia4 (1004): Data collected at plot not found - hard copy may have been lost.
# --> Supp2026, Supp5127, and ROSS4001REF: Supplemental plots taken for future reference. Ignore corresponding 1980 plot name; this was the closest plot.
# --> Thor223 (4044), Bak494 (8017): History of logging.

### STEP 3: Adding fires as a covariate. See old script (NOCA_Understory_Fire_Analysis_2020_PRESENCE_ONLY.R) to add fire as a 3-level variable ("Unburned", "Burned before 1983", "Burned after 1983")

#Create new variable, fire.cat, identifying plots burned > 1983
fires$fire.cat <- ifelse(fires$CAL_YEAR >= 1983, "Burned", "Unburned")
fires[is.na(fires$fire.cat) == TRUE, ]$fire.cat <- paste(rep("Unburned", times=length(fires[is.na(fires$fire.cat)==TRUE,6]))) # Any NAs are from plots that are not burned

#Adding prescribed burns. See column Prescribed.burn.year
fires$fire.cat[c(39,48,49,59,60)] <- paste(rep("Burned", times=5))
names(fires)[2] <- paste("Plot.2015")

#Fixing naming errors
fires[fires$Plot.2015 == "Thor225-m", 2] <- paste("Thor225")

#Adding missing data
fires[nrow(fires) + 1, ] <- c(2014, "Thor221", NA, NA, NA, "Unburned", rep(NA, times = length(fires) - 6))

#Preparing to merge with list of 2015 plot names
fires.covariate <- fires[,c(2,6)]
names.fires <- merge(fires.covariate, plot.names, by="Plot.2015", all.y=TRUE)




#haven't fixed past this point

#Adding plots that missing from All_Plots_Wildfire_Join.csv but are present in Understory_all.csv
names.fires[c(373:378),2]<-paste(c("Unburned","Unburned","After 1983","Unburned","Unburned","Unburned"))
list.fires<-melt(names.fires, id.vars=c("Elevation.m", "CAL_YEAR"), measure.vars=c("Plot.2015", "Plot.1980"))
names(list.fires)<-c("Elevation.m", "Fires","Plot.Year", "Plot")
list.fires.nodup<-list.fires[!duplicated(list.fires$Plot),] #getting rid of duplicates
und.cover.fires<-merge(und.cover, list.fires.nodup, by="Plot")

#2020 update: change fire to 2-level variable (after 1983 only)
und.cover.fires$Fires <- ifelse(und.cover.fires$Fires == "After 1983", "Burned", "Unburned")







