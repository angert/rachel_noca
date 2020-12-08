# Created: Nov. 24, 2020
# Updated: Dec. 8, 2020

# This script will be used to create the datasets used in the PRESENCE analyses (including FIRE).

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(reshape2)
library(rms)
library(plyr)
library(survival) #Possibly don't need this one
library(MuMIn)

setwd("data")



#### STEP 1: Import data ####

und.cover <- read.csv("Understory_All.csv", header=TRUE, na.strings="") # L = Legacy and R = Resurvey
und.cover$Elevation.m <- as.numeric(as.character(und.cover$Elevation.m)) #gives warning - no worries

lat.long <- read.csv("Lat.Long.csv", header=TRUE, na.strings="")
plot.names <- lat.long[, c(2, 3, 6)]
names(plot.names) <- c("Plot.2015", "Plot.1980", "Elevation.m")

load("Species.List.Rda")

fires <- read.csv("All_Plots_Wildfire_Join.csv", header=TRUE, na.strings="")




#### STEP 2: Plot-related corrections (removals, edits, and additions) ####

# List of datasets to be modified: 
# --> fires
# --> plot.names
# --> und.cover

# List of plots to be removed and reasons why (NB - not present in every dataset):
# --> HB5144 (5144): Latitude/longitude was not recorded
# --> Dia4 (1004): Data collected at plot not found - hard copy may have been lost.
# --> Supp2026, Supp5127, and ROSS4001REF: Supplemental plots taken for future reference. Ignore corresponding 1980 plot name; this was the closest plot.
# --> Thor223 (4044), Bak494 (8017): History of logging.

nrow(fires) #Should be 373 before, 368 after --> which 3 are missing?
nrow(plot.names) #Should be 378 before, 371 after
nrow(und.cover) #Should be 6803 before, 6696 after

fires <- fires[!fires$Name == "Supp2026" & !fires$Name == "Supp5127" & !fires$Name == "ROSS4001REF" & !fires$Name == "Thor223" & !fires$Name == "Bak494", ]
plot.names <- plot.names[!plot.names$Plot.2015 == "HB5144" & !plot.names$Plot.2015 == "Dia4" & !plot.names$Plot.2015 == "Supp2026" & !plot.names$Plot.2015 == "Supp5127" & !plot.names$Plot.2015 == "ROSS4001REF" & !plot.names$Plot.2015 == "Thor223" & !plot.names$Plot.2015 == "Bak494", ]
und.cover <- und.cover[!und.cover$Plot == "HB5144" & !und.cover$Plot == "5144" & !und.cover$Plot == "1004" & !und.cover$Plot == "Supp2026" & !und.cover$Plot == "Supp5127" & !und.cover$Plot == "ROSS4001REF" & !und.cover$Plot == "Thor223" & !und.cover$Plot == "4044" & !und.cover$Plot == "Bak494" & !und.cover$Plot == "8017", ]

und.presence<-und.presence[!und.presence$Plot=="Thor223" & !und.presence$Plot=="8017" & !und.presence$Plot=="4044" & !und.presence$Plot=="Bak494",] 

# List of plots to be renamed:
# --> Change "Thor225-m" to "Thor225" in fires dataset

fires[fires$Name == "Thor225-m", 2] <- paste("Thor225")

# List of plots to be added:
# --> Thor221 added to fires dataset
fires[nrow(fires) + 1, ] <- c(2014, "Thor221", NA, NA, NA, "Unburned", rep(NA, times = length(fires) - 6))

nrow(fires) #Should be 373 before, 368 after --> which 3 are missing?
nrow(plot.names) #Should be 378 before, 371 after
nrow(und.cover) #Should be 6803 before, 6696 after

### STEP 3: Adding fires as a covariate. See old script (NOCA_Understory_Fire_Analysis_2020_PRESENCE_ONLY.R) to add fire as a 3-level variable ("Unburned", "Burned before 1983", "Burned after 1983")

#Create new variable, fire.cat, identifying plots burned > 1983
fires$fire.cat <- ifelse(fires$CAL_YEAR >= 1983, "Burned", "Unburned")
fires[is.na(fires$fire.cat) == TRUE, ]$fire.cat <- paste(rep("Unburned", times=length(fires[is.na(fires$fire.cat)==TRUE,6]))) # Any NAs are from plots that are not burned

#Adding prescribed burns. See column Prescribed.burn.year
fires$fire.cat[c(39,48,49,59,60)] <- paste(rep("Burned", times=5))
names(fires)[2] <- paste("Plot.2015")

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







