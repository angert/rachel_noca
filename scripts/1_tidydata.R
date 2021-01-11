# Created: Nov. 24, 2020
# Updated: Dec. 11, 2020

# This script will be used to create the datasets used in the COVER and PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(reshape2)
library(rms)
library(plyr)
library(MuMIn)


#### STEP 1: Import data ####

und.cover <- read.csv("data/Understory_All.csv", header=TRUE, na.strings="") # L = Legacy and R = Resurvey
und.cover$Elevation.m <- as.numeric(as.character(und.cover$Elevation.m)) #gives warning - no worries

lat.long <- read.csv("data/Lat.Long.csv", header=TRUE, na.strings="")
plot.names <- lat.long[, c(2, 3, 6)]
names(plot.names) <- c("Plot.2015", "Plot.1980", "Elevation.m")

fires <- read.csv("data/All_Plots_Wildfire_Join.csv", header=TRUE, na.strings="")


#### STEP 2: Plot-related corrections (removals, edits, and additions) ####

# List of datasets to be modified: 
# --> fires
# --> plot.names
# --> und.cover

# List of plots to be removed and reasons why (NB - not present in every dataset):
# --> HB5144 (5144), Copp6046 (6046): Latitude/longitude was not recorded
# --> Dia4 (1004): Data collected at plot not found - hard copy may have been lost.
# --> Supp2026, Supp5127, and ROSS4001REF: Supplemental plots taken for future reference. Ignore corresponding 1980 plot name; this was the closest plot.
# --> Thor223 (4044), Bak494 (8017): History of logging.

nrow(fires) #Should be 373 before
nrow(plot.names) #Should be 378 before
nrow(und.cover) #Should be 6803 before

fires <- fires[!fires$Name == "Supp2026" &
                 !fires$Name == "Supp5127" &
                 !fires$Name == "ROSS4001REF" &
                 !fires$Name == "Thor223" &
                 !fires$Name == "Bak494", ]
plot.names <- plot.names[!plot.names$Plot.2015 == "HB5144" &
                 !plot.names$Plot.2015 == "Dia4" &
                 !plot.names$Plot.2015 == "Supp2026" &
                 !plot.names$Plot.2015 == "Supp5127" &
                 !plot.names$Plot.2015 == "ROSS4001REF" &
                 !plot.names$Plot.2015 == "Thor223" &
                 !plot.names$Plot.2015 == "Bak494" &
                 !plot.names$Plot.2015 == "Copp6046", ]
und.cover <- und.cover[!und.cover$Plot == "HB5144" &
                 !und.cover$Plot == "5144" &
                 !und.cover$Plot == "1004" &
                 !und.cover$Plot == "Supp2026" &
                 !und.cover$Plot == "Supp5127" & 
                 !und.cover$Plot == "ROSS4001REF" &
                 !und.cover$Plot == "Thor223" & 
                 !und.cover$Plot == "4044" & 
                 !und.cover$Plot == "Bak494" & 
                 !und.cover$Plot == "8017"  & 
                 !und.cover$Plot == "Copp6046" & 
                 !und.cover$Plot == "6046", ]

# List of plots to be renamed:
# --> Change "Thor225-m" to "Thor225" in fires dataset

fires[fires$Name == "Thor225-m", 2] <- paste("Thor225")

# List of plots to be added:
# --> Thor221 added to fires dataset
# --< Copp6040 added to fires dataset

fires[nrow(fires) + 1, ] <- c(2014, "Thor221", rep(NA, times = length(fires) - 2))
fires[nrow(fires) + 1, ] <- c(2015, "Copp6040", rep(NA, times = length(fires) - 2))

# Fixing row names
rownames(fires) <- 1:nrow(fires)
rownames(plot.names) <- 1:nrow(plot.names)
rownames(und.cover) <- 1:nrow(und.cover)

# Checking lengths
nrow(fires) #Should be 370 after
nrow(plot.names) #Should be 370 after
nrow(und.cover) #Should be 6690 after


#### STEP 3: Adding fires as a covariate ####

# Create new variable, fire.cat, identifying plots burned > 1983
fires$fire.cat <- ifelse(fires$CAL_YEAR >= 1983, "Burned", "Unburned")
fires[is.na(fires$fire.cat) == TRUE, ]$fire.cat <- 
  paste(rep("Unburned", 
            times=length(fires[is.na(fires$fire.cat) == TRUE, 6]))) # Any NAs are unburned

# Adding prescribed burns. See column Prescribed.burn.year
fires[is.na(fires$Prescribed.burn.year) == FALSE, ][ncol(fires)] <- 
  paste(rep("Burned",
            times = nrow(fires[is.na(fires$Prescribed.burn.year) == FALSE, ])))
names(fires)[2] <- paste("Plot.2015")

table(fires$fire.cat ) #Should be 38 Burned and 332 Unburned

# Parse down fire data (fires) and merge with list of 2015 plot names
fires.covariate <- data.frame(fires$Plot.2015, fires$fire.cat)
names(fires.covariate) <- paste(c("Plot.2015", "fire.cat"))
names.fires <- merge(fires.covariate, plot.names, by="Plot.2015", all.y=TRUE)

# Reshape fire data (names.fires) to prepare for merge with und.cover data
list.fires <- 
  melt(names.fires, id.vars=c("fire.cat", "Elevation.m"), 
       measure.vars=c("Plot.2015", "Plot.1980"))
names(list.fires) <- c("Fires", "Elevation.m", "Data.Type", "Plot")
list.fires$Data.Type <- 
  ifelse(list.fires$Data.Type == "Plot.1980", "Legacy", "Resurvey")

cover.fires <- merge(und.cover, list.fires[-c(2, 3)], by="Plot")
cover.fires$Fires<-as.factor(cover.fires$Fires)


#### STEP 4: Write cover.fires, to be used in subsequent scripts ####

#write.csv(data/cover.fires, file="1_cover_with_fires.csv", row.names=FALSE)


#### STEP 5: Create binary presence (und.presence) file from understory cover data, to be used in subsequent steps ####

# Important note! The presabs code DOES INCLUDE presences where the cover was recorded as "NA". This is fine as these species were definitely present, we just forgot to record their cover. 

# Creating a new data frame from cover.fires of 1s/0s presence/absence in each plot
pres.abs <- table(cover.fires$Plot, cover.fires$Species.Code)
und.presence.small <- melt(pres.abs, id.vars=c("Plot", "Species.Code"))
names(und.presence.small) <- c("Plot", "Species.Code", "Pres.Abs")
und.presence <- merge(und.presence.small, list.fires, by = "Plot")
und.presence$Data.Type <- as.factor(und.presence$Data.Type)

#Correcting for a small number of species that ended up double-counted in Understory_All
und.presence$Pres.Abs <- ifelse(und.presence$Pres.Abs >= 1, 1, 0)

# Removes one species named "NA" with abundance of 0 in every plot
und.presence <- und.presence[complete.cases(und.presence),]


#write.csv(data/und.presence, "1_presence_with_fires.csv", row.names = FALSE)











