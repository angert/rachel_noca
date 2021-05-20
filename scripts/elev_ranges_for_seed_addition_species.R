# Created: May 18, 2021
# Updated: 

# This script is modified from Rachel's 1_tidydata.R script.
# Its purpose is to describe the elevation ranges of focal species used in the seed addition experiment (i.e., to characterize each seed addition plot as being within vs outside of the species' range)

# Understory species: anemone occidentalis, eriophyllum lanatum, erigeron perigrinus, lupinus latifolius (probably recorded as arcticus at time of survey), mahonia aquifolium,  rubus ursinus, sambucus racemosa, sorbus sitchensis, ambucus cerulea, tellima grandiflora, tolmiea menziesii, vaccinium parvifollium

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(reshape2)
library(rms)
library(plyr)
library(MuMIn)


#### STEP 1: Import data ####

und.cover <- read.csv("data/Understory_All.csv", header=TRUE, na.strings="") # L = Legacy and R = Resurvey
und.cover$Elevation.m <- as.numeric(as.character(und.cover$Elevation.m)) #gives NA warning - no worries

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


#### STEP 3: Filter to focal species ####

focal.list <- as.data.frame(c("ANOC", "ERLA", "ERPE", "LULA", "MAAQ", "RUUR", "SARA", "SOSI", "AMCE", "TEGR", "TOME", "VAPA"))
names(focal.list) = "species"

und.cover.focal <- left_join(focal.list, und.cover, by=c("species"="Species.Code"))



#### STEP 4: Calculate elevation ranges of focal species ####
focal.ranges <- und.cover.focal %>% 
  group_by(species) %>% 
  summarise(min_el = min(Elevation.m),
         med_el = median(Elevation.m),
         max_el = max(Elevation.m),
         n_obs = sum(!is.na(Elevation.m)))



