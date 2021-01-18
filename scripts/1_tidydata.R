# Created: Nov. 24, 2020
# Updated: Jan. 18, 2020

# This script will be used to create the datasets used in the COVER and PRESENCE analyses. Species not shared appearing in both surveys, hybrids, family-level IDs, invasives, and unknowns are removed in this script.

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




### STEP 3: Species-related corrections (remove unknowns, hybrids, etc.) ####

## How many species are in legacy vs resurvey data, PRE-tidying?
length(table(und.cover$Species.Code[und.cover$Data.Type == "Legacy"])) #219 species
length(table(und.cover$Species.Code[und.cover$Data.Type == "Resurvey"])) #536 species

## 3(a): Remove invasives, family-level IDs, trees, and lichen
removal3A.cover <- und.cover[!und.cover$Species.Code == "XBOR" &    # X___ = family-level ID
                               !und.cover$Species.Code == "XBRA" &
                               !und.cover$Species.Code == "XLAM" &
                               !und.cover$Species.Code == "XLIC" &  # Lichen
                               !und.cover$Species.Code == "XORC" &
                               !und.cover$Species.Code == "XPOA" &
                               !und.cover$Species.Code == "XRAN" &
                               !und.cover$Species.Code == "NA" &
                               !und.cover$Species.Code == "BRTE" &  # Cheatgrass (invasive)
                               !und.cover$Species.Code == "ABGR" &  #Abies grandis (tree)
                               !und.cover$Species.Code == "LAMU" &
                               !und.cover$Species.Code == "HYPE" &
                               !und.cover$Species.Code == "POBU" &
                               !und.cover$Species.Code == "TRDU" &
                               !und.cover$Species.Code == "ACGR" &
                               !und.cover$Species.Code == "ACMA", ]

## 3(b) Remove uncertains and hybrids (primarily from resurvey; e.g. VAOVxAL)
removal3B.cover <- removal3A.cover[which(nchar(removal3A.cover$Species.Code) == 4), ]

## 3(c) Remove any remaining unknowns (primarily from resurvey)
removal3C.cover <- subset(removal3B.cover, !grepl("unk", removal3B.cover$Species.Code))

## How many species are in legacy vs resurvey data, POST-tidying?
length(table(removal3C.cover$Species.Code[removal3C.cover$Data.Type == "Legacy"])) #209 species
length(table(removal3C.cover$Species.Code[removal3C.cover$Data.Type == "Resurvey"])) #353 species




#### STEP 4: Additional data tidying (reduce to shared species only) ####

# 4(a) Separate into legacy and resurvey datasets to make comparison easier
legacy.removal4A.cover <- removal3C.cover[removal3C.cover$Data.Type == "Legacy", ]
legacy.removal4A.cover$Species.Code <- factor(legacy.removal4A.cover$Species.Code)
resurvey.removal4A.cover <- removal3C.cover[removal3C.cover$Data.Type == "Resurvey",]
resurvey.removal4A.cover$Species.Code <- factor(resurvey.removal4A.cover$Species.Code)

# 4(b) Which species are common to both surveys?
(common.sp <- as.vector(levels(legacy.removal4A.cover$Species.Code)
                        [levels(legacy.removal4A.cover$Species.Code) %in% 
                            levels(resurvey.removal4A.cover$Species.Code)])) #130 species

common.sp.for.merge <- data.frame("Species.Code" = common.sp, 
                                  "Num" = rep(1:length(common.sp)))

removal4B.cover <- merge(common.sp.for.merge, removal3C.cover, 
                         by="Species.Code", all.x=FALSE)

# Checking lengths
nrow(removal4B.cover) #Should be 4908 after




#### STEP 5: Adding fires as a covariate ####

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

cover.fires <- merge(removal4B.cover, list.fires[-c(2, 3)], by="Plot")
cover.fires$Fires <- as.factor(cover.fires$Fires)


#### STEP 6: Adding raw % cover as a covariate, to be used in rarefaction ####

resurvey.2015 <- read.csv("data/Understory_2015.csv", na.strings = c("", " "))
resurvey.2014<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/Understory_2014_Aug8.csv", na.strings = c("", " "))
resurvey.2015$Percent.Cover <-as.numeric(as.character(resurvey.2015 $Percent.Cover))
resurvey.2014$Percent.Cover <-as.numeric(as.character(resurvey.2014$Percent.Cover))

# Now, I will average % cover to the plot level
resurvey.2015<-resurvey.2015[complete.cases(resurvey.2015$Percent.Cover),]
resurvey.2014<-resurvey.2014[complete.cases(resurvey.2014$Percent.Cover),]
und.2015<-aggregate(resurvey.2015["Percent.Cover"], (resurvey.2015[c("Plot", "Species")]), mean)
und.2015$Year<-rep(2015)
und.2014<-aggregate(resurvey.2014["Percent.Cover"], (resurvey.2014[c("Plot", "Species")]), mean)
und.2014$Year<-rep(2014)

percent.all<-rbind(und.2014, und.2015)
names(percent.all)[2]<-"Species.Code"
test<-join(percent.all, nounk.resurvey, type="right")
test[test$Year=="2015" & is.na(test$Percent.Cover)==TRUE,] #make sure everything crossed over ok

# for some reason, Sour4023 refuses to store % cover for O replicate..!?
test[test$Plot=="Sour4023" & is.na(test$Percent.Cover)==TRUE,4]<-c(0.1, 0.1)

#I need to include % cover as a probability weight for all these species so for now I'm just coding all 2014 ACCI covers as their 2015 mean across plots. Same for ACGL
test[test$Year=="2014" & is.na(test$Percent.Cover)==TRUE & test$Species.Code=="ACCI",4]<-rep(mean(test[test$Species.Code=="ACCI",4], na.rm=TRUE))
test[test$Year=="2014" & is.na(test$Percent.Cover)==TRUE & test$Species.Code=="ACGL",4]<-rep(mean(test[test$Species.Code=="ACGL",4], na.rm=TRUE))

#### STOP!!! You are about to convert the ORIGINAL und.cover into an und.cover of only common species (genus-level ID or more)! ####
nounk.legacy$Percent.Cover<-rep(NA)
und.covertemp<-rbind(nounk.legacy, test) #nrow = ~6400

test2 <- und.covertemp[und.covertemp$Species.Code %in% common.sp,]
test2$Species.Code <- factor(test2$Species.Code)
test2$Plot <- factor(test2$Plot)

#these plots contain only uncommon species - hardcode them in as NAs
guh <- und.covertemp[und.covertemp$Plot == "2045" | und.covertemp$Plot == "Hozo140",]
blerp <- guh[1:2,]
blerp[,2:3] <- paste(NA) #overwrite as NAs

und.cover <- rbind(test2, blerp)
und.cover$Plot <- factor(und.cover$Plot)

#is it really just common species?
length(levels(und.cover$Species.Code)) #should be 131 including 1 NA







#### STEP 7: Write cover.fires, to be used in subsequent scripts ####

write.csv(cover.fires, file="data/1_cover_with_fires.csv", row.names=FALSE)




#### STEP 8: Create binary presence (und.presence) file from understory cover data, unrarefied ####

# Important note! The presabs code DOES INCLUDE presences where the cover was recorded as "NA". This is fine as these species were definitely present, we just forgot to record their cover. 

# Creating a new data frame from cover.fires of 1s/0s presence/absence in each plot

pres.abs <- table(cover.fires$Plot, cover.fires$Species.Code)
und.presence.small <- melt(pres.abs, id.vars=c("Plot", "Species.Code"))
names(und.presence.small) <- c("Plot", "Species.Code", "Pres.Abs")
und.presence <- merge(und.presence.small, list.fires, by = "Plot")
und.presence$Data.Type <- as.factor(und.presence$Data.Type)

#Correcting for a small number of species that ended up double-counted in Understory_All
und.presence$Pres.Abs <- ifelse(und.presence$Pres.Abs >= 1, 1, 0)

write.csv(und.presence, "data/1_presence_fires_unrarefied.csv", row.names = FALSE)











