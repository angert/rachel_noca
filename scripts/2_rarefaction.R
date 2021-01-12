# Created: Jan. 11, 2020
# Updated: Jan. 11, 2020

# This script will be used to created a RAREFIED dataset based on a dataset whittled to shared species (those appearing in both surveys).


#### STEP 1: Import data ####

und.cover <- read.csv("data/1_cover_with_fires.csv", header = TRUE, na.strings = "")
und.cover$Fires <- as.factor(und.cover$Fires)
und.cover$Data.Type <- as.factor(und.cover$Data.Type)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)


##### STEP 4 (Optional): Exploratory questions ####

# Do the number of genus-level IDs (__XX) differ between surveys? (YES)

genus.only <- subset(removal3C.cover, grepl("XX", removal3C.cover$Species.Code))
length(table(genus.only$Species.Code[genus.only$Data.Type == "Legacy"])) # 77 species
length(table(genus.only$Species.Code[genus.only$Data.Type == "Resurvey"])) # 32 species

# How many species were found per plot, on average?

mean(table(removal2C.cover$Plot[removal2C.cover$Data.Type == "Legacy"])) # 6.859459 species/plot
mean(table(removal2C.cover$Plot[removal2C.cover$Data.Type == "Resurvey"])) # 10.22973 species/plot



# 4(c) How many species were found per plot, on average, after reducing to common species?

mean(table(removal4B.cover$Plot[removal4B.cover$Data.Type == "Legacy"])) # 5.688347 species/plot
mean(table(removal4B.cover$Plot[removal4B.cover$Data.Type == "Resurvey"])) # 7.612466 species/plot

# In other words, the resurvey found ~2 more species per plot.


#### PLACEHOLDER STEP: Incorporate raw % values from resurvey to use as weights in rarefaction ####


#### STEP 5: Rarefaction ####








