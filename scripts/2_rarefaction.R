# Created: Jan. 11, 2020
# Updated: Jan. 11, 2020

# This script will be used to created a RAREFIED dataset
# This script will be used to created a RAREFIED dataset based on a dataset whittled to common species (those appearing in both surveys).

#### STEP 1: Import data ####

und.cover <- read.csv("data/1_cover_with_fires.csv", header = TRUE, na.strings = "")
und.cover$Fires <- as.factor(und.cover$Fires)
und.cover$Data.Type <- as.factor(und.cover$Data.Type)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

