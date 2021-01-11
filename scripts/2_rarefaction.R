# Created: Jan. 11, 2020
# Updated: Jan. 11, 2020

# This script will be used to created a RAREFIED dataset based on a dataset whittled to common species (those appearing in both surveys).

#### STEP 1: Import data ####

und.cover <- read.csv("data/1_cover_with_fires.csv", header = TRUE, na.strings = "")
und.cover$Fires <- as.factor(und.cover$Fires)
und.cover$Data.Type <- as.factor(und.cover$Data.Type)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

#### STEP 2: Additional data tidying ####

# How many species are in each dataset, pre-tidying?

length(table(und.cover$Species.Code[und.cover$Data.Type == "Legacy"])) #219 species
length(table(und.cover$Species.Code[und.cover$Data.Type == "Resurvey"])) #536 species

## 2(a): Remove invasives and family-level IDs

removal2A.cover <- und.cover[!und.cover$Species.Code == "XBOR" &    # X___ = family-level ID
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
