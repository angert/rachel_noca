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

## How many species are in each dataset, PRE-tidying?

length(table(und.cover$Species.Code[und.cover$Data.Type == "Legacy"])) #219 species
length(table(und.cover$Species.Code[und.cover$Data.Type == "Resurvey"])) #536 species

## 2(a): Remove invasives, family-level IDs, trees, and lichen

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

## 2(b) Remove uncertains and hybrids (primarily from resurvey)
  
removal2B.cover <- removal2A.cover[which(nchar(removal2A.cover$Species.Code) == 4), ]

## 2(c) Remove any remaining unknowns (primarily from resurvey)

removal2C.cover <- subset(removal2B.cover, !grepl("unk", removal2B.cover$Species.Code))

## How many species are in each dataset, POST-tidying?

length(table(removal2C.cover$Species.Code[removal2C.cover$Data.Type == "Legacy"])) #209 species
length(table(removal2C.cover$Species.Code[removal2C.cover$Data.Type == "Resurvey"])) #353 species


#### STEP 3 (Optional): Exploratory questions ####

# Do the number of genus-level IDs (__XX) differ between surveys? (YES)

genus.only <- subset(removal2C.cover, grepl("XX", removal2C.cover$Species.Code))
length(table(genus.only$Species.Code[genus.only$Data.Type == "Legacy"])) # 77 species
length(table(genus.only$Species.Code[genus.only$Data.Type == "Resurvey"])) # 32 species






