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

#### STEP 2: Data tidying (remove unknowns, hybrids, etc.) ####

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

# How many species were found per plot, on average?

mean(table(removal2C.cover$Plot[removal2C.cover$Data.Type == "Legacy"])) # 6.859459 species/plot
mean(table(removal2C.cover$Plot[removal2C.cover$Data.Type == "Resurvey"])) # 10.22973 species/plot


#### STEP 4: Additional data tidying (reduce to shared species only) ####

# 4(a) Separate into legacy and resurvey datasets to make comparison easier

legacy.removal2C.cover <- removal2C.cover[removal2C.cover$Data.Type == "Legacy", ]
legacy.removal2C.cover$Species.Code <- factor(legacy.removal2C.cover$Species.Code)
resurvey.removal2C.cover <- removal2C.cover[removal2C.cover$Data.Type == "Resurvey",]
resurvey.removal2C.cover$Species.Code <- factor(resurvey.removal2C.cover$Species.Code)

# 4(b) Which species are common to both surveys?

(common.sp <- as.vector(levels(legacy.removal2C.cover$Species.Code)
                        [levels(legacy.removal2C.cover$Species.Code) %in% 
                            levels(resurvey.removal2C.cover$Species.Code)])) #130 species

common.sp.for.merge <- data.frame("Species.Code" = common.sp, 
                                  "Num" = rep(1:length(common.sp)))

removal4B.cover <- merge(common.sp.for.merge, removal2C.cover, 
                        by="Species.Code", all.x=FALSE)

# 4(c) How many species were found per plot, on average, after reducing to common species?

mean(table(removal4B.cover$Plot[removal4B.cover$Data.Type == "Legacy"])) # 5.688347 species/plot
mean(table(removal4B.cover$Plot[removal4B.cover$Data.Type == "Resurvey"])) # 7.612466 species/plot

# In other words, the resurvey found ~2 more species per plot.










