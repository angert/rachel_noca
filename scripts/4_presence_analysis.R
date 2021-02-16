# Created: Dec. 11, 2020
# Updated: Feb. 15, 2020

# This script will be used to undertake part 1 of the PRESENCE analyses (modeling)

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:


#### STEP 1: Import data ####

warn.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_warnings.csv", header = TRUE)
coeff.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_coefficients.csv", header = TRUE)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

head(warn.ALLDAT)
table(warn.ALLDAT$Has_warning, warn.ALLDAT$Dataset, warn.ALLDAT$Species)


#### STEP 2: Exploratory visualizations of warnings ####

(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))

warn.SPEC <- warn.ALLDAT[warn.ALLDAT$Species == "AMAL", ]

table(warn.SPEC$Has_warning, warn.SPEC$Dataset)











