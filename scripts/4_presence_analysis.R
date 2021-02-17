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

just.avg <- coeff.ALLDAT[coeff.ALLDAT$Type == "Avg", ]
just.avg.fire <- just.avg[just.avg$Fire.Included == "Yes", ]

#### STEP 2: Exploratory visualizations of warnings ####

(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))

warn.SPEC <- warn.ALLDAT[warn.ALLDAT$Species == "EPAN", ]

table(warn.SPEC$Has_warning, warn.SPEC$Dataset)
table(table(warn.SPEC$Has_warning, warn.SPEC$Dataset))
 nhead(warn.SPEC[warn.SPEC$Has_warning == TRUE, ])



coeff.SPEC <- coeff.ALLDAT[coeff.ALLDAT$Species == "RHAL", ]

# Exploring coefficients
mean(coeff.SPEC$Data.Type.Elevation.m.Fires[coeff.SPEC$Type == "Avg"], na.rm = TRUE)

#### STEP 3: How did rarefaction vary between species? ####
par(mfrow=c(1,3), mar=c(5, 1.5, 1, 0), oma=c(0,4,0,0)) # 3-paneled graph
hist(coeff.SPEC$R.Occ, main = "RHAL", xlab = "No. resurvey occ", ylab = "Frequency")






