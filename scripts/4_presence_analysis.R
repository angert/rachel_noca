# Created: Dec. 11, 2020
# Updated: Mar. 8, 2021

# This script will be used to undertake part 1 of the PRESENCE analyses (modeling)

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:
library(dplyr)

# Functions needed:

# Turning averaged coefficients into +/-/0
simplify.fun <- function(varib) {
  coeff.summary.SPEC <- coeff.summary.empty[varib]
  for(d in 1:100) {
    if(coeff.SPEC.avg[d, varib] > 0) {
      coeff.summary.SPEC[d, varib] <- paste("+")
    }
    if(coeff.SPEC.avg[d, varib] == 0) {
      coeff.summary.SPEC[d, varib] <- paste("0")
    }
    if(coeff.SPEC.avg[d, varib] < 0) {
      coeff.summary.SPEC[d, varib] <- paste("-")
    }
  }
  return(coeff.summary.SPEC)
}

# Summing numbers of +/-/0 for each coeff for one species
# Empty data frame
coeff.count.empty <- data.frame(Elevation.m = rep("NULL", times = 3), 
                                Elevation.m2 = rep("NULL", times = 3), 
                                Resurvey.Burned.fi = rep("NULL", times = 3), 
                                Resurvey.Unburned.fi = rep("NULL", times = 3), 
                                Elevation.m.Res.Burn.fi = rep("NULL", times = 3), 
                                Elevation.m.Res.Unburn.fi = rep("NULL", times = 3), 
                                Elevation.m2.Res.Burn.fi = rep("NULL", times = 3), 
                                Elevation.m2.Res.Unburn.fi = rep("NULL", times = 3), 
                                Data.Type.nofi = rep("NULL", times = 3), 
                                Data.Type.Elevation.m.nofi = rep("NULL", times = 3), 
                                Data.Type.Elevation.m2.nofi = rep("NULL", times = 3),
                                row.names = c("+", "-", 0))
# Function
count.fun <- function(varib) {
  df.count <- coeff.count.empty[varib]
  if(is.na(table(simple.coeffs.SPEC[varib])["+"]) == FALSE) {
    df.count["+", varib] <- table(simple.coeffs.SPEC[varib])["+"]
  } 
  if(is.na(table(simple.coeffs.SPEC[varib])["-"]) == FALSE) {
    df.count["-", varib] <- table(simple.coeffs.SPEC[varib])["-"]
  } 
  if(is.na(table(simple.coeffs.SPEC[varib])["0"]) == FALSE) {
    df.count["0", varib] <- table(simple.coeffs.SPEC[varib])["0"]
  } 
  return(df.count)
}


#### STEP 1: Import data ####

warn.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_warnings.csv", header = TRUE)

# Changed to 3c input file Mar. 8 2021
coeff.ALLDAT <- read.csv("data/3c_top_mod_coefficients.csv", header = TRUE)
coeff.ALLDAT[is.na(coeff.ALLDAT)] <- paste(0)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS" &
                                      !shifts$Species.Code=="COST" &
                                      !shifts$Species.Code=="LUPE" &
                                      !shifts$Species.Code=="PHEM" &
                                      !shifts$Species.Code=="RHAL" &
                                      !shifts$Species.Code=="VAAL" &
                                      !shifts$Species.Code=="VADE"] #removing problematic species
species.list <- factor(species.list)

#### STEP 2 (OPTIONAL): Exploratory visualizations of warnings ####

(numbered.species <- data.frame(Species=species.list, No.=rep(1:nrow(numbered.species))))

# Choose species of interest
warn.SPEC <- warn.ALLDAT[warn.ALLDAT$Species == "VAME", ]

# Assess  warnings
table(warn.SPEC$Has_warning, warn.SPEC$Dataset)
table(table(warn.SPEC$Has_warning, warn.SPEC$Dataset))
head(warn.SPEC[warn.SPEC$Has_warning == TRUE, ])


#### STEP 3: Exploring coefficients ####

coeff.count.LIST <- list()

for(S in 1:nrow(numbered.species)) {
  coeff.SPEC.avg <- coeff.ALLDAT[coeff.ALLDAT$Species == levels(numbered.species$Species)[S], ]
  #coeff.SPEC.avg <- coeff.ALLDAT[coeff.ALLDAT$Species == levels(numbered.species$Species)[S] & 
  #                                 coeff.ALLDAT$Type == "Avg", ]
  
  coeff.summary.empty <- data.frame(Elevation.m = rep("NULL", times = 100), 
                                    Elevation.m2 = rep("NULL", times = 100), 
                                    Resurvey.Burned.fi = rep("NULL", times = 100), 
                                    Resurvey.Unburned.fi = rep("NULL", times = 100), 
                                    Elevation.m.Res.Burn.fi = rep("NULL", times = 100), 
                                    Elevation.m.Res.Unburn.fi = rep("NULL", times = 100), 
                                    Elevation.m2.Res.Burn.fi = rep("NULL", times = 100), 
                                    Elevation.m2.Res.Unburn.fi = rep("NULL", times = 100), 
                                    Data.Type.nofi = rep("NULL", times = 100), 
                                    Data.Type.Elevation.m.nofi = rep("NULL", times = 100), 
                                    Data.Type.Elevation.m2.nofi = rep("NULL", times = 100))
  
  simple.coeffs.SPEC <- bind_cols(lapply(names(coeff.summary.empty), simplify.fun))
  
  # Summarizing the summary
  
  coeff.count.SPEC <- bind_cols((lapply(names(coeff.count.empty), count.fun)))
  coeff.count.SPEC$Species <- levels(factor(coeff.SPEC.avg$Species))
  if(length(levels(factor(coeff.SPEC.avg$Fire.Included))) == 1) {
    coeff.count.SPEC$Fire.Included <- levels(factor(coeff.SPEC.avg$Fire.Included))
  } else(coeff.count.SPEC$Fire.Included <- "Sometimes")
  coeff.count.SPEC$Effect <- row.names(coeff.count.SPEC)
  coeff.count.LIST[[S]] <- coeff.count.SPEC
}

coeff.count <- ldply(coeff.count.LIST, data.frame)
write.csv(coeff.count, 
          file = "data/4c_temp_presence_coefficients_count.csv", 
          row.names = FALSE)





#### STEP 4 (Optional): How did rarefaction vary between species? ####
par(mfrow=c(1,3), mar=c(5, 1.5, 1, 0), oma=c(0,4,0,0)) # 3-paneled graph
hist(coeff.SPEC$R.Occ, main = "RHAL", xlab = "No. resurvey occ", ylab = "Frequency")






