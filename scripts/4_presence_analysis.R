# Created: Dec. 11, 2020
# Updated: Feb. 15, 2020

# This script will be used to undertake part 1 of the PRESENCE analyses (modeling)

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:
library(plyr)

# Functions needed:

# Turning averaged coefficients into +/-/0
simplify.fun <- function(var) {
  coeff.summary.SPEC <- coeff.summary.empty[var]
  for(d in 1:100) {
    if(coeff.SPEC.avg[d, var] > 0) {
      coeff.summary.SPEC[d, var] <- paste("+")
    }
    if(coeff.SPEC.avg[d, var] == 0) {
      coeff.summary.SPEC[d, var] <- paste("0")
    }
    if(coeff.SPEC.avg[d, var] < 0) {
      coeff.summary.SPEC[d, var] <- paste("-")
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
count.fun <- function(var) {
  df.count <- coeff.count.empty[var]
  if(is.na(table(simple.coeffs.SPEC[var])["+"]) == FALSE) {
    df.count["+", var] <- table(simple.coeffs.SPEC[var])["+"]
  } 
  if(is.na(table(simple.coeffs.SPEC[var])["-"]) == FALSE) {
    df.count["-", var] <- table(simple.coeffs.SPEC[var])["-"]
  } 
  if(is.na(table(simple.coeffs.SPEC[var])["0"]) == FALSE) {
    df.count["0", var] <- table(simple.coeffs.SPEC[var])["0"]
  } 
  return(df.count)
}


#### STEP 1: Import data ####

warn.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_warnings.csv", header = TRUE)
coeff.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_coefficients.csv", header = TRUE)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)


#### STEP 2 (OPTIONAL): Exploratory visualizations of warnings ####

(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))

# Choose species of interest
warn.SPEC <- warn.ALLDAT[warn.ALLDAT$Species == "VAME", ]

# Assess  warnings
table(warn.SPEC$Has_warning, warn.SPEC$Dataset)
table(table(warn.SPEC$Has_warning, warn.SPEC$Dataset))
head(warn.SPEC[warn.SPEC$Has_warning == TRUE, ])


#### STEP 3: Exploring coefficients ####

coeff.count.LIST <- list()

for(S in 1:nrow(numbered.species)) {
  coeff.SPEC.avg <- coeff.ALLDAT[coeff.ALLDAT$Species == levels(numbered.species$Species)[S] & 
                                   coeff.ALLDAT$Type == "Avg", ]
  
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
  coeff.count.SPEC$Effect <- row.names(coeff.count.SPEC)
  coeff.count.LIST[[S]] <- coeff.count.SPEC
}

coeff.count <- ldply(coeff.count.LIST, data.frame)
write.csv(coeff.count, 
          file = "data/4_presence_coefficients_count.csv", 
          row.names = FALSE)





#### STEP 4 (Optional): How did rarefaction vary between species? ####
par(mfrow=c(1,3), mar=c(5, 1.5, 1, 0), oma=c(0,4,0,0)) # 3-paneled graph
hist(coeff.SPEC$R.Occ, main = "RHAL", xlab = "No. resurvey occ", ylab = "Frequency")






