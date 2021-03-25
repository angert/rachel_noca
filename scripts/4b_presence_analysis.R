# Created: Mar. 22, 2021

# This script will be used to summarize results of the PRESENCE analyses - P values

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:
library(plyr)

# Functions needed:

# FIRE: Turning P values into S / MS / NS / Ignore
simplify.P.fi <- function(varib) { # FIRE
  P.summary.SPEC <- P.fi.summary.empty[varib]
  dataset.vect <- as.vector(P.fi.SPEC$Dataset)
  for(d in dataset.vect) {
    if(is.na(P.fi.SPEC[P.fi.SPEC$Dataset == d, varib]) == FALSE) {
      if(P.fi.SPEC[P.fi.SPEC$Dataset == d, varib] <= 0.05) {
        P.summary.SPEC[d, varib] <- paste("Significant")
      }
      if(P.fi.SPEC[P.fi.SPEC$Dataset == d, varib] <= 0.1 & 
         P.fi.SPEC[P.fi.SPEC$Dataset == d, varib] > 0.05) {
        P.summary.SPEC[d, varib] <- paste("Marginal")
      }
      if(P.fi.SPEC[P.fi.SPEC$Dataset == d, varib] > 0.1) {
        P.summary.SPEC[d, varib] <- paste("NS")
      }
    } else {
      P.summary.SPEC[d, varib] <- paste("Ignore")
    }
  }
  return(P.summary.SPEC)
}

simplify.P.nofi <- function(varib) { # NO FIRE
  P.nofi.summary.SPEC <- P.nofi.summary.empty[varib]
  dataset.vect <- as.vector(P.nofi.SPEC$Dataset)
  for(d in dataset.vect) {
    if(is.na(P.nofi.SPEC[P.nofi.SPEC$Dataset == d, varib]) == FALSE) {
      if(P.nofi.SPEC[P.nofi.SPEC$Dataset == d, varib] <= 0.05) {
        P.nofi.summary.SPEC[d, varib] <- paste("Significant")
      }
      if(P.nofi.SPEC[P.nofi.SPEC$Dataset == d, varib] <= 0.1 & 
         P.nofi.SPEC[P.nofi.SPEC$Dataset == d, varib] > 0.05) {
        P.nofi.summary.SPEC[d, varib] <- paste("Marginal")
      }
      if(P.nofi.SPEC[P.nofi.SPEC$Dataset == d, varib] > 0.1) {
        P.nofi.summary.SPEC[d, varib] <- paste("NS")
      }
    } else {
      P.nofi.summary.SPEC[d, varib] <- paste("Ignore")
    }
  }
  return(P.nofi.summary.SPEC)
}


# Summing numbers of +/-/0 for each coeff for one species
# Empty data frame
P.fi.count.empty <- data.frame(P.Intercept = rep(NA, times = 4), # FIRE
                            P.Elevation.m = rep(NA, times = 4), 
                            P.Elevation.m2 = rep(NA, times = 4), 
                            P.Resurvey.Burned.fi = rep(NA, times = 4), 
                            P.Resurvey.Unburned.fi = rep(NA, times = 4), 
                            P.Elevation.m.Res.Burn.fi = rep(NA, times = 4), 
                            P.Elevation.m.Res.Unburn.fi = rep(NA, times = 4), 
                            P.Elevation.m2.Res.Burn.fi = rep(NA, times = 4), 
                            P.Elevation.m2.Res.Unburn.fi = rep(NA, times = 4),
                            row.names = c("Significant", "Marginal", "NS", "Ignore"))
P.nofi.count.empty <- data.frame(P.Intercept = rep(NA, times = 4), # NO FIRE
                                 P.Elevation.m = rep(NA, times = 4), 
                                 P.Elevation.m2 = rep(NA, times = 4), 
                                 P.Data.Type.nofi = rep(NA, times = 4), 
                                 P.Data.Type.Elevation.m.nofi = rep(NA, times = 4), 
                                 P.Data.Type.Elevation.m2.nofi = rep(NA, times = 4),
                                 row.names = c("Significant", "Marginal", "NS", "Ignore"))

# Collapsing 100 rows into summary
P.fi.count.fun <- function(varib) { # FIRE
  df.count <- P.fi.count.empty[varib]
  if(is.na(table(simple.P.fi.SPEC[varib])["Significant"]) == FALSE) {
    df.count["Significant", varib] <- table(simple.P.fi.SPEC[varib])["Significant"]
  } 
  if(is.na(table(simple.P.fi.SPEC[varib])["Marginal"]) == FALSE) {
    df.count["Marginal", varib] <- table(simple.P.fi.SPEC[varib])["Marginal"]
  } 
  if(is.na(table(simple.P.fi.SPEC[varib])["NS"]) == FALSE) {
    df.count["NS", varib] <- table(simple.P.fi.SPEC[varib])["NS"]
  } 
  if(is.na(table(simple.P.fi.SPEC[varib])["Ignore"]) == FALSE) {
    df.count["Ignore", varib] <- table(simple.P.fi.SPEC[varib])["Ignore"]
  } 
  return(df.count)
}

P.nofi.count.fun <- function(varib) { #NO FIRE
  df.count <- P.nofi.count.empty[varib]
  if(is.na(table(simple.P.nofi.SPEC[varib])["Significant"]) == FALSE) {
    df.count["Significant", varib] <- table(simple.P.nofi.SPEC[varib])["Significant"]
  } 
  if(is.na(table(simple.P.nofi.SPEC[varib])["Marginal"]) == FALSE) {
    df.count["Marginal", varib] <- table(simple.P.nofi.SPEC[varib])["Marginal"]
  } 
  if(is.na(table(simple.P.nofi.SPEC[varib])["NS"]) == FALSE) {
    df.count["NS", varib] <- table(simple.P.nofi.SPEC[varib])["NS"]
  } 
  if(is.na(table(simple.P.nofi.SPEC[varib])["Ignore"]) == FALSE) {
    df.count["Ignore", varib] <- table(simple.P.nofi.SPEC[varib])["Ignore"]
  } 
  return(df.count)
}


#### STEP 1: Import data ####

# Can use either top_mod input or new_coefficients input
P.fi.ALLDAT <- read.csv("data/3c_global_mod_coefficients_with_P_FIRE.csv", header = TRUE)
P.nofi.ALLDAT <- read.csv("data/3c_global_mod_coefficients_with_P_NOFIRE.csv", header = TRUE)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS" &
                                      !shifts$Species.Code=="COST" &
                                      !shifts$Species.Code=="LUPE" &
                                      !shifts$Species.Code=="PHEM" &
                                      !shifts$Species.Code=="RHAL" &
                                      !shifts$Species.Code=="VAAL" &
                                      !shifts$Species.Code=="VADE"] #removing problematic species
species.list <- factor(species.list)

(numbered.species <- data.frame(Species=species.list, No.=rep(1:length(species.list))))
species.with.fire <- numbered.species[numbered.species$Species == "ACMI" |
                                        numbered.species$Species == "ARUV" |
                                        numbered.species$Species == "CARU" |
                                        numbered.species$Species == "CEVE" |
                                        numbered.species$Species == "EPAN" |
                                        numbered.species$Species == "PAMY" |
                                        numbered.species$Species == "VAME", ]
species.with.fire$Species <- factor(species.with.fire$Species)
species.without.fire <- numbered.species[!numbered.species$Species 
                                         %in% species.with.fire$Species, ]
species.without.fire$Species <- factor(species.without.fire$Species)

#### STEP 2: Fire P-values ####

P.fi.count.LIST <- list()

for(S in 1:nrow(species.with.fire)) {

  P.fi.SPEC <- P.fi.ALLDAT[P.fi.ALLDAT$Species == levels(species.with.fire$Species)[S], ]
  
  P.fi.summary.empty <- data.frame(P.Intercept = rep(NA, times = 100), 
                                P.Elevation.m = rep(NA, times = 100), 
                                P.Elevation.m2 = rep(NA, times = 100), 
                                P.Resurvey.Burned.fi = rep(NA, times = 100), 
                                P.Resurvey.Unburned.fi = rep(NA, times = 100), 
                                P.Elevation.m.Res.Burn.fi = rep(NA, times = 100), 
                                P.Elevation.m.Res.Unburn.fi = rep(NA, times = 100), 
                                P.Elevation.m2.Res.Burn.fi = rep(NA, times = 100), 
                                P.Elevation.m2.Res.Unburn.fi = rep(NA, times = 100))
  
  simple.P.fi.SPEC <- bind_cols(lapply(names(P.fi.summary.empty), simplify.P.fi))

  # Summarizing the summary
  
  P.fi.count.SPEC <- bind_cols((lapply(names(P.fi.count.empty), P.fi.count.fun)))
  P.fi.count.SPEC$Species <- levels(factor(P.fi.SPEC$Species))
  if(length(levels(factor(P.fi.SPEC$Fire.Included))) == 1) { # Red flag if this is ever "sometimes"
    P.fi.count.SPEC$Fire.Included <- levels(factor(P.fi.SPEC$Fire.Included))
  } else(P.fi.count.SPEC$Fire.Included <- "Sometimes")
  P.fi.count.SPEC$Significance <- row.names(P.fi.count.SPEC)
  P.fi.count.LIST[[S]] <- P.fi.count.SPEC
  
}

P.fi.count <- ldply(P.fi.count.LIST, data.frame)

write.csv(P.fi.count, 
          file = "data/4b_FIRE_P_summary.csv", 
          row.names = FALSE)

#### STEP 3: No-fire P values ####

P.nofi.count.LIST <- list()

for(S in 1:nrow(species.without.fire)) {
  
  P.nofi.SPEC <- P.nofi.ALLDAT[P.nofi.ALLDAT$Species == levels(species.without.fire$Species)[S], ]
  
  P.nofi.summary.empty <- data.frame(P.Intercept = rep(NA, times = 100), 
                                P.Elevation.m = rep(NA, times = 100), 
                                P.Elevation.m2 = rep(NA, times = 100), 
                                P.Data.Type.nofi = rep(NA, times = 100), 
                                P.Data.Type.Elevation.m.nofi = rep(NA, times = 100), 
                                P.Data.Type.Elevation.m2.nofi = rep(NA, times = 100))
  
  simple.P.nofi.SPEC <- bind_cols(lapply(names(P.nofi.summary.empty), simplify.P.nofi))
  
  # Summarizing the summary
  
  P.nofi.count.SPEC <- bind_cols((lapply(names(P.nofi.count.empty), P.nofi.count.fun)))
  P.nofi.count.SPEC$Species <- levels(factor(P.nofi.SPEC$Species))
  if(length(levels(factor(P.nofi.SPEC$Fire.Included))) == 1) { # Red flag if this is "sometimes"
    P.nofi.count.SPEC$Fire.Included <- levels(factor(P.nofi.SPEC$Fire.Included))
  } else(P.nofi.count.SPEC$Fire.Included <- "Sometimes")
  P.nofi.count.SPEC$Significance <- row.names(P.nofi.count.SPEC)
  P.nofi.count.LIST[[S]] <- P.nofi.count.SPEC
  
}

P.nofi.count <- ldply(P.nofi.count.LIST, data.frame)


write.csv(P.nofi.count, 
        file = "data/4b_NOFIRE_P_summary.csv", 
        row.names = FALSE)






