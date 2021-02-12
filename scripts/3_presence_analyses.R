# Created: Dec. 11, 2020
# Updated: Feb. 4, 2020

# This script will be used to undertake the PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(MuMIn)
library(plyr)
source("scripts/3_dredge_log_to_df.R")

# Function to create data frame of coefficients, AIC and model ID:

df.fun <- function(ModID) {
  df <- as.data.frame(t(coef(dredge.list.globfi[[ModID]]))) # Transposed DF of coefs
  df$new_Model_id <- paste(ModID) # Store unique model ID
  df$AIC <- dredge.list.globfi[[ModID]]$aic # Store model AIC
  df$logLik <- as.numeric(logLik(dredge.list.globfi[[ModID]])) # Store logLik
  return(df)
}

#### STEP 1: Import data ####

# To analyze un-rarefied data (exclude 100x loop):
# und.presence <- read.csv("data/1_presence_fires_unrarefied", header = TRUE, na.strings = "")

# To analyze rarefied data (as list of dataframes named rare.ALL):
load("data/rare.ALL.Rda")

# Import for both data types:
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)




#### STEP 2: Loop to analyze presence data. Run time unknown ####

# Can be run as a loop outputting all species, or S can be modified to isolated specific species. Check number here:
(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))
# Exclude D loop to run for only one dataset (e.g. unrarefied data)

coeff.ALLDAT <- list()

for(D in 1:2) { #TODO leave as 2 for now just in case
  
  coeff.ALLSPEC <- list()
  und.presence <- rare.ALL[[D]]
  und.presence$Fires <- as.factor(und.presence$Fires)
  und.presence$Data.Type <- as.factor(und.presence$Data.Type)
  und.presence$Elevation.m <- as.numeric(und.presence$Elevation.m)
  
  for(S in 1:length(species.list)) {
    
    # Create subset for species of interest S
    und.presence.SPEC <- subset(und.presence, Species.Code == levels(species.list)[S])
    und.presence.SPEC$Fires <- relevel(und.presence.SPEC$Fires, ref="Unburned")
    und.presence.SPEC$Data.Type <- relevel(und.presence.SPEC$Data.Type, ref="Legacy")
    und.presence.SPEC$Elevation.m2 <- und.presence.SPEC$Elevation.m^2 #TODO better than poly()?
    und.presence.SPEC <- und.presence.SPEC[complete.cases(und.presence.SPEC), ] #Just in case
    
    # Emptying out previous objects
    mod.globfi <- NULL
    mod.globnofi <- NULL
    dredge.globfi <- NULL
    dredge.list.globfi <- NULL
    dredge.globnofi <- NULL
    dredge.list.globfnofi <- NULL
    avg.mods <- NULL
    top.mods.coeff <- NULL
   
    
     # Did the species occur in burned plots 5+ times?
    num.burns <- 
      table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type)
    
    
    # Create new error-logging file specific to permutation & species
    log.file <- file(paste("data/warning_logs/warnings", 
                           D, levels(species.list)[S], ".txt", sep = "_"), open = "wt")
    log.file.path <- paste("data/warning_logs/warnings", 
                           D, levels(species.list)[S], ".txt", sep = "_")
    sink(log.file, append = TRUE, type = "output") # Sink to log file
    sink(log.file, append = TRUE, type = "message")
    
    # If > 5, include fire as a predictor in global model:
    if(num.burns["1", "Burned", "Legacy"] >= 5 | num.burns["1", "Burned", "Resurvey"] >= 5) {
      mod.globfi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2) * Fires, 
                        data = und.presence.SPEC, family = "binomial", na.action = na.fail)
      options(warn = -1) # Ignore warnings - not for logging.
      dredge.list.globfi <- lapply(dredge(mod.globfi, rank = AIC, subset = 
                                dc(Elevation.m, Elevation.m2) &&
                                dc(Data.Type:Elevation.m, Data.Type:Elevation.m2) &&
                                dc(Elevation.m:Fires, Elevation.m2:Fires) &&
                                dc(Data.Type:Elevation.m:Fires, Data.Type:Elevation.m2:Fires), 
                              trace = FALSE, evaluate = FALSE), eval)
      names(dredge.list.globfi) <- paste("Mod", 
                                         as.numeric(names(dredge.list.globfi)) - 1, 
                                         sep = ".") # Converting to model ID
      options(warn = 1) # Tell me if a model throws an error - for logging.
      dredge.globfi <- dredge(mod.globfi, rank = AIC, subset = 
                                dc(Elevation.m, Elevation.m2) &&
                                dc(Data.Type:Elevation.m, Data.Type:Elevation.m2) &&
                                dc(Elevation.m:Fires, Elevation.m2:Fires) &&
                                dc(Data.Type:Elevation.m:Fires, Data.Type:Elevation.m2:Fires), 
                              trace = 1)
      
    }
    
    
    #TODO this part unmodified. If no, exclude fire from global model:
    if(num.burns["1", "Burned", "Legacy"] < 5 & num.burns["1", "Burned", "Resurvey"] < 5) {
      mod.globnofi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2), 
                          data = und.presence.SPEC, family = "binomial", na.action = na.fail) 
      dredge.globnofi <- dredge(mod.globnofi, rank = AIC, subset = 
                                  dc(Elevation.m, Elevation.m2) &&
                                  dc(Data.Type:Elevation.m, Data.Type:Elevation.m2), 
                                trace = 1)
      avg.mods <- model.avg(dredge.globnofi, subset = delta <= 2)
      top.mods.coeff <- as.data.frame(coef(subset(dredge.globnofi, delta <= 2)))
      avg.mods.coeff <- as.data.frame(t(avg.mods$coefficients["full",]))
    }
    
    # Stop writing to .txt, import dataframe of warning logs
    closeAllConnections()
    log.warn <- dredge_log_to_df(log.file.path) # See 3_dredge_log_to_df.R
    log.warn$new_Model_id <- paste("Mod", log.warn$Model_id, sep = ".")
    
    # Run df.fun to pull out coefficients, AIC, model ID from dredge list
    coeff.df <- ldply(lapply(log.warn$new_Model_id, df.fun))
    
    # Join warning log to dataframe of coefficients and AIC
    coeff.warn <- merge(coeff.df, log.warn, by = "new_Model_id", all.x = TRUE)
    
    # Exclude models for which there was a warning
    coeff.nowarn <- coeff.warn[coeff.warn$Has_warning == FALSE, ]
    
    # Calculate delta AIC based on warning-less models, reduce to delta <=2
    coeff.nowarn$delta <- coeff.nowarn$AIC - 
      coeff.nowarn$AIC[coeff.nowarn$AIC == min(coeff.nowarn$AIC)]
    top.mods.coeff <- coeff.nowarn[coeff.nowarn$delta <= 2, ]
    
    # Null model (for Psuedo-R-squared calculation later)
    mod.NULL <- glm(Pres.Abs ~ 1, 
                    data = und.presence.SPEC, family = "binomial", na.action = na.fail)
    
    
    # Storing output
    
    Mods.list <- list()
    
    for(i in 1:nrow(top.mods.coeff)) {
      Mods.list[[i]] <- data.frame(
        Species = levels(species.list)[S], 
        Dataset = D,
        L.Occ = sum(num.burns["1", , "Legacy"]), 
        R.Occ = sum(num.burns["1", , "Resurvey"]), 
        Fire.Included = ifelse(is.null(mod.globnofi) == TRUE, "Yes", "No"),
        Type = "Unavg", 
        deltaAIC = top.mods.coeff$delta[i], 
        Weight = avg.mods$msTable$weight[i], #TODO calculate this in an earlier step
        Rsquared = 1 - top.mods.coeff$logLik[i] / as.numeric(logLik(mod.NULL)),
        Intercept = top.mods.coeff$`(Intercept)`[i],
        Data.Type = top.mods.coeff$Data.TypeResurvey[i],
        Elevation.m = top.mods.coeff$Elevation.m[i],
        Fires = top.mods.coeff$FiresBurned[i],
        Data.Type.Elevation.m = top.mods.coeff$`Data.TypeResurvey:Elevation.m`[i],
        Elevation.m.Fires = top.mods.coeff$`Elevation.m:FiresBurned`[i],
        Data.Type.Fires = top.mods.coeff$`Data.TypeResurvey:FiresBurned`[i],
        Elevation.m2 = top.mods.coeff$Elevation.m2[i],
        Data.Type.Elevation.m2 = top.mods.coeff$`Data.TypeResurvey:Elevation.m2`[i],
        Elevation.m2.Fires = top.mods.coeff$`Elevation.m2:FiresBurned`[i],
        Data.Type.Elevation.m.Fires = 
          top.mods.coeff$`Data.TypeResurvey:Elevation.m:FiresBurned`[i],
        Data.Type.Elevation.m2.Fires = 
          top.mods.coeff$`Data.TypeResurvey:Elevation.m2:FiresBurned`[i],
        row.names = NULL)
    }
    
    Mods <- ldply(Mods.list, data.frame)
    
    #TODO: WIP
    Avg <- data.frame(
      Species = levels(species.list)[S],
      Dataset = D,
      L.Occ = sum(num.burns["1", , "Legacy"]), 
      R.Occ = sum(num.burns["1", , "Resurvey"]), 
      Fire.Included = ifelse(is.null(mod.globnofi) == TRUE, "Yes", "No"),
      Type = "Avg", 
      deltaAIC = NA, 
      Weight = NA,
      Rsquared = NA,
      Intercept = avg.mods.coeff$`(Intercept)`,
      Data.Type = avg.mods.coeff$Data.TypeResurvey,
      Elevation.m = avg.mods.coeff$Elevation.m,
      Fires = avg.mods.coeff$FiresBurned,
      Data.Type.Elevation.m = avg.mods.coeff$`Data.TypeResurvey:Elevation.m`,
      Elevation.m.Fires = avg.mods.coeff$`Elevation.m:FiresBurned`,
      Data.Type.Fires = avg.mods.coeff$`Data.TypeResurvey:FiresBurned`,
      Elevation.m2 = avg.mods.coeff$Elevation.m2,
      Data.Type.Elevation.m2 = avg.mods.coeff$`Data.TypeResurvey:Elevation.m2`,
      Elevation.m2.Fires = avg.mods.coeff$`Elevation.m2:FiresBurned`,
      Data.Type.Elevation.m.Fires = 
        avg.mods.coeff$`Data.TypeResurvey:Elevation.m:FiresBurned`,
      Data.Type.Elevation.m2.Fires = 
        avg.mods.coeff$`Data.TypeResurvey:Elevation.m2:FiresBurned`,
      row.names = NULL)
    
    
    coeff.ALLSPEC[[S]] <- rbind(Mods, Avg)
    
  }  
  
  #Collapse list into dataframe
  
  coeff.ALLDAT[[D]] <- ldply(coeff.ALLSPEC, data.frame)
  
}



# Store output as CSV

# write.csv(coeff, file = "data/3_presence_analyses_coefficients.csv", row.names=FALSE)

  



