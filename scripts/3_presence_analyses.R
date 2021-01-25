# Created: Dec. 11, 2020
# Updated: Jan. 25, 2020

# This script will be used to undertake the PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(MuMIn)
library(plyr)
library(futile.logger) # For caching errors
library(tryCatchLog) # For caching errors

#### STEP 1: Import data ####

# To analyze un-rarefied data (exclude 100x loop):
# und.presence <- read.csv("data/1_presence_fires_unrarefied", header = TRUE, na.strings = "")

# To analyze rarefied data (as list of dataframes named rare.ALL):
load("data/rare.ALL.Rda")

# Import for both data types:
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)




#### STEP 2: Error logging ####

# Work in progress

options(keep.source = TRUE)        # source code file name and line number tracking
options("tryCatchLog.write.error.dump.file" = TRUE) # dump for post-mortem analysis

flog.appender(appender.file("error_logs.log"))  # to log into a file instead of console
flog.threshold(WARN)    # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

tryCatchLog(source("scripts/3_presence_analyses.R"))




#### STEP 3: Loop to analyze presence data Run time ~ 2 sec ####

# Can be run as a loop outputting all species, or S can be modified to isolated specific species. Check number here:
(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))
# Exclude D loop to run for only one dataset (e.g. unrarefied data)

coeff.ALLDAT <- list()

for(D in 1:1) {
  
  coeff.ALLSPEC <- list()
  #options(warn = 1) # Tell me if a model throws an error.
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
    
    #Emptying out previous objects
    mod.globfi <- NULL
    mod.globnofi <- NULL
    dredge.globfi <- NULL
    dredge.globnofi <- NULL
    avg.mods <- NULL
    top.mods.coeff <- NULL
    
    # Did the species occur in burned plots 5+ times?
    num.burns <- 
      table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type)
    
    # If yes, include fire as a predictor in global model:
    if(num.burns["1", "Burned", "Legacy"] >= 5 | num.burns["1", "Burned", "Resurvey"] >= 5) {
      mod.globfi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2) * Fires, 
                        data = und.presence.SPEC, family = "binomial", na.action = na.fail)
      dredge.globfi <- dredge(mod.globfi, rank = AIC, subset = 
                                dc(Elevation.m, Elevation.m2) &&
                                dc(Data.Type:Elevation.m, Data.Type:Elevation.m2) &&
                                dc(Elevation.m:Fires, Elevation.m2:Fires) &&
                                dc(Data.Type:Elevation.m:Fires, Data.Type:Elevation.m2:Fires), 
                              trace = 1)
      avg.mods <- model.avg(dredge.globfi, subset = delta <= 2)
      top.mods.coeff <- as.data.frame(coef(subset(dredge.globfi, delta <= 2)))
      avg.mods.coeff <- as.data.frame(t(avg.mods$coefficients["full",]))
    }
    
    # If no, exclude fire from global model:
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
    
    # Null model (for Psuedo-R-squared calculation later)
    
    mod.NULL <- glm(Pres.Abs ~ 1, 
                    data = und.presence.SPEC, family = "binomial", na.action = na.fail)
    
    # Adding NAs to missing coefficients in top.mods.coeff and 
    
    coeff.all <- c("Data.TypeResurvey", 
                   "Elevation.m", 
                   "FiresBurned", 
                   "Data.TypeResurvey:Elevation.m", 
                   "Elevation.m:FiresBurned", 
                   "Data.TypeResurvey:FiresBurned", 
                   "Elevation.m2", 
                   "Data.TypeResurvey:Elevation.m2", 
                   "Elevation.m2:FiresBurned", 
                   "Data.TypeResurvey:Elevation.m:FiresBurned", 
                   "Data.TypeResurvey:Elevation.m2:FiresBurned")
    
    for(C in 1:length(coeff.all)) {
      if(!coeff.all[C] %in% colnames(top.mods.coeff)) {
        top.mods.coeff[, coeff.all[C]] <- rep(NA, times = nrow(top.mods.coeff))
      }
      if(!coeff.all[C] %in% colnames(avg.mods.coeff)) {
        avg.mods.coeff[, coeff.all[C]] <- rep(NA, times = 1)
      }
    }    
    
    # Storing output
    
    Mods.list <- list()
    
    for(i in 1:nrow(top.mods.coeff)) {
      Mods.list[[i]] <- data.frame(
        Species = levels(species.list)[S],  
        L.Occ = sum(num.burns["1", , "Legacy"]), 
        R.Occ = sum(num.burns["1", , "Resurvey"]), 
        Fire.Included = ifelse(is.null(mod.globnofi) == TRUE, "Yes", "No"),
        Type = "Unavg", 
        deltaAIC = avg.mods$msTable$delta[i], 
        Weight = avg.mods$msTable$weight[i],
        Rsquared = 1 - avg.mods$msTable$logLik[i]/logLik(mod.NULL)[1],
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
    
    Avg <- data.frame(
      Species = levels(species.list)[S],  
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

write.csv(coeff, file="data/2_presence_analyses_coefficients.csv", row.names=FALSE)

  
  



