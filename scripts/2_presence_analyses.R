# Created: Dec. 11, 2020
# Updated: Dec. 11, 2020

# This script will be used to undertake the PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

##TODO add packages here
library(MuMIn)

#### STEP 1: Import data ####

und.presence <- read.csv("data/1_presence_with_fires.csv", header = TRUE, na.strings = "")
und.presence$Fires <- as.factor(und.presence$Fires)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)


#### STEP 2: Loop to create presence analyses. Run time ~ 2 sec ####

# Can be run as a loop outputting all species, or S can be modified to isolated specific species. Check number here:
(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))


coeff.SPEC<-list()
options(warn = 1) # Tell me if a model throws an error.

for(S in 1:length(species.list)) {
  
  # Create subset for species of interest S
  und.presence.SPEC <- subset(und.presence, Species.Code == levels(species.list)[S])
  und.presence.SPEC$Fires <- relevel(und.presence.SPEC$Fires, ref="Unburned")
  und.presence.SPEC$Elevation.m2 <- und.presence.SPEC$Elevation.m^2 #TODO better than poly()?
  und.presence.SPEC <- und.presence.SPEC[complete.cases(und.presence.SPEC), ] #Just in case
  
  # Did the species occur in burned plots 5+ times?
  num.burns <- 
    table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type)
  
  #Emptying out previous objects
  mod.globfi <- NULL
  dredge.globfi <- NULL
  avg.glbofi <- NULL
  mod.globnofi <- NULL
  dredge.globnofi <- NULL
  avg.globnofi <- NULL
  top.mods <- NULL
  
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
    avg.globfi <- model.avg(dredge.globfi, subset = delta <= 2)
    top.mods.coeff <- as.data.frame(coef(subset(dredge.globnofi, delta <= 2)))
  }
  
  # If no, exclude fire from global model:
  if(num.burns["1", "Burned", "Legacy"] < 5 & num.burns["1", "Burned", "Resurvey"] < 5) {
    mod.globnofi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2), 
                        data = und.presence.SPEC, family = "binomial", na.action = na.fail) 
    dredge.globnofi <- dredge(mod.globnofi, rank = AIC, subset = 
                                dc(Elevation.m, Elevation.m2) &&
                                dc(Data.Type:Elevation.m, Data.Type:Elevation.m2), 
                              trace = 1)
    avg.globnofi <- model.avg(dredge.globnofi, subset = delta <= 2)
    top.mods.coeff <- as.data.frame(coef(subset(dredge.globnofi, delta <= 2)))
  }
  
  # Null model (for Psuedo-R-squared calculation later)
  
  mod.NULL <- glm(Pres.Abs ~ 1, 
              data = und.presence.SPEC, family = "binomial", na.action = na.fail)
  
  # Storing output
  
  Mods.list.nofi <- list()
  
  for(i in 1:length(top.mods)) {
    Mods.list.nofi[[i]] <- data.frame(
                      Species = levels(species.list)[S],  
                      L.Occ = sum(num.burns["1", , "Legacy"]), 
                      R.Occ = sum(num.burns["1", , "Resurvey"]), 
                      Type = "Unavg", 
                      deltaAIC = avg.globnofi$msTable$delta[i], 
                      Weight = avg.globnofi$msTable$weight[i],
                      Rsquared = 1 - avg.globnofi$msTable$logLik[i]/logLik(mod.NULL)[1],
                      Intercept = top.mods.coeff$`(Intercept)`[i],
                      Data.Type = top.mods.coeff$Data.TypeResurvey[i], #TODO change to Data.Type
                      Elevation.m = top.mods.coeff$Data.TypeResurvey[i],
                      Elevation.m2 = top.mods.coeff$Elevation.m2[i],
                      DataType.Elevation.m = top.mods.coeff$Data.TypeResurvey:Elevation.m[i],
  }
}  

  
  
  
  
