# Created: Dec. 11, 2020
# Updated: Dec. 11, 2020

# This script will be used to undertake the PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

##TODO add packages here


#### STEP 1: Import data ####

und.presence <- read.csv("data/1_presence_with_fires.csv", header = TRUE, na.strings = "")
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)


#### STEP 2: Loop to create presence analyses. Run time ~ 2 sec ####

# Can be run as a loop outputting all species, or S can be modified to isolated specific species. Check number here:
(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))


coeff.SPEC<-list()

for(S in 1:length(species.list)) {
  
  # Create subset for species of interest S
  und.presence.SPEC = subset(und.presence, Species.Code == levels(species.list)[S])
  und.presence.SPEC$Fires <- relevel(und.presence.SPEC$Fires, ref="Unburned")
  und.presence.SPEC$Elevation.m2 <- und.presence.SPEC$Elevation.m^2 #TODO better than poly()?
  und.presence.SPEC <- und.presence.SPEC[complete.cases(und.presence.SPEC), ] #Just in case
  
  # Did the species occur in burned plots 5+ times?
  num.burns <- table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type)
  
  # If yes, include fire as a predictor in global model:
  if(num.burns[2,2,1] >= 5 | num.burns[2,2,2] >= 5) {
    mod.globfi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2) * Fires, data = und.presence.SPEC, family = "binomial", na.action = na.fail) #Global fire model
    options(warn = 1)
    dredge.globfi <- dredge(mod.globfi, rank = AIC, subset = 
                              dc(Elevation.m, Elevation.m2) &&
                              dc(Data.Type:Elevation.m, Data.Type:Elevation.m2) &&
                              dc(Elevation.m:Fires, Elevation.m2:Fires) &&
                              dc(Data.Type:Elevation.m:Fires, Data.Type:Elevation.m2:Fires), 
                            trace = 1)
    avg.globfi <- model.avg(dredge.globfi, subset = delta <= 2)
  }
  
  # If no, exclude fire from global model:
  if(num.burns[2,2,2] < 5 | num.burns[2,2,1] < 5) {
    mod.globnofi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2), data = und.presence.SPEC, family = "binomial", na.action = na.fail) #global w/o fire
    dredge.globnofi <- dredge(mod.globnofi, rank = AIC, subset = 
                                dc(Elevation.m, Elevation.m2) &&
                                dc(Data.Type:Elevation.m, Data.Type:Elevation.m2) &&
                                dc(Elevation.m:Fires, Elevation.m2:Fires) &&
                                dc(Data.Type:Elevation.m:Fires, Data.Type:Elevation.m2:Fires), 
                              trace = 1)
    avg.globnofi <- model.avg(dredge.globnofi, subset = delta <= 2)
  }

