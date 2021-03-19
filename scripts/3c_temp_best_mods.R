#TODO: Add global model to loop output
# Created: Mar. 18, 2021

# This script will be used to undertake part 3 of the PRESENCE analyses (modeling)
# Use this script to produce the set of best models

# This script accommodates the following species-specific issues:
# --> Discard species for whom 2 or more models threw warnings
# # ---> COST, LUPE, PHEM, RHAL, VAAL, VADE
# --> Exclude the most-complex model (X * elev^2) for species in which this model threw warnings
# # ---> HODI, VAME*
# --> For species that flip-flopped between yes/no fire, use "majority rules" to decide
#     which framework to use
# # ---> No fire: AMAL, SPBE
# # ---> Fire: ARUV, CARU, VAME*
# --> Exclude problematic sets when remainder threw no warnings 
# # ---> CAME, GAOV, HIAL, RULA, TRBO
# As well as the following general changes:
# --> Switch elevation^2 term to poly(elevation)

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(MuMIn)
library(plyr)


# List of coefficients between fire and non-fire model framework
#TODO: confusing
coeff.all <-c("(Intercept)",
              "Elevation.m.poly", 
              "Elevation.m2.poly", 
              "New.Data.TypeResurvey.Burned", 
              "New.Data.TypeResurvey.Unburned", 
              "Elevation.m.poly:New.Data.TypeResurvey.Burned", 
              "Elevation.m.poly:New.Data.TypeResurvey.Unburned", 
              "Elevation.m2.poly:New.Data.TypeResurvey.Burned", 
              "Elevation.m2.poly:New.Data.TypeResurvey.Unburned",
              "Data.TypeResurvey",
              "Data.TypeResurvey:Elevation.m.poly",
              "Data.TypeResurvey:Elevation.m2.poly")

#### STEP 1: Import data #### Takes ~ 5 sec to run

# To analyze un-rarefied data (exclude 100x loop):
# und.presence <- read.csv("data/1_presence_fires_unrarefied", header = TRUE, na.strings = "")

# To analyze rarefied data (as list of dataframes named rare.ALL):
load("data/rare.ALL.Rda") # Run time 5 sec

# Import for both data types:
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS" &
                                      !shifts$Species.Code=="COST" &
                                      !shifts$Species.Code=="LUPE" &
                                      !shifts$Species.Code=="PHEM" &
                                      !shifts$Species.Code=="RHAL" &
                                      !shifts$Species.Code=="VAAL" &
                                      !shifts$Species.Code=="VADE"] #removing problematic species
species.list <- factor(species.list)
warn.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_warnings.csv", header = TRUE)

# Formula to correct erroneous burn coding. Run time 3 sec
for(D in 1:100) {
  rare.ALL[[D]]$Fires[rare.ALL[[D]]$Data.Type == "Legacy"] <- rep("Unburned")
  rare.ALL[[D]]$New.Data.Type <- paste(rare.ALL[[D]]$Data.Type, rare.ALL[[D]]$Fires, sep = ".")
}


#### STEP 2: Loop to analyze presence data ####

# Can be run as a loop outputting all species, or S can be modified to isolated specific species. Check number here:
(numbered.species <- data.frame(Species=species.list, No.=rep(1:length(species.list))))
species.with.fire <- numbered.species[numbered.species$Species == "ACMI" |
                                        numbered.species$Species == "ARUV" |
                                        numbered.species$Species == "CARU" |
                                        numbered.species$Species == "CEVE" |
                                        numbered.species$Species == "EPAN" |
                                        numbered.species$Species == "PAMY" |
                                        numbered.species$Species == "VAME", ]
species.without.fire <- numbered.species[!numbered.species$Species 
                                         %in% species.with.fire$Species, ]
# Exclude D loop to run for only one dataset (e.g. unrarefied data)

coeff.ALLDAT <- list() # Store coefficient outputs
framework.ALLDAT <- list () # Store record of which decision framework was used

for(D in 1:100) { #RUN TIME: 5 min
  
  coeff.ALLSPEC <- list()
  avg.confint.ALLSPEC <- list() 
  framework.ALLSPEC <- list()
  und.presence <- rare.ALL[[D]]
  und.presence$Elevation.m <- as.numeric(und.presence$Elevation.m)
  und.presence$New.Data.Type <- factor(und.presence$New.Data.Type)
  und.presence$Fires <- as.factor(und.presence$Fires)
  und.presence$Data.Type <- as.factor(und.presence$Data.Type)
  
  # Create dataframe of warnings for this dataset
  warn.dataset <- subset(warn.ALLDAT, Dataset == D,
                         select = c(Species, Dataset, Fire.Included, Has_warning))
  
  for(S in 1:length(species.list)) {
    
    # Create subset for species of interest S
    und.presence.SPEC <- subset(und.presence, Species.Code == levels(species.list)[S])
    und.presence.SPEC$New.Data.Type <- relevel(und.presence.SPEC$New.Data.Type, 
                                               ref="Legacy.Unburned")
    und.presence.SPEC$Fires <- relevel(und.presence.SPEC$Fires, ref="Unburned")
    und.presence.SPEC$Data.Type <- relevel(und.presence.SPEC$Data.Type, ref="Legacy")
    #und.presence.SPEC$Elevation.m2 <- und.presence.SPEC$Elevation.m^2
    # Added Mar. 1: Replace Elevation.m with poly()
    und.presence.SPEC$Elevation.m.poly <- poly(und.presence.SPEC$Elevation.m, 2)[ , 1]
    und.presence.SPEC$Elevation.m2.poly <- poly(und.presence.SPEC$Elevation.m, 2)[ , 2]
    und.presence.SPEC <- und.presence.SPEC[complete.cases(und.presence.SPEC), ] #Just in case
    
    # Create subset of warnings for species of interest S
    warn.SPEC <- subset(warn.dataset, Species == levels(species.list)[S])
    
    # Create empty data frame to record decision framework
    framework.SPEC <- data.frame(Dataset = paste(D), 
                                 Species = paste(levels(species.list)[S]),
                                 Forced.Fire = paste(NA),
                                 Forced.No.Fire = paste(NA),
                                 Forced.Simpler.Mod = paste(NA),
                                 Discard.Later = paste(NA),
                                 One.Top.Mod = paste(NA))
    
    # Emptying out previous model objects
    mod.globfi <- NULL
    mod.globnofi <- NULL
    mod.globfi.reduced <- NULL
    mod.globnofi.reduced <- NULL
    dredge.globfi <- NULL
    dredge.globnofi <- NULL
    top.mods.coeff <- NULL
    
    # Did the species occur in burned plots 5+ times?
    num.burns <- 
      table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type)
    
    #### START OF MODEL FRAMEWORK #### 
    
    # If burn > 5 in >50% of datasets, include fire as a predictor in global model (New.Data.Type):
    if(levels(species.list)[S] %in% species.with.fire$Species == TRUE) {
      
      # Did we force this D & S to run in the fire framework?
      if(levels(factor(warn.SPEC$Fire.Included)) == "No") {
        framework.SPEC$Forced.Fire <- paste("Yes") # Record
      } else {
        framework.SPEC$Forced.Fire <- paste("No") # Record
      }
      
      # Was there a warning associated with this D & S?
      if("TRUE" %in% levels(factor(warn.dataset$Has_warning[warn.dataset$Species 
                                                            == levels(species.list)[S]]))) {
        
        if(levels(species.list)[S] == "VAME") { # No elev^2 * year-burn
          mod.globfi.reduced <- glm(Pres.Abs ~ (Elevation.m.poly + Elevation.m2.poly) + 
                                      New.Data.Type + Elevation.m.poly:New.Data.Type, 
                                    data = und.presence.SPEC, family = "binomial", na.action = na.fail)
          dredge.globfi.reduced <- dredge(mod.globfi.reduced, rank = AIC, subset = 
                                            dc(Elevation.m.poly, Elevation.m2.poly))
          
          # Coefficients:
          top.mods.coeff <- as.data.frame(coef(subset(dredge.globfi.reduced, delta == 0)))
          top.mods.coeff$logLik <- dredge.globfi.reduced$logLik[dredge.globfi.reduced$delta == 0]
          
          framework.SPEC$Forced.Simpler.Mod <- paste("Yes") # Record
          
        } else {
          framework.SPEC$Discard.Later <- paste("Yes") # Record
          # Create empty DFs
          top.mods.coeff <- data.frame(logLik = rep(NA, 1))
        }
        
        
      } else { # Run as normal
        mod.globfi <- glm(Pres.Abs ~ (Elevation.m.poly + Elevation.m2.poly) * New.Data.Type, 
                          data = und.presence.SPEC, family = "binomial", na.action = na.fail)
        dredge.globfi <- dredge(mod.globfi, rank = AIC, subset = 
                                  dc(Elevation.m.poly, Elevation.m2.poly) &&
                                  dc(New.Data.Type:Elevation.m.poly, 
                                     New.Data.Type:Elevation.m2.poly) &&
                                  dc(Elevation.m.poly:New.Data.Type, 
                                     Elevation.m2.poly:New.Data.Type))
        
        # Storing coefficients:
        top.mods.coeff <- as.data.frame(coef(subset(dredge.globfi, delta == 0)))
        top.mods.coeff$logLik <- dredge.globfi$logLik[dredge.globfi$delta == 0]
        
      }
    }
    
    #If burn < 5 in >50% of datasets, exclude fire from global model:
    if(levels(species.list)[S] %in% species.with.fire$Species == FALSE) {
      
      # Did we force this D & S to run in the no-fire framework?
      if(levels(factor(warn.SPEC$Fire.Included)) == "Yes") {
        framework.SPEC$Forced.No.Fire <- paste("Yes") # Record
      } else {
        framework.SPEC$Forced.No.Fire <- paste("No") # Record
      }
      
      # Was there a warning associated with this D & S?
      if("TRUE" %in% levels(factor(warn.dataset$Has_warning[warn.dataset$Species 
                                                            == levels(species.list)[S]]))) {
        if(levels(species.list)[S] == "HODI") { # No elev^2 * year
          mod.globnofi.reduced <- glm(Pres.Abs ~ Elevation.m.poly + Elevation.m2.poly + 
                                        Data.Type + Elevation.m.poly:Data.Type, 
                                      data = und.presence.SPEC, family = "binomial", 
                                      na.action = na.fail)
          dredge.globnofi.reduced <- dredge(mod.globnofi.reduced, rank = AIC, subset = 
                                              dc(Elevation.m.poly, Elevation.m2.poly))
          
          # Coefficients:
          top.mods.coeff <- as.data.frame(coef(subset(dredge.globnofi.reduced, delta == 0)))
          top.mods.coeff$logLik <- dredge.globnofi.reduced$logLik[dredge.globnofi.reduced$delta == 0]
          
          framework.SPEC$Forced.Simpler.Mod <- paste("Yes") # Record
          
        } else {
          framework.SPEC$Discard.Later <- paste("Yes") # Record
          # Create empty DFs
          top.mods.coeff <- data.frame(logLik = rep(NA, 1))
          
        }
        
      } else { # Run as normal
        mod.globnofi <- glm(Pres.Abs ~ Data.Type * (Elevation.m.poly + Elevation.m2.poly), 
                            data = und.presence.SPEC, family = "binomial", na.action = na.fail) 
        dredge.globnofi <- dredge(mod.globnofi, rank = AIC, subset = 
                                    dc(Elevation.m.poly, Elevation.m2.poly) &&
                                    dc(Data.Type:Elevation.m.poly, Data.Type:Elevation.m2.poly))
        
        # Coefficients:
        top.mods.coeff <- as.data.frame(coef(subset(dredge.globnofi, delta == 0)))
        top.mods.coeff$logLik <- dredge.globnofi$logLik[dredge.globnofi$delta == 0]
      }
    }
    
    
    
    #### END OF MODEL FRAMEWORK ####
    # Output: 3 DFs named top.mods.coeff, avg.mods.coeff, avg.mods.confint
    
    
    # Null model (for Psuedo-R-squared calculation later)
    mod.NULL <- glm(Pres.Abs ~ 1, 
                    data = und.presence.SPEC, family = "binomial", na.action = na.fail)
    
    # Adding in missing coefficient headers
    for(C in 1:length(coeff.all)) {
      if(!coeff.all[C] %in% colnames(top.mods.coeff)) {
        top.mods.coeff[, coeff.all[C]] <- rep(NA, times = nrow(top.mods.coeff))
      }
    }
    
    ## Storing top model coefficients
    
    Mods.list <- list()
    
    for(i in 1:nrow(top.mods.coeff)) {
      Mods.list[[i]] <- data.frame(
        Species = levels(species.list)[S], 
        Dataset = D,
        L.Occ = sum(num.burns["1", , "Legacy"]), 
        R.Occ = sum(num.burns["1", , "Resurvey"]), 
        Fire.Included = ifelse(levels(species.list)[S] 
                               %in% species.with.fire$Species == TRUE, "Yes", "No"),
        Type = "Unavg", 
        Rsquared = 1 - top.mods.coeff$logLik[i] / as.numeric(logLik(mod.NULL)),
        Intercept = top.mods.coeff$`(Intercept)`[i],
        Elevation.m = top.mods.coeff$Elevation.m.poly[i],
        Elevation.m2 = top.mods.coeff$Elevation.m2.poly[i],
        Data.Type.nofi = top.mods.coeff$Data.TypeResurvey[i],
        Data.Type.Elevation.m.nofi = top.mods.coeff$`Data.TypeResurvey:Elevation.m.poly`[i],
        Data.Type.Elevation.m2.nofi = top.mods.coeff$`Data.TypeResurvey:Elevation.m2.poly`[i],
        Resurvey.Burned.fi = 
          top.mods.coeff$New.Data.TypeResurvey.Burned[i],
        Resurvey.Unburned.fi = 
          top.mods.coeff$New.Data.TypeResurvey.Unburned[i],
        Elevation.m.Res.Burn.fi = 
          top.mods.coeff$`Elevation.m.poly:New.Data.TypeResurvey.Burned`[i],
        Elevation.m.Res.Unburn.fi = 
          top.mods.coeff$`Elevation.m.poly:New.Data.TypeResurvey.Unburned`[i],
        Elevation.m2.Res.Burn.fi = 
          top.mods.coeff$`Elevation.m2.poly:New.Data.TypeResurvey.Burned`[i],
        Elevation.m2.Res.Unburn.fi = 
          top.mods.coeff$`Elevation.m2.poly:New.Data.TypeResurvey.Unburned`[i],
        row.names = NULL)
    }
    
    coeff.ALLSPEC[[S]] <- ldply(Mods.list, data.frame)
    
    ## Storing record of framework
    
    framework.ALLSPEC[[S]] <- framework.SPEC
    
  }  
  
  #Collapse ALLSPEC lists into single dataframe, store that df as part of ALLDAT list
  
  coeff.ALLDAT[[D]] <- ldply(coeff.ALLSPEC, data.frame)
  framework.ALLDAT[[D]] <- ldply(framework.ALLSPEC, data.frame)
  
  #### END OF SPECIES LOOP
  
  if(D == 100) system("say Your loop is done")
  
}

#### END OF DATASET LOOP

# Collate ALLDAT lists into one big DF

coeff.ALLDAT.finaldf <- ldply(coeff.ALLDAT, data.frame)
framework.ALLDAT.finaldf <- ldply(framework.ALLDAT, data.frame)

#TODO remove error-laden coeffs


# Store output as CSV

write.csv(coeff.ALLDAT.finaldf, 
          file = "data/3c_top_mod_coefficients.csv", 
          row.names = FALSE)






