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

for(S in 1:length(species.list))

