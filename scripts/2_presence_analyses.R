# Created: Dec. 11, 2020
# Updated: Dec. 11, 2020

# This script will be used to undertake the PRESENCE analyses

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

##TODO add packages here


#### STEP 1: Import data ####

und.presence <- read.csv("data/1_presence_with_fires.csv", header = TRUE, na.strings = "")

