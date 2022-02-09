
# Created: Dec. 15, 2021 from script 8
# Updated: Dec. 15, 2021

# This script will be used to create 4 forest plots:
# --> PART 1: The model-averaged coefficients
# --> PART 2: Percent +/- out of total datasets

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(ggplot2)
library(tidyverse)

####### PART 1: AVERAGE AVERAGED COEFFICIENTS #########

## Step 1: Loading and tidying average coefficient data

coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)
coeff.ALLDAT[coeff.ALLDAT$Species == "EPAN", 1] <- paste("CHAN") # Correcting taxonomy issue
coeff.fire <- 
  coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "Yes" & coeff.ALLDAT$Type == "Avg",
                               c(1:12, 16:21)]
coeff.nofire <- coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "No" & coeff.ALLDAT$Type == "Avg",
                               c(1:12, 13:15)]



