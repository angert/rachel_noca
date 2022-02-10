
# Created: Dec. 15, 2021 from script 8
# Updated: Dec. 15, 2021

# This script will be used to create 4 forest plots:
# --> PART 1: The model-averaged coefficients
# --> PART 2: Percent +/- out of total datasets

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(ggplot2)
library(tidyverse)
library(forestplot)

####### PART 1: LOAD AND SUMMARIZE AVERAGED COEFFICIENTS FROM EACH RAREFACTION #########

## Step 1: Load coefficient data

coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)
coeff.ALLDAT[coeff.ALLDAT$Species == "EPAN", 1] <- paste("CHAN") # Correcting taxonomy issue
coeff.ALLDAT[is.na(coeff.ALLDAT)] <- 0 # Hard code absent coeffs as 0 before averaging

coeff.fire <- #split by fire vs no-fire
  coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "Yes" & coeff.ALLDAT$Type == "Avg", c(1:12, 16:21)]
coeff.nofire <- coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "No" & coeff.ALLDAT$Type == "Avg", c(1:12, 13:15)]

## Step 2: Summarize mean, lower and upper CI of each coefficient across the rarefactions
means.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.5)), .names = "mean_{.col}"))
lowers.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.025)), .names="lower_{.col}"))
uppers.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.975)), .names="upper_{.col}"))

all.fire <- left_join(left_join(means.fire, lowers.fire), uppers.fire)

ACMI <- all.fire %>% filter(Species=="ACMI")

ACMI.mean <- ACMI %>% select(starts_with("mean")) %>% as.data.frame()
ACMI.lower <- ACMI %>% select(starts_with("lower")) 
ACMI.upper <- ACMI %>% select(starts_with("upper")) 

base.data <- tibble(mean=ACMI.mean,
                    lower=ACMI.lower,
                    upper=ACMI.upper,
                    study=c("Elevation", 
                            "Elevation2", 
                            "Burned",
                            "Unburned",
                            "Elevation*Burned",
                            "Elevation*Unburned",
                            "Elevation2*Burned", 
                            "Elevation2*Unburned"))

header <- tibble(study=c("Predictor"), 
                 summary=FALSE)

output_df <- bind_rows(header, base.data)

output_df %>% forestplot(labeltext=study,
                         is.summary=FALSE,
                         xlog=FALSE)
