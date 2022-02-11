
# Created: Dec. 15, 2021 from script 8
# Updated: Feb. 10, 2022

# This script will be used to create 4 forest plots:
# --> PART 1: The model-averaged coefficients
# --> PART 2: Percent +/- out of total datasets

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(ggplot2)
library(tidyverse)

## Step 1: Load coefficient data

coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)
coeff.ALLDAT[coeff.ALLDAT$Species == "EPAN", 1] <- paste("CHAN") # Correcting taxonomy issue
coeff.ALLDAT[is.na(coeff.ALLDAT)] <- 0 # Hard code absent coeffs as 0 before averaging

coeff.fire <- #split by fire vs no-fire
  coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "Yes" & coeff.ALLDAT$Type == "Avg", c(1:12, 16:21)]
coeff.nofire <- coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "No" & coeff.ALLDAT$Type == "Avg", c(1:12, 13:15)]

## Step 2: Summarize mean, lower and upper CI of each coefficient across the rarefactions (FIRE SPECIES)
means.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.5)))) %>% #, .names = "mean_{.col}" 
  mutate(param="mean")
lowers.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.025)))) %>% #, .names="lower_{.col}" 
  mutate(param="lower")
uppers.fire <- coeff.fire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Elevation.m2.Res.Unburn.fi, ~ unname(quantile(.x, 0.975)))) %>% #, .names="upper_{.col}" 
  mutate(param="upper")

## Step 3: reshape for graphing
all.fire <- rbind(means.fire, lowers.fire, uppers.fire) %>% 
  pivot_longer(!c(Species, param), names_to="Parameter", values_to="Estimate") %>% 
  pivot_wider(names_from=param, values_from="Estimate")

## Step 4: Graph fire species

# order of parameters along y axis
order.list.fire <- c("Elevation.m", 
           "Elevation.m2", 
           "Resurvey.Burned.fi", 
           "Resurvey.Unburned.fi",
           "Elevation.m.Res.Burn.fi", 
           "Elevation.m.Res.Unburn.fi", 
           "Elevation.m2.Res.Burn.fi", 
           "Elevation.m2.Res.Unburn.fi")

all.fire$Parameter <- factor(all.fire$Parameter, levels = rev(order.list.fire))

# tick labels for y axis
vars.fire <- c("Elevation", 
          expression("Elevation" ^ 2), 
          "Burned",
          "Unburned",
          "Elevation * Burned",
          "Elevation * Unburned",
          expression("Elevation" ^ 2 * " * Burned"), 
          expression("Elevation" ^ 2 * " * Unburned"))

# faceted plot
forestplot.fire <- ggplot(dat=all.fire, aes(y=Parameter, x=mean, xmin=lower, xmax=upper)) +
  facet_wrap(~Species) +
  geom_point(cex=3) + 
  geom_errorbarh(height=0.3) + 
  geom_vline(xintercept=0, linetype="dotted") +
  scale_y_discrete(labels=rev(vars.fire)) +
  xlab("Estimate") +
  theme_classic() 

ggsave("figures/forestplot_coeffs_fire.pdf", forestplot.fire, width=12, height=8)


## Step 5: Repeat steps 2-4 for no-fire species

means.nofire <- coeff.nofire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Data.Type.Elevation.m2.nofi, ~ unname(quantile(.x, 0.5)))) %>%  
  mutate(param="mean")
lowers.nofire <- coeff.nofire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Data.Type.Elevation.m2.nofi, ~ unname(quantile(.x, 0.025)))) %>%  
  mutate(param="lower")
uppers.nofire <- coeff.nofire %>% 
  group_by(Species) %>% 
  summarise(across(.cols = Elevation.m:Data.Type.Elevation.m2.nofi, ~ unname(quantile(.x, 0.975)))) %>%  
  mutate(param="upper")

all.nofire <- rbind(means.nofire, lowers.nofire, uppers.nofire) %>% 
  pivot_longer(!c(Species, param), names_to="Parameter", values_to="Estimate") %>% 
  pivot_wider(names_from=param, values_from="Estimate")

order.list.nofire <- c("Elevation.m", 
                "Elevation.m2", 
                "Data.Type.nofi",
                "Data.Type.Elevation.m.nofi", 
                "Data.Type.Elevation.m2.nofi")

all.nofire$Parameter <- factor(all.nofire$Parameter, levels = rev(order.list.nofire))

# tick labels for y axis
vars.nofire <- c("Elevation", 
          expression("Elevation" ^ 2), 
          "Year",
          "Elevation * Year",
          expression("Elevation" ^ 2 * " * Year"))

# faceted plot
forestplot.nofire <- ggplot(dat=all.nofire, aes(y=Parameter, x=mean, xmin=lower, xmax=upper)) +
  facet_wrap(~Species) +
  geom_point(cex=2) + 
  geom_errorbarh(height=0.3) + 
  geom_vline(xintercept=0, linetype="dotted") +
  scale_y_discrete(labels=rev(vars.nofire)) +
  xlab("Estimate") +
  theme_classic() 

ggsave("figures/forestplot_coeffs_nofire.pdf", forestplot.nofire, width=12, height=8)

