# Created: May 10, 2021

# This script will be used to create a violin plot of elevation vs fire history

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(tidyverse)

####### PART 1: All plots regardless of species #########

load("data/list.fires.Rda")
burn.data <- list.fires[list.fires$Data.Type == "Resurvey",]
burn.data$Elevation.m <- as.numeric(burn.data$Elevation.m)

violin.plot.burns <- ggplot(burn.data, aes(x = Fires, y = Elevation.m)) +
  geom_violin() +
  theme_classic() +
  xlab("") +
  ylim(100, 2200) +
  ylab("Elevation (m)") +
  theme(legend.position="none", text = element_text(size = 16)) +
  stat_summary(fun=mean, geom="point", cex=2)


####### PART 2: By species #########

species.list <- read.csv("data/3b_new_coefficients.csv", header = TRUE) %>% 
  filter(Fire.Included=="Yes") %>% 
  select(Species) %>% 
  distinct()

dat <- read_csv("data/1_presence_fires_unrarefied.csv") %>% 
  filter(Pres.Abs==1) %>% 
  filter(Data.Type=="Resurvey") %>% 
  filter(Species.Code %in% species.list$Species)

violin.plot.burns.spp <- ggplot(dat, aes(x = Fires, y = Elevation.m)) +
  facet_wrap(~Species.Code) +
  geom_violin() +
  theme_classic() +
  xlab("") +
  ylim(100, 2200) +
  ylab("Elevation (m)") +
  theme(legend.position="none", text = element_text(size = 16)) +
  stat_summary(fun=mean, geom="point", cex=2)

