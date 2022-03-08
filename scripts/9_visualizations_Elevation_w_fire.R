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

####### PART 3: By MAP #########

### read in climate files from ClimateWNA
annual <- read_csv("data/All_Plots_RNW_1970-2015YT.csv")

### merge elevation and turn it to numeric
plot.names <- load("~/data/plot.names.Rda") #why isn't this loading??
# loading it manually by double-clicking from finder window

annual <- left_join(annual, plot.names, by=c("ID1" = "Plot.2015"))
annual$Elevation.m <- as.numeric(annual$Elevation.m)

### merge onto fire status
clim_fire <- left_join(annual, burn.data, by=c("ID1" = "Plot")) %>% 
  filter(Year==2015) %>% 
  filter(!is.na(Fires))

violin.plot.MAP <- ggplot(clim_fire, aes(x = Fires, y = MAP)) +
  geom_violin() +
  theme_classic() +
  xlab("") +
  ylab("Mean annual precipitation (mm)") +
  theme(legend.position="none", text = element_text(size = 16)) +
  stat_summary(fun=mean, geom="point", cex=2)

MAP.elev <- ggplot(clim_fire, aes(x=Elevation.m.x, y=MAP)) +
  geom_point() +
  xlab("Elevation (m)") +
  ylab("Mean annual precipitation (mm)") +
  theme_classic()

