# Created: May 10, 2021

# This script will be used to create a violin plot of elevation vs fire history

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(ggplot2)

####### PART 1: Load data #########

load("data/list.fires.Rda")
burn.data <- list.fires[list.fires$Data.Type == "Resurvey",]
burn.data$Elevation.m <- as.numeric(burn.data$Elevation.m)



violin.plot.burns <- ggplot(burn.data, aes(x = Fires, y = Elevation.m)) +
  geom_violin() +
  theme_classic() +
  xlab("") +
  # scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(100, 2200) +
  ylab("Elevation (m)") +
  theme(legend.position="none", text = element_text(size = 16)) +
  stat_summary(fun=mean, geom="point", cex=2)


