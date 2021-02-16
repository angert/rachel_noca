# Created: Feb. 16, 2020

# This script is Amy's quick attempt at visualizing raw patterns in rarefied data; see script 6 for visualizing model output

library(tidyverse)
library(Hmisc) #for mean_sdl function to put mean+/-SD on violin plots

# Load rarefied data (as list of dataframes named rare.ALL):
load("data/rare.ALL.Rda")

# Import for both data types:
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

# Set up empty matrices to store values
el.mins.leg <- matrix(nrow=100,ncol=42)
el.maxs.leg <- matrix(nrow=100,ncol=42)
el.meds.leg <- matrix(nrow=100,ncol=42)
el.mins.res <- matrix(nrow=100,ncol=42)
el.maxs.res <- matrix(nrow=100,ncol=42)
el.meds.res <- matrix(nrow=100,ncol=42)

# Clunky for-loops across rarefied datasets by species
for(D in 1:100) {
  und <- rare.ALL[[D]]
  und$Data.Type <- as.factor(und$Data.Type)
  und$Elevation.m <- as.numeric(und$Elevation.m)
    for(S in 1:length(species.list)) {
    und.SPEC <- und %>% 
      filter(Species.Code == levels(species.list)[S])
    und.presence.SPEC.leg <- und.SPEC %>% 
      filter(Data.Type=="Legacy" & Pres.Abs==1) %>% 
      mutate(el.min.leg = min(Elevation.m),
             el.max.leg = max(Elevation.m),
             el.med.leg = median(Elevation.m))
    und.presence.SPEC.res <- und.SPEC %>% 
      filter(Data.Type=="Resurvey" & Pres.Abs==1) %>% 
      mutate(el.min.res = min(Elevation.m),
             el.max.res = max(Elevation.m),
             el.med.res = median(Elevation.m))
    el.mins.leg[D,S] <- und.presence.SPEC.leg$el.min.leg[1]
    el.maxs.leg[D,S] <- und.presence.SPEC.leg$el.max.leg[1]
    el.meds.leg[D,S] <- und.presence.SPEC.leg$el.med.leg[1]
    el.mins.res[D,S] <- und.presence.SPEC.res$el.min.res[1]
    el.maxs.res[D,S] <- und.presence.SPEC.res$el.max.res[1]
    el.meds.res[D,S] <- und.presence.SPEC.res$el.med.res[1]
    }
}

# collapse to averages across rarefied replicates
el.mins.leg.means <- as.data.frame(el.mins.leg) %>% summarise(across(starts_with("V"),mean))
el.mins.res.means <- as.data.frame(el.mins.res) %>% summarise(across(starts_with("V"),mean))
el.maxs.leg.means <- as.data.frame(el.maxs.leg) %>% summarise(across(starts_with("V"),mean))
el.maxs.res.means <- as.data.frame(el.maxs.res) %>% summarise(across(starts_with("V"),mean))
el.meds.leg.means <- as.data.frame(el.meds.leg) %>% summarise(across(starts_with("V"),mean))
el.meds.res.means <- as.data.frame(el.meds.res) %>% summarise(across(starts_with("V"),mean))

#reshape and join into one frame
el.mins.leg.tall <- gather(el.mins.leg.means, "species", "min.leg", 1:42)
el.mins.res.tall <- gather(el.mins.res.means, "species", "min.res", 1:42)
el.maxs.leg.tall <- gather(el.maxs.leg.means, "species", "max.leg", 1:42)
el.maxs.res.tall <- gather(el.maxs.res.means, "species", "max.res", 1:42)
el.meds.leg.tall <- gather(el.meds.leg.means, "species", "med.leg", 1:42)
el.meds.res.tall <- gather(el.meds.res.means, "species", "med.res", 1:42)

rarefied.change <- left_join(left_join(left_join(left_join(left_join(el.mins.leg.tall, el.mins.res.tall),el.maxs.leg.tall), el.maxs.res.tall), el.meds.leg.tall), el.meds.res.tall)

# calculate range changes
rarefied.change <- rarefied.change %>% 
  mutate(rear.change = min.res - min.leg,
         med.change = med.res - med.leg,
         lead.change = max.res - max.leg,
         span.change = (max.res-min.res) - (max.leg - min.leg))

# reshape for plotting
rarefied.change.tall <- rarefied.change %>% 
  select(rear.change, med.change, lead.change, span.change) %>% 
  gather("edge", "change", 1:4)

# approximate colors from Rumpf et al. 2018 PNAS (doi: 10.1073/pnas.1713936115)
col.pal <- c("lightcoral", "goldenrod1", "slategray2", "bisque4")

violin.plot <- ggplot(rarefied.change.tall, aes(x=edge, y=change, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  scale_color_manual(values=col.pal) +
  scale_fill_manual(values=alpha(col.pal, 0.3)) +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Low\nedge", "Range\ncenter", "Upper\nedge", "Range\nsize")) +
  ylab(c("Elevational change (m)\n1980-2015")) +
  theme(legend.position="none")
  #stat_summary(fun.data=mean_sdl, geom="pointrange")

ggsave("figures/violin_rarefied.pdf", violin.plot, device="pdf", width=6, height=5)
