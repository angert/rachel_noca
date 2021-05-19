# Created: Feb. 16, 2021
# Modified: May 03, 2021

#### This script is Amy's quick attempt at visualizing raw patterns in rarefied data; see script 6 for visualizing model output

### Load libraries
library(tidyverse)
library(Hmisc) #for mean_sdl function to put mean+/-SD on violin plots
library(cowplot) #for multipanel figures

### Load data
# rarefied data (as list of dataframes named rare.ALL):
load("data/rare.ALL.Rda")

### Species list

# If all species combined, regardless of model type
#load("data/Species.List.Rda") #TODO this file was made in an undocumented step
#species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
#species.list <- factor(species.list)

# If separating fire and no-fire species
coeff.avgs <- read.csv("data/3b_new_coefficients.csv", header = TRUE) %>% filter(Type=="Avg") 

# split into species modeled with fire vs without
coeffs.fire <- coeff.avgs %>% filter(Fire.Included=="Yes")
coeffs.nofire <- coeff.avgs %>% filter(Fire.Included=="No")

species.list.fire <- coeffs.fire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))

species.list.nofire <- coeffs.nofire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))

### Set up empty matrices to store values
el.mins.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.meds.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.mins.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.meds.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])

el.mins.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.meds.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.mins.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.meds.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])

### Clunky for-loops across rarefied datasets by species

## no-fire species
for(D in 1:100) {
  und <- rare.ALL[[D]]
  und$Data.Type <- as.factor(und$Data.Type)
  und$Elevation.m <- as.numeric(und$Elevation.m)
    for(S in 1:dim(species.list.nofire)[1]) {
    species <- as.list(species.list.nofire[S,1])
    und.SPEC <- und %>% 
      filter(Species.Code == species) %>% 
      droplevels()
    und.presence.SPEC.leg <- und.SPEC %>% 
      filter(Data.Type=="Legacy" & Pres.Abs==1) %>% 
      mutate(el.min.leg.nofire = min(Elevation.m),
             el.max.leg.nofire = max(Elevation.m),
             el.med.leg.nofire = median(Elevation.m))
    und.presence.SPEC.res <- und.SPEC %>% 
      filter(Data.Type=="Resurvey" & Pres.Abs==1) %>% 
      mutate(el.min.res.nofire = min(Elevation.m),
             el.max.res.nofire = max(Elevation.m),
             el.med.res.nofire = median(Elevation.m))
    el.mins.leg.nofire[D,S] <- und.presence.SPEC.leg$el.min.leg.nofire[1]
    el.maxs.leg.nofire[D,S] <- und.presence.SPEC.leg$el.max.leg.nofire[1]
    el.meds.leg.nofire[D,S] <- und.presence.SPEC.leg$el.med.leg.nofire[1]
    el.mins.res.nofire[D,S] <- und.presence.SPEC.res$el.min.res.nofire[1]
    el.maxs.res.nofire[D,S] <- und.presence.SPEC.res$el.max.res.nofire[1]
    el.meds.res.nofire[D,S] <- und.presence.SPEC.res$el.med.res.nofire[1]
    }
}

## fire species
for(D in 1:100) {
  und <- rare.ALL[[D]]
  und$Data.Type <- as.factor(und$Data.Type)
  und$Elevation.m <- as.numeric(und$Elevation.m)
  for(S in 1:dim(species.list.fire)[1]) {
    species <- as.list(species.list.fire[S,1])
    und.SPEC <- und %>% 
      filter(Species.Code == species) %>% 
      droplevels()
    und.presence.SPEC.leg <- und.SPEC %>% 
      filter(Data.Type=="Legacy" & Pres.Abs==1) %>% 
      mutate(el.min.leg.fire = min(Elevation.m),
             el.max.leg.fire = max(Elevation.m),
             el.med.leg.fire = median(Elevation.m))
    und.presence.SPEC.res <- und.SPEC %>% 
      filter(Data.Type=="Resurvey" & Pres.Abs==1) %>% 
      mutate(el.min.res.fire = min(Elevation.m),
             el.max.res.fire = max(Elevation.m),
             el.med.res.fire = median(Elevation.m))
    el.mins.leg.fire[D,S] <- und.presence.SPEC.leg$el.min.leg.fire[1]
    el.maxs.leg.fire[D,S] <- und.presence.SPEC.leg$el.max.leg.fire[1]
    el.meds.leg.fire[D,S] <- und.presence.SPEC.leg$el.med.leg.fire[1]
    el.mins.res.fire[D,S] <- und.presence.SPEC.res$el.min.res.fire[1]
    el.maxs.res.fire[D,S] <- und.presence.SPEC.res$el.max.res.fire[1]
    el.meds.res.fire[D,S] <- und.presence.SPEC.res$el.med.res.fire[1]
  }
}

## collapse to averages across rarefied replicates

# no-fire species
el.mins.leg.means.nofire <- as.data.frame(el.mins.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.mins.res.means.nofire <- as.data.frame(el.mins.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.leg.means.nofire <- as.data.frame(el.maxs.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.res.means.nofire <- as.data.frame(el.maxs.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.meds.leg.means.nofire <- as.data.frame(el.meds.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.meds.res.means.nofire <- as.data.frame(el.meds.res.nofire) %>% summarise(across(starts_with("V"),mean))

# fire species
el.mins.leg.means.fire <- as.data.frame(el.mins.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.mins.res.means.fire <- as.data.frame(el.mins.res.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.leg.means.fire <- as.data.frame(el.maxs.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.res.means.fire <- as.data.frame(el.maxs.res.fire) %>% summarise(across(starts_with("V"),mean))
el.meds.leg.means.fire <- as.data.frame(el.meds.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.meds.res.means.fire <- as.data.frame(el.meds.res.fire) %>% summarise(across(starts_with("V"),mean))

## reshape and join into one frame

# no-fire species
el.mins.leg.tall.nofire <- gather(el.mins.leg.means.nofire, "species", "min.leg", 1:dim(species.list.nofire)[1])
el.mins.res.tall.nofire <- gather(el.mins.res.means.nofire, "species", "min.res", 1:dim(species.list.nofire)[1])
el.maxs.leg.tall.nofire <- gather(el.maxs.leg.means.nofire, "species", "max.leg", 1:dim(species.list.nofire)[1])
el.maxs.res.tall.nofire <- gather(el.maxs.res.means.nofire, "species", "max.res", 1:dim(species.list.nofire)[1])
el.meds.leg.tall.nofire <- gather(el.meds.leg.means.nofire, "species", "med.leg", 1:dim(species.list.nofire)[1])
el.meds.res.tall.nofire <- gather(el.meds.res.means.nofire, "species", "med.res", 1:dim(species.list.nofire)[1])

rarefied.change.nofire <- left_join(left_join(left_join(left_join(left_join(el.mins.leg.tall.nofire, el.mins.res.tall.nofire),el.maxs.leg.tall.nofire), el.maxs.res.tall.nofire), el.meds.leg.tall.nofire), el.meds.res.tall.nofire)

rarefied.change.nofire <- cbind(rarefied.change.nofire, species.list.nofire)
# fire species
el.mins.leg.tall.fire <- gather(el.mins.leg.means.fire, "species", "min.leg", 1:dim(species.list.fire)[1])
el.mins.res.tall.fire <- gather(el.mins.res.means.fire, "species", "min.res", 1:dim(species.list.fire)[1])
el.maxs.leg.tall.fire <- gather(el.maxs.leg.means.fire, "species", "max.leg", 1:dim(species.list.fire)[1])
el.maxs.res.tall.fire <- gather(el.maxs.res.means.fire, "species", "max.res", 1:dim(species.list.fire)[1])
el.meds.leg.tall.fire <- gather(el.meds.leg.means.fire, "species", "med.leg", 1:dim(species.list.fire)[1])
el.meds.res.tall.fire <- gather(el.meds.res.means.fire, "species", "med.res", 1:dim(species.list.fire)[1])

rarefied.change.fire <- left_join(left_join(left_join(left_join(left_join(el.mins.leg.tall.fire, el.mins.res.tall.fire),el.maxs.leg.tall.fire), el.maxs.res.tall.fire), el.meds.leg.tall.fire), el.meds.res.tall.fire)

rarefied.change.fire <- cbind(rarefied.change.fire, species.list.fire)

## calculate range changes
# no-fires species
rarefied.change.nofire <- rarefied.change.nofire %>% 
  mutate(rear.change = min.res - min.leg,
         med.change = med.res - med.leg,
         lead.change = max.res - max.leg)#,
         #span.change = (max.res-min.res) - (max.leg - min.leg))

# fire species
rarefied.change.fire <- rarefied.change.fire %>% 
  mutate(rear.change = min.res - min.leg,
         med.change = med.res - med.leg,
         lead.change = max.res - max.leg)#,
#span.change = (max.res-min.res) - (max.leg - min.leg))

## reshape for plotting

# no-fire species
rarefied.change.tall.nofire <- rarefied.change.nofire %>% 
  select(rear.change, med.change, lead.change) %>% #, span.change) %>% 
  #gather("edge", "change", 1:4) 
  gather("edge", "change", 1:3) 

# fire species
rarefied.change.tall.fire <- rarefied.change.fire %>% 
  select(rear.change, med.change, lead.change) %>% #, span.change) %>% 
  #gather("edge", "change", 1:4) 
  gather("edge", "change", 1:3) 

level_order = c("rear.change", "med.change", "lead.change")

violin.plot.nofire <- ggplot(rarefied.change.tall.nofire, aes(x=factor(edge, level=level_order), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.plot.fire <- ggplot(rarefied.change.tall.fire, aes(x=factor(edge, level=level_order), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Low\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.fig <- plot_grid(violin.plot.nofire, violin.plot.fire, labels=c("A", "B"))

ggsave("figures/violin_2panel.pdf", violin.fig, device="pdf", width=8, height=5)
