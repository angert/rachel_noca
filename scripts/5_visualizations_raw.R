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

# start with all species
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts %>% 
  filter(Species.Code!="MOSS") %>% 
  select(Species=Species.Code)
species.list$Species <- as.character(species.list$Species)

# separate out fire species
species.list.fire <- read.csv("data/3b_new_coefficients.csv", header = TRUE) %>% 
  filter(Type=="Avg")  %>% 
  filter(Fire.Included=="Yes") %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))

species.list.nofire <- anti_join(species.list, species.list.fire)

### Set up empty matrices to store values
el.mins.raw.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.mins.025.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.raw.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.975.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.meds.leg.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.mins.raw.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.mins.025.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.raw.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.maxs.975.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])
el.meds.res.nofire <- matrix(nrow=100,ncol=dim(species.list.nofire)[1])

el.mins.raw.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.mins.025.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.raw.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.975.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.meds.leg.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.mins.raw.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.mins.025.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.raw.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
el.maxs.975.res.fire <- matrix(nrow=100,ncol=dim(species.list.fire)[1])
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
      mutate(el.min.raw.leg.nofire = min(Elevation.m),
             el.max.raw.leg.nofire = max(Elevation.m),
             el.med.leg.nofire = median(Elevation.m),
             el.min.025.leg.nofire = quantile(Elevation.m, probs=0.025),
             el.max.975.leg.nofire = quantile(Elevation.m, probs=0.975))
    und.presence.SPEC.res <- und.SPEC %>% 
      filter(Data.Type=="Resurvey" & Pres.Abs==1) %>% 
      mutate(el.min.raw.res.nofire = min(Elevation.m),
             el.max.raw.res.nofire = max(Elevation.m),
             el.med.res.nofire = median(Elevation.m),             
             el.min.025.res.nofire = quantile(Elevation.m, probs=0.025),
             el.max.975.res.nofire = quantile(Elevation.m, probs=0.975))
    el.mins.raw.leg.nofire[D,S] <- und.presence.SPEC.leg$el.min.raw.leg.nofire[1]
    el.mins.025.leg.nofire[D,S] <- und.presence.SPEC.leg$el.min.025.leg.nofire[1]
    el.maxs.raw.leg.nofire[D,S] <- und.presence.SPEC.leg$el.max.raw.leg.nofire[1]
    el.maxs.975.leg.nofire[D,S] <- und.presence.SPEC.leg$el.max.975.leg.nofire[1]
    el.meds.leg.nofire[D,S] <- und.presence.SPEC.leg$el.med.leg.nofire[1]
    el.mins.raw.res.nofire[D,S] <- und.presence.SPEC.res$el.min.raw.res.nofire[1]
    el.mins.025.res.nofire[D,S] <- und.presence.SPEC.res$el.min.025.res.nofire[1]
    el.maxs.raw.res.nofire[D,S] <- und.presence.SPEC.res$el.max.raw.res.nofire[1]
    el.maxs.975.res.nofire[D,S] <- und.presence.SPEC.res$el.max.975.res.nofire[1]
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
      mutate(el.min.raw.leg.fire = min(Elevation.m),
             el.max.raw.leg.fire = max(Elevation.m),
             el.med.leg.fire = median(Elevation.m),
             el.min.025.leg.fire = quantile(Elevation.m, probs=0.025),
             el.max.975.leg.fire = quantile(Elevation.m, probs=0.975))
und.presence.SPEC.res <- und.SPEC %>% 
      filter(Data.Type=="Resurvey" & Pres.Abs==1) %>% 
      mutate(el.min.raw.res.fire = min(Elevation.m),
             el.max.raw.res.fire = max(Elevation.m),
             el.med.res.fire = median(Elevation.m),
             el.min.025.res.fire = quantile(Elevation.m, probs=0.025),
             el.max.975.res.fire = quantile(Elevation.m, probs=0.975))
    el.mins.raw.leg.fire[D,S] <- und.presence.SPEC.leg$el.min.raw.leg.fire[1]
    el.mins.025.leg.fire[D,S] <- und.presence.SPEC.leg$el.min.025.leg.fire[1]
    el.maxs.raw.leg.fire[D,S] <- und.presence.SPEC.leg$el.max.raw.leg.fire[1]
    el.maxs.975.leg.fire[D,S] <- und.presence.SPEC.leg$el.max.975.leg.fire[1]
    el.meds.leg.fire[D,S] <- und.presence.SPEC.leg$el.med.leg.fire[1]
    el.mins.raw.res.fire[D,S] <- und.presence.SPEC.res$el.min.raw.res.fire[1]
    el.mins.025.res.fire[D,S] <- und.presence.SPEC.res$el.min.025.res.fire[1]
    el.maxs.raw.res.fire[D,S] <- und.presence.SPEC.res$el.max.raw.res.fire[1]
    el.maxs.975.res.fire[D,S] <- und.presence.SPEC.res$el.max.975.res.fire[1]
    el.meds.res.fire[D,S] <- und.presence.SPEC.res$el.med.res.fire[1]
  }
}

## collapse to averages across rarefied replicates

# no-fire species
el.mins.raw.leg.means.nofire <- as.data.frame(el.mins.raw.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.mins.025.leg.means.nofire <- as.data.frame(el.mins.025.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.mins.raw.res.means.nofire <- as.data.frame(el.mins.raw.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.mins.025.res.means.nofire <- as.data.frame(el.mins.025.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.raw.leg.means.nofire <- as.data.frame(el.maxs.raw.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.975.leg.means.nofire <- as.data.frame(el.maxs.975.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.raw.res.means.nofire <- as.data.frame(el.maxs.raw.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.maxs.975.res.means.nofire <- as.data.frame(el.maxs.975.res.nofire) %>% summarise(across(starts_with("V"),mean))
el.meds.leg.means.nofire <- as.data.frame(el.meds.leg.nofire) %>% summarise(across(starts_with("V"),mean))
el.meds.res.means.nofire <- as.data.frame(el.meds.res.nofire) %>% summarise(across(starts_with("V"),mean))

# fire species
el.mins.raw.leg.means.fire <- as.data.frame(el.mins.raw.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.mins.025.leg.means.fire <- as.data.frame(el.mins.025.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.mins.raw.res.means.fire <- as.data.frame(el.mins.raw.res.fire) %>% summarise(across(starts_with("V"),mean))
el.mins.025.res.means.fire <- as.data.frame(el.mins.025.res.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.raw.leg.means.fire <- as.data.frame(el.maxs.raw.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.975.leg.means.fire <- as.data.frame(el.maxs.975.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.raw.res.means.fire <- as.data.frame(el.maxs.raw.res.fire) %>% summarise(across(starts_with("V"),mean))
el.maxs.975.res.means.fire <- as.data.frame(el.maxs.975.res.fire) %>% summarise(across(starts_with("V"),mean))
el.meds.leg.means.fire <- as.data.frame(el.meds.leg.fire) %>% summarise(across(starts_with("V"),mean))
el.meds.res.means.fire <- as.data.frame(el.meds.res.fire) %>% summarise(across(starts_with("V"),mean))

## reshape and join into one frame

# no-fire species
el.mins.raw.leg.tall.nofire <- gather(el.mins.raw.leg.means.nofire, "species", "min.raw.leg", 1:dim(species.list.nofire)[1])
el.mins.025.leg.tall.nofire <- gather(el.mins.025.leg.means.nofire, "species", "min.025.leg", 1:dim(species.list.nofire)[1])
el.mins.raw.res.tall.nofire <- gather(el.mins.raw.res.means.nofire, "species", "min.raw.res", 1:dim(species.list.nofire)[1])
el.mins.025.res.tall.nofire <- gather(el.mins.025.res.means.nofire, "species", "min.025.res", 1:dim(species.list.nofire)[1])
el.maxs.raw.leg.tall.nofire <- gather(el.maxs.raw.leg.means.nofire, "species", "max.raw.leg", 1:dim(species.list.nofire)[1])
el.maxs.975.leg.tall.nofire <- gather(el.maxs.975.leg.means.nofire, "species", "max.975.leg", 1:dim(species.list.nofire)[1])
el.maxs.raw.res.tall.nofire <- gather(el.maxs.raw.res.means.nofire, "species", "max.raw.res", 1:dim(species.list.nofire)[1])
el.maxs.975.res.tall.nofire <- gather(el.maxs.975.res.means.nofire, "species", "max.975.res", 1:dim(species.list.nofire)[1])
el.meds.leg.tall.nofire <- gather(el.meds.leg.means.nofire, "species", "med.leg", 1:dim(species.list.nofire)[1])
el.meds.res.tall.nofire <- gather(el.meds.res.means.nofire, "species", "med.res", 1:dim(species.list.nofire)[1])

rarefied.change.nofire <- left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(el.mins.raw.leg.tall.nofire, el.mins.raw.res.tall.nofire),el.maxs.raw.leg.tall.nofire), el.maxs.raw.res.tall.nofire), el.meds.leg.tall.nofire), el.meds.res.tall.nofire), el.mins.025.leg.tall.nofire), el.mins.025.res.tall.nofire), el.maxs.975.leg.tall.nofire), el.maxs.975.res.tall.nofire)

rarefied.change.nofire <- cbind(rarefied.change.nofire, species.list.nofire)
rarefied.change.nofire$fire <- "no"

# fire species
el.mins.raw.leg.tall.fire <- gather(el.mins.raw.leg.means.fire, "species", "min.raw.leg", 1:dim(species.list.fire)[1])
el.mins.025.leg.tall.fire <- gather(el.mins.025.leg.means.fire, "species", "min.025.leg", 1:dim(species.list.fire)[1])
el.mins.raw.res.tall.fire <- gather(el.mins.raw.res.means.fire, "species", "min.raw.res", 1:dim(species.list.fire)[1])
el.mins.025.res.tall.fire <- gather(el.mins.025.res.means.fire, "species", "min.025.res", 1:dim(species.list.fire)[1])
el.maxs.raw.leg.tall.fire <- gather(el.maxs.raw.leg.means.fire, "species", "max.raw.leg", 1:dim(species.list.fire)[1])
el.maxs.975.leg.tall.fire <- gather(el.maxs.975.leg.means.fire, "species", "max.975.leg", 1:dim(species.list.fire)[1])
el.maxs.raw.res.tall.fire <- gather(el.maxs.raw.res.means.fire, "species", "max.raw.res", 1:dim(species.list.fire)[1])
el.maxs.975.res.tall.fire <- gather(el.maxs.975.res.means.fire, "species", "max.975.res", 1:dim(species.list.fire)[1])
el.meds.leg.tall.fire <- gather(el.meds.leg.means.fire, "species", "med.leg", 1:dim(species.list.fire)[1])
el.meds.res.tall.fire <- gather(el.meds.res.means.fire, "species", "med.res", 1:dim(species.list.fire)[1])

rarefied.change.fire <- left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(el.mins.raw.leg.tall.fire, el.mins.raw.res.tall.fire),el.maxs.raw.leg.tall.fire), el.maxs.raw.res.tall.fire), el.meds.leg.tall.fire), el.meds.res.tall.fire), el.mins.025.leg.tall.fire), el.mins.025.res.tall.fire), el.maxs.975.leg.tall.fire), el.maxs.975.res.tall.fire)

rarefied.change.fire <- cbind(rarefied.change.fire, species.list.fire)
rarefied.change.fire$fire <- "yes"

## calculate range changes
# no-fires species
rarefied.change.nofire <- rarefied.change.nofire %>% 
  mutate(rear.change.raw = min.raw.res - min.raw.leg,
         rear.change.perc = min.025.res - min.025.leg,
         med.change = med.res - med.leg,
         lead.change.raw = max.raw.res - max.raw.leg,
         lead.change.perc = max.975.res - max.975.leg)#,
         #span.change = (max.res-min.res) - (max.leg - min.leg))

# fire species
rarefied.change.fire <- rarefied.change.fire %>% 
  mutate(rear.change.raw = min.raw.res - min.raw.leg,
         rear.change.perc = min.025.res - min.025.leg,
         med.change = med.res - med.leg,
         lead.change.raw = max.raw.res - max.raw.leg,
         lead.change.perc = max.975.res - max.975.leg)#,
#span.change = (max.res-min.res) - (max.leg - min.leg))

rarefied.change.calcs <- rbind(rarefied.change.nofire, rarefied.change.fire)
write.csv(rarefied.change.calcs, "data/5_range.change.calcs.csv")

## reshape for violin plotting

# no-fire species
rarefied.change.tall.nofire.raw <- rarefied.change.nofire %>% 
  select(rear.change.raw, med.change, lead.change.raw) %>% 
  gather("edge", "change", 1:3) %>% 
  add_column(fire="no")

rarefied.change.tall.nofire.perc <- rarefied.change.nofire %>% 
  select(rear.change.perc, med.change, lead.change.perc) %>% 
  gather("edge", "change", 1:3) %>% 
  add_column(fire="no")

# fire species
rarefied.change.tall.fire.raw <- rarefied.change.fire %>% 
  select(rear.change.raw, med.change, lead.change.raw) %>% 
  gather("edge", "change", 1:3) %>% 
  add_column(fire="yes")

rarefied.change.tall.fire.perc <- rarefied.change.fire %>% 
  select(rear.change.perc, med.change, lead.change.perc) %>% 
  gather("edge", "change", 1:3) %>% 
  add_column(fire="yes")

rarefied.change.tall.perc <- bind_rows(rarefied.change.tall.nofire.perc, rarefied.change.tall.fire.perc)
rarefied.change.tall.raw <- bind_rows(rarefied.change.tall.nofire.raw, rarefied.change.tall.fire.raw)


level_order.raw = c("rear.change.raw", "med.change", "lead.change.raw")
level_order.perc = c("rear.change.perc", "med.change", "lead.change.perc")

# new versions by range position
col.pal <- c("skyblue", "orange")

violin.plot.raw <- ggplot(rarefied.change.tall.raw, aes(x=factor(edge, level=level_order.raw), y=change, fill=fire)) + 
  geom_violin() +
  scale_fill_manual(values=col.pal) +
  #stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  geom_segment(aes(x = 0.8, xend = 1.2, y = 400, yend = 400)) +
  geom_segment(aes(x = 1.8, xend = 2.2, y = 400, yend = 400)) +
  geom_segment(aes(x = 2.75, xend = 3.15, y = 400, yend = 400)) +
  annotate("text", x=1, y=450, label="ns") +
  annotate("text", x=2, y=450, label="+") +
  annotate("text", x=2.95, y=440, label="*") 
violin.plot.raw

ggsave("figures/violin_1panel_raw.pdf", violin.plot.raw, device="pdf", width=8, height=5)

violin.plot.perc <- ggplot(rarefied.change.tall.perc, aes(x=factor(edge, level=level_order.perc), y=change, fill=fire)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  scale_fill_manual(values=col.pal) +
  #stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  geom_segment(aes(x = 0.8, xend = 1.2, y = 400, yend = 400)) +
  geom_segment(aes(x = 1.8, xend = 2.2, y = 400, yend = 400)) +
  geom_segment(aes(x = 2.75, xend = 3.15, y = 400, yend = 400)) +
  annotate("text", x=1, y=450, label="ns") +
  annotate("text", x=2, y=450, label="+") +
  annotate("text", x=2.95, y=440, label="*") 
violin.plot.perc

ggsave("figures/violin_1panel_perc.pdf", violin.plot.perc, device="pdf", width=8, height=5)
  
# old versions by fire status
violin.plot.nofire.raw <- ggplot(rarefied.change.tall.nofire.raw, aes(x=factor(edge, level=level_order.raw), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.plot.nofire.perc <- ggplot(rarefied.change.tall.nofire.perc, aes(x=factor(edge, level=level_order.perc), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Lower\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.plot.fire.raw <- ggplot(rarefied.change.tall.fire.raw, aes(x=factor(edge, level=level_order.raw), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Low\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.plot.fire.perc <- ggplot(rarefied.change.tall.fire.perc, aes(x=factor(edge, level=level_order.perc), y=change)) +#, color=edge, fill=edge)) + 
  geom_violin() +
  stat_summary(fun=mean, geom="point", cex=2)  +
  theme_classic() +
  geom_hline(yintercept=0, lty="dashed") +
  xlab("") +
  scale_x_discrete(labels=c("Low\nedge", "Range\ncenter", "Upper\nedge")) +
  ylim(-600,700) +
  ylab(c("Elevational change (m)\n1983-2015")) +
  theme(legend.position="none")

violin.fig.raw <- plot_grid(violin.plot.nofire.raw, violin.plot.fire.raw, labels=c("A", "B"))
violin.fig.perc <- plot_grid(violin.plot.nofire.perc, violin.plot.fire.perc, labels=c("A", "B"))

ggsave("figures/violin_2panel_raw.pdf", violin.fig.raw, device="pdf", width=8, height=5)
ggsave("figures/violin_2panel_perc.pdf", violin.fig.perc, device="pdf", width=8, height=5)


## Freeman-style elevation ranges

rarefied.change.calcs <- read_csv("data/5_range.change.calcs.csv")

# option a: all species interdigitated regardless of fire; fire species labeled
rarefied.change.calcs <- rarefied.change.calcs %>% 
  mutate(species.rank.med = dense_rank(med.leg),
         #species.rank.min = dense_rank(min.raw.leg), #doesn't work because of ties
         #species.rank.max = dense_rank(max.raw.leg), #doesn't work because of ties
         both.min.raw = pmax(min.raw.leg, min.raw.res),
         both.max.raw = pmin(max.raw.leg, max.raw.res),
         both.min.perc = pmax(min.025.leg, min.025.res),
         both.max.perc = pmin(max.975.leg, max.975.res))

fire.pos <- rarefied.change.calcs %>% 
  filter(fire=="yes") %>% 
  mutate(y.pos = pmax(max.975.leg, max.975.res) + 50) %>% 
  select(species.rank.med, y.pos) %>% 
  droplevels()

p <- ggplot(rarefied.change.calcs) + 
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.leg, ymax=max.975.leg), fill = "#F8766D") + # historic range in red; will show areas of range contractions
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.res, ymax=max.975.res), fill = "#00BFC4") + # modern range in blue; will show areas of range expansion
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=both.min.perc, ymax=both.max.perc), fill = "#bdbdbd") + # areas common to both in grey
  scale_x_continuous("Species", breaks=c(1,10,20,30,40)) +
  scale_y_continuous("Elevation (m)", breaks=c(0,500,1000,1500,2000)) +
  theme_bw() +
  theme(text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor =   element_blank()) +
  annotate(geom="text", x=fire.pos$species.rank.med, y=fire.pos$y.pos, label="F")

ggsave("figures/elevation_ranges_1panel.pdf", p, device="pdf", width=5, height=5)


# option b: fire and no-fire species separated
rarefied.change.calcs.fire <- rarefied.change.calcs %>% 
  filter(fire=="yes") %>% 
  mutate(species.rank.med = dense_rank(med.leg),
         #species.rank.min = dense_rank(min.raw.leg), #doesn't work because of ties
         #species.rank.max = dense_rank(max.raw.leg), #doesn't work because of ties
         both.min.raw = pmax(min.raw.leg, min.raw.res),
         both.max.raw = pmin(max.raw.leg, max.raw.res),
         both.min.perc = pmax(min.025.leg, min.025.res),
         both.max.perc = pmin(max.975.leg, max.975.res))

rarefied.change.calcs.nofire <- rarefied.change.calcs %>% 
  filter(fire=="no") %>% 
  mutate(species.rank.med = dense_rank(med.leg),
         #species.rank.min = dense_rank(min.raw.leg), #doesn't work because of ties
         #species.rank.max = dense_rank(max.raw.leg), #doesn't work because of ties
         both.min.raw = pmax(min.raw.leg, min.raw.res),
         both.max.raw = pmin(max.raw.leg, max.raw.res),
         both.min.perc = pmax(min.025.leg, min.025.res),
         both.max.perc = pmin(max.975.leg, max.975.res))

p.nofire <- ggplot(rarefied.change.calcs.nofire) + 
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.leg, ymax=max.975.leg), fill = "#F8766D") + # historic range in red; will show areas of range contractions
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.res, ymax=max.975.res), fill = "#00BFC4") + # modern range in blue; will show areas of range expansion
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=both.min.perc, ymax=both.max.perc), fill = "#bdbdbd") + # areas common to both in grey
  scale_x_continuous(breaks=c(1,35)) +
  ylim(0,2200) +
  xlab("") +
  ylab("Elevation (m)") +
  theme_bw() +
  theme(text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor =   element_blank())

p.fire <- ggplot(rarefied.change.calcs.fire) + 
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.leg, ymax=max.975.leg), fill = "#F8766D") + # historic range in red; will show areas of range contractions
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=min.025.res, ymax=max.975.res), fill = "#00BFC4") + # modern range in blue; will show areas of range expansion
  geom_rect(aes(xmin=species.rank.med-0.33, xmax=species.rank.med+0.33, ymin=both.min.perc, ymax=both.max.perc), fill = "#bdbdbd") + # areas common to both in grey
  ylim(0,2200) +
  scale_x_continuous(breaks=c(1,7)) +
  xlab("") +
  #ylab("Elevation (m)") +
  theme_bw() +
  theme(text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor =   element_blank())

range.fig <- plot_grid(p.nofire, p.fire, rel_widths=c(3,1), labels=c("A", "B"))
range.fig <- ggdraw(add_sub(range.fig, "Species", vpadding=grid::unit(0,"lines"), y=6, x=0.75, vjust=4.5, size=16))

ggsave("figures/elevation_ranges_2panel.pdf", range.fig, device="pdf", width=5, height=5)


## Statistical tests for differences between fire and no-fire species groups
rarefied.change.calcs <- read_csv("data/5_range.change.calcs.csv")

rear.t <- t.test(rarefied.change.calcs$rear.change.perc[rarefied.change.calcs$fire=="no"], rarefied.change.calcs$rear.change.perc[rarefied.change.calcs$fire=="yes"])
rear.t

rear.t.raw <- t.test(rarefied.change.calcs$rear.change.raw[rarefied.change.calcs$fire=="no"], rarefied.change.calcs$rear.change.raw[rarefied.change.calcs$fire=="yes"])
rear.t.raw

med.t <- t.test(rarefied.change.calcs$med.change[rarefied.change.calcs$fire=="no"], rarefied.change.calcs$med.change[rarefied.change.calcs$fire=="yes"])
med.t

lead.t <- t.test(rarefied.change.calcs$lead.change.perc[rarefied.change.calcs$fire=="no"], rarefied.change.calcs$lead.change.perc[rarefied.change.calcs$fire=="yes"])
lead.t

lead.t.raw <- t.test(rarefied.change.calcs$lead.change.raw[rarefied.change.calcs$fire=="no"], rarefied.change.calcs$lead.change.raw[rarefied.change.calcs$fire=="yes"])
lead.t.raw

