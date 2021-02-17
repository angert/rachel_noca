# Created: Feb. 16, 2020
# This script is Amy's first attempt at visualizing model outputs

library(tidyverse)

# coefficients from all top models for each rarefied dataset that ran without warnings
coeff.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_coefficients.csv", header = TRUE)

# filter to just model average for each rarefied dataset
coeff.avgs <- coeff.ALLDAT %>% filter(Type=="Avg") # now we have 100 model averages per species (but shouldn't this be <100 for the species for which some rarefied datasets threw warnings?)

# species list
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

# get rid of 12 species that Rachel flagged as having too many model warnings
problems = c("ACMI", "CAME", "COST", "HODI", "LUPE", "MEFE", "PHEM", "RHAL", "TRBO", "VAAL", "VADE", "VAME") #3 of these are probably salvageable (CAME, HODI, TRBO) but leaving out for now
species.short <- anti_join(as.data.frame(species.list), as.data.frame(problems), by=c("species.list"="problems"))

coeffs <- semi_join(coeff.avgs, species.short, by=c("Species"="species.list")) #should be length 100*n less than coeff.avgs; where n=# problematic species

coeffs.fire <- coeffs %>% filter(Fire.Included=="Yes")

# focusing on species with updated fire interaction models for now
species.fire <- semi_join(species.short, coeffs.fire, by=c("species.list"="Species"))

elev.vec = seq(0, 2200, by=1)
pred.leg.unburn.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec),ncol=100)

col.pal <- c("turquoise4", "red3", "goldenrod1")

for (i in 1:dim(species.fire)[1]) {
  sp = species.fire[i,]
  mods <- coeffs %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Year=Data.Type, 
           Elev=Elevation.m, 
           Fire=Fires, 
           YearxElev=Data.Type.Elevation.m, 
           ElevxFire=Elevation.m.Fires, 
           YearxFire=Data.Type.Fires, 
           Elev2=Elevation.m2, 
           YearxElev2=Data.Type.Elevation.m2, 
           Elev2xFire=Elevation.m2.Fires, 
           YearxElevxFire=Data.Type.Elevation.m.Fires, 
           YearxElev2xFire=Data.Type.Elevation.m2.Fires)
    for (j in 1:dim(mods)[1]) {
      pred.leg.unburn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec
      pred.res.unburn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
        mods$Year[j] + mods$YearxElev[j]*elev.vec + mods$YearxElev2[j]*elev.vec*elev.vec
      pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
        mods$Year[j] + mods$YearxElev[j]*elev.vec + mods$YearxElev2[j]*elev.vec*elev.vec + 
        mods$Fire[j] + mods$ElevxFire[j]*elev.vec + mods$YearxFire[j] + mods$Elev2xFire[j]*elev.vec*elev.vec + 
        mods$YearxElevxFire[j]*elev.vec + mods$YearxElev2xFire[j]*elev.vec*elev.vec
    }
  t1.unburn.reps <- as.data.frame(cbind(elev.vec, 'leg.unburn', pred.leg.unburn.reps))
  t1.unburn.reps.tall <- gather(t1.unburn.reps, "rep", "preds", 3:102)
  t1.unburn.summary <- t1.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp),
              lower.resp = unname(quantile(resp, c(0.05))),
              upper.resp = unname(quantile(resp, c(0.95))))
  t2.unburn.reps <- as.data.frame(cbind(elev.vec, 'res.unburn', pred.res.unburn.reps))
  t2.unburn.reps.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102)
  t2.unburn.summary <- t2.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp),
              lower.resp = unname(quantile(resp, c(0.05))),
              upper.resp = unname(quantile(resp, c(0.95))))
  t2.burn.reps <- as.data.frame(cbind(elev.vec, 'res.burn', pred.res.burn.reps))
  t2.burn.reps.tall <- gather(t2.burn.reps, "rep", "preds", 3:102)
  t2.burn.summary <- t2.burn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp),
              lower.resp = unname(quantile(resp, c(0.05))),
              upper.resp = unname(quantile(resp, c(0.95))))
  graph.dat <- bind_rows(t1.unburn.summary, t2.unburn.summary, t2.burn.summary)
  graph.dat$elev.vec <- as.numeric(graph.dat$elev.vec)
  
 gg <- ggplot(graph.dat, aes(x=elev.vec, y=mean.resp, color=V2)) +
    theme_classic() +
    ylab("Elevation (m)") +
    xlab("Probability of presence") +
    geom_errorbar(aes(ymin=lower.resp, ymax=upper.resp), alpha=0.1) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
 ggsave(paste("figures/model.preds_",sp,".pdf",sep=""), gg, width=5, height=5)

}
  