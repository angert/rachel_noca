# Created: Mar 1, 2020
# This script is Amy's redo using model average CIs

library(tidyverse)

# coefficients from all top models for each rarefied dataset that ran without warnings
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)

# 95% CIs from all top models for each rarefied dataset that ran without warnings
cis.ALLDAT <- read.csv("data/3b_new_confint.csv", header = TRUE)

# filter to just model average for each rarefied dataset
coeff.avgs <- coeff.ALLDAT %>% filter(Type=="Avg") # now we have 100 model averages per species (but shouldn't this be <100 for the species for which some rarefied datasets threw warnings?)
cis.avgs <- cis.ALLDAT %>% filter(Type=="Avg.Confidence") 

# species list
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

# Discard species for whom 2 or more models threw warnings: COST, LUPE, PHEM, RHAL, VAAL, VADE
problems = c("COST", "LUPE", "PHEM", "RHAL", "VAAL", "VADE") 
species.short <- anti_join(as.data.frame(species.list), as.data.frame(problems), by=c("species.list"="problems"))

coeffs <- semi_join(coeff.avgs, species.short, by=c("Species"="species.list")) #should be length 100*n less than coeff.avgs; where n=# problematic species
cis <- semi_join(cis.avgs, species.short, by=c("Species"="species.list")) 

coeffs.fire <- coeffs %>% filter(Fire.Included=="Yes")
coeffs.nofire <- coeffs %>% filter(Fire.Included=="No")
cis.fire <- cis %>% filter(Fire.Included=="Yes")
cis.nofire <- cis %>% filter(Fire.Included=="No")

# focusing on species with updated fire interaction models first
species.fire <- semi_join(species.short, coeffs.fire, by=c("species.list"="Species"))

elev.vec = seq(0, 2200, by=1)
pred.leg.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.leg.reps.low = matrix(nrow=length(elev.vec),ncol=100)
pred.res.unburn.reps.low = matrix(nrow=length(elev.vec),ncol=100)
pred.res.burn.reps.low = matrix(nrow=length(elev.vec),ncol=100)
pred.leg.reps.high = matrix(nrow=length(elev.vec),ncol=100)
pred.res.unburn.reps.high = matrix(nrow=length(elev.vec),ncol=100)
pred.res.burn.reps.high = matrix(nrow=length(elev.vec),ncol=100)

col.pal <- c("turquoise4", "red3", "goldenrod1")

for (i in 1:dim(species.fire)[1]) {
  sp = species.fire[i,]  
  mods <- coeffs.fire %>% 
    filter(Species==sp) %>% # pull 100 avg coefficients for a species
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           ResurvBurn = Resurvey.Burned.fi,
           ResurvUnburn = Resurvey.Unburned.fi,
           ResurvBurnxElev = Elevation.m.Res.Burn.fi,
           ResurvBurnxElev2 = Elevation.m2.Res.Burn.fi,
           ResurvUnburnxElev = Elevation.m2.Res.Unburn.fi,
           ResurvUnburnXElev2 = Elevation.m2.Res.Unburn.fi)
  mods[is.na(mods)] <- 0
  confs <- cis.fire %>% 
    filter(Species==sp) %>% # pull 100 avg upper and lower bounds for a species
    select(Int.low=Intercept.CI.Lower, 
           Elev.low=Elevation.m.CI.Lower, 
           Elev2.low=Elevation.m2.CI.Lower, 
           ResurvBurn.low = Resurvey.Burned.fi.CI.Lower,
           ResurvUnburn.low = Resurvey.Unburned.fi.CI.Lower,
           ResurvBurnxElev.low = Elevation.m.Res.Burn.fi.CI.Lower,
           ResurvBurnxElev2.low = Elevation.m2.Res.Burn.fi.CI.Lower,
           ResurvUnburnxElev.low = Elevation.m2.Res.Unburn.fi.CI.Lower,
           ResurvUnburnXElev2.low = Elevation.m2.Res.Unburn.fi.CI.Lower,
           Int.high=Intercept.CI.Upper, 
           Elev.high=Elevation.m.CI.Upper, 
           Elev2.high=Elevation.m2.CI.Upper, 
           ResurvBurn.high = Resurvey.Burned.fi.CI.Upper,
           ResurvUnburn.high = Resurvey.Unburned.fi.CI.Upper,
           ResurvBurnxElev.high = Elevation.m.Res.Burn.fi.CI.Upper,
           ResurvBurnxElev2.high = Elevation.m2.Res.Burn.fi.CI.Upper,
           ResurvUnburnxElev.high = Elevation.m2.Res.Unburn.fi.CI.Upper,
           ResurvUnburnXElev2.high = Elevation.m2.Res.Unburn.fi.CI.Upper)
  confs[is.na(confs)] <- 0
    for (j in 1:dim(mods)[1]) { #calculate response curve per each avg model across elevation vector
    # mega-average (i.e., average of 100 rarefied averages) model prediction & CI for legacy time point
    # dim is n rarefied reps wide x elev vec long
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec
    pred.leg.reps.low[,j] = confs$Int.low[j] + confs$Elev.low[j]*elev.vec + confs$Elev2.low[j]*elev.vec*elev.vec
    pred.leg.reps.high[,j] = confs$Int.high[j] + confs$Elev.high[j]*elev.vec + confs$Elev2.high[j]*elev.vec*elev.vec
    # mega-average model prediction for resurvey time point, unburned
    pred.res.unburn.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$ResurvUnburn[j] + mods$ResurvUnburnxElev[j]*elev.vec + 
      mods$ResurvUnburnXElev2[j]*elev.vec*elev.vec
    pred.res.unburn.reps.low[,j] = confs$Int.low[j]  + confs$Elev.low[j]*elev.vec + confs$Elev2.low[j]*elev.vec*elev.vec +
      confs$ResurvUnburn.low[j] + confs$ResurvUnburnxElev.low[j]*elev.vec + 
      confs$ResurvUnburnXElev2.low[j]*elev.vec*elev.vec
    pred.res.unburn.reps.high[,j] = confs$Int.high[j]  + confs$Elev.high[j]*elev.vec + confs$Elev2.high[j]*elev.vec*elev.vec +
      confs$ResurvUnburn.high[j] + confs$ResurvUnburnxElev.high[j]*elev.vec + 
      confs$ResurvUnburnXElev2.high[j]*elev.vec*elev.vec
    # model prediction for resurvey time point, burned
    pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$ResurvBurn[j] + mods$ResurvBurnxElev[j]*elev.vec + 
      mods$ResurvBurnxElev2[j]*elev.vec*elev.vec
    pred.res.burn.reps.low[,j] = confs$Int.low[j] + confs$Elev.low[j]*elev.vec + confs$Elev2.low[j]*elev.vec*elev.vec +
      confs$ResurvBurn.low[j] + confs$ResurvBurnxElev.low[j]*elev.vec + 
      confs$ResurvBurnxElev2.low[j]*elev.vec*elev.vec
    pred.res.burn.reps.high[,j] = confs$Int.high[j] + confs$Elev.high[j]*elev.vec + confs$Elev2.high[j]*elev.vec*elev.vec +
      confs$ResurvBurn.high[j] + confs$ResurvBurnxElev.high[j]*elev.vec + 
      confs$ResurvBurnxElev2.high[j]*elev.vec*elev.vec
    }
  # reshape dfs and express on response scale (0-1)
  t1.unburn.reps <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps))
  t1.unburn.reps.low <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps.low))
  t1.unburn.reps.high <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps.high))
  
  t1.unburn.reps.tall <- gather(t1.unburn.reps, "rep", "preds", 3:102)
  t1.unburn.reps.tall.low <- gather(t1.unburn.reps.low, "rep", "preds", 3:102)
  t1.unburn.reps.tall.high <- gather(t1.unburn.reps.high, "rep", "preds", 3:102)
  
  t1.unburn.summary <- t1.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp))
  t1.unburn.summary.low <- t1.unburn.reps.tall.low %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.lower = mean(lower))
  t1.unburn.summary.high <- t1.unburn.reps.tall.high %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.upper = mean(upper))
  
  t2.unburn.reps <- as.data.frame(cbind(elev.vec, 'res.unburn', pred.res.unburn.reps))
  t2.unburn.reps.low <- as.data.frame(cbind(elev.vec, 'res.unburn', pred.res.unburn.reps.low))
  t2.unburn.reps.high <- as.data.frame(cbind(elev.vec, 'res.unburn', pred.res.unburn.reps.high))
  
  t2.unburn.reps.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102)
  t2.unburn.reps.tall.low <- gather(t2.unburn.reps.low, "rep", "preds", 3:102)
  t2.unburn.reps.tall.high <- gather(t2.unburn.reps.high, "rep", "preds", 3:102)
  
  t2.unburn.summary <- t2.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp))
  t2.unburn.summary.low <- t2.unburn.reps.tall.low %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.lower = mean(lower))
  t2.unburn.summary.high <- t2.unburn.reps.tall.high %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.upper = mean(upper))
  
  t2.burn.reps <- as.data.frame(cbind(elev.vec, 'res.burn', pred.res.burn.reps))
  t2.burn.reps.low <- as.data.frame(cbind(elev.vec, 'res.burn', pred.res.burn.reps.low))
  t2.burn.reps.high <- as.data.frame(cbind(elev.vec, 'res.burn', pred.res.burn.reps.high))
  
  t2.burn.reps.tall <- gather(t2.burn.reps, "rep", "preds", 3:102)
  t2.burn.reps.tall.low <- gather(t2.burn.reps.low, "rep", "preds", 3:102)
  t2.burn.reps.tall.high <- gather(t2.burn.reps.high, "rep", "preds", 3:102)
  
  t2.burn.summary <- t2.burn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp))
  t2.burn.summary.low <- t2.burn.reps.tall.low %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.lower = mean(lower))
  t2.burn.summary.high <- t2.burn.reps.tall.high %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.upper = mean(upper))
  
  mean.dat <- bind_rows(t1.unburn.summary, t2.unburn.summary, t2.burn.summary)
  low.dat <- bind_rows(t1.unburn.summary.low, t2.unburn.summary.low, t2.burn.summary.low)
  high.dat <- bind_rows(t1.unburn.summary.high, t2.unburn.summary.high, t2.burn.summary.high)
  
  graph.dat <- left_join(left_join(mean.dat, low.dat), high.dat)
  graph.dat$elev.vec <- as.numeric(graph.dat$elev.vec)
  
 gg <- ggplot(graph.dat, aes(x=elev.vec, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=mean.lower, ymax=mean.upper), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
 ggsave(paste("figures/model.preds.CIs_",sp,".pdf",sep=""), gg, width=5, height=5)

}
  
# now the species without fire
species.nofire <- semi_join(species.short, coeffs.nofire, by=c("species.list"="Species"))

elev.vec = seq(0, 2200, by=1)
pred.leg.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.leg.reps.low = matrix(nrow=length(elev.vec),ncol=100)
pred.leg.reps.high = matrix(nrow=length(elev.vec),ncol=100)
pred.res.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.reps.low = matrix(nrow=length(elev.vec),ncol=100)
pred.res.reps.high = matrix(nrow=length(elev.vec),ncol=100)

col.pal <- c("turquoise4", "goldenrod1")

for (i in 1:dim(species.nofire)[1]) {
  sp = species.nofire[i,]
  mods <- coeffs.nofire %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           Year = Data.Type.nofi,
           YearxElev = Data.Type.Elevation.m.nofi,
           YearxElev2 = Data.Type.Elevation.m2.nofi)
  mods[is.na(mods)] <- 0
  confs <- cis.nofire %>% 
    filter(Species==sp) %>% 
    select(Int.low=Intercept.CI.Lower, 
           Elev.low=Elevation.m.CI.Lower, 
           Elev2.low=Elevation.m2.CI.Lower, 
           Year.low = Data.Type.nofi.CI.Lower,
           YearxElev.low = Data.Type.Elevation.m.nofi.CI.Lower,
           YearxElev2.low = Data.Type.Elevation.m2.nofi.CI.Lower,
           Int.high=Intercept.CI.Upper, 
           Elev.high=Elevation.m.CI.Upper, 
           Elev2.high=Elevation.m2.CI.Upper, 
           Year.high = Data.Type.nofi.CI.Upper,
           YearxElev.high = Data.Type.Elevation.m.nofi.CI.Upper,
           YearxElev2.high = Data.Type.Elevation.m2.nofi.CI.Upper)
  confs[is.na(confs)] <- 0
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec
    pred.leg.reps.low[,j] = confs$Int.low[j] + confs$Elev.low[j]*elev.vec + confs$Elev2.low[j]*elev.vec*elev.vec
    pred.leg.reps.high[,j] = confs$Int.high[j] + confs$Elev.high[j]*elev.vec + confs$Elev2.high[j]*elev.vec*elev.vec

    pred.res.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$Year[j] + mods$YearxElev[j]*elev.vec + mods$YearxElev2[j]*elev.vec^2
    pred.res.reps.low[,j] = confs$Int.low[j]  + confs$Elev.low[j]*elev.vec + confs$Elev2.low[j]*elev.vec*elev.vec +
      confs$Year.low[j] + confs$YearxElev.low[j]*elev.vec + confs$YearxElev2.low[j]*elev.vec^2
    pred.res.reps.high[,j] = confs$Int.high[j]  + confs$Elev.high[j]*elev.vec + confs$Elev2.high[j]*elev.vec*elev.vec +
      confs$Year.high[j] + confs$YearxElev.high[j]*elev.vec + confs$YearxElev2.high[j]*elev.vec^2
  }
  
  t1.reps <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps))
  t1.reps.low <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps.low))
  t1.reps.high <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps.high))

  t1.reps.tall <- gather(t1.reps, "rep", "preds", 3:102)
  t1.reps.tall.low <- gather(t1.reps.low, "rep", "preds", 3:102)
  t1.reps.tall.high <- gather(t1.reps.high, "rep", "preds", 3:102)

  t1.summary <- t1.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp))
  t1.summary.low <- t1.reps.tall.low %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.lower = mean(lower))
  t1.summary.high <- t1.reps.tall.high %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.upper = mean(upper))
  
  t2.reps <- as.data.frame(cbind(elev.vec, 'resurvey', pred.res.reps))
  t2.reps.low <- as.data.frame(cbind(elev.vec, 'resurvey', pred.res.reps.low))
  t2.reps.high <- as.data.frame(cbind(elev.vec, 'resurvey', pred.res.reps.high))

  t2.reps.tall <- gather(t2.reps, "rep", "preds", 3:102)
  t2.reps.tall.low <- gather(t2.reps.low, "rep", "preds", 3:102)
  t2.reps.tall.high <- gather(t2.reps.high, "rep", "preds", 3:102)
  
  t2.summary <- t2.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp))
  t2.summary.low <- t2.reps.tall.low %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.lower = mean(lower))
  t2.summary.high <- t2.reps.tall.high %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.upper = mean(upper))
  
  mean.dat <- bind_rows(t1.summary, t2.summary)
  low.dat <- bind_rows(t1.summary.low, t2.summary.low)
  high.dat <- bind_rows(t1.summary.high, t2.summary.high)
  
  graph.dat <- left_join(left_join(mean.dat, low.dat), high.dat)
  graph.dat$elev.vec <- as.numeric(graph.dat$elev.vec)
  
  gg <- ggplot(graph.dat, aes(x=elev.vec, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=mean.lower, ymax=mean.upper), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
  ggsave(paste("figures/model.preds.CI_",sp,".pdf",sep=""), gg, width=5, height=5)
  
}
