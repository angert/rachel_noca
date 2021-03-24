# Created: Mar 24, 2021

library(tidyverse)

# coefficients from the global model for each rarefied dataset that ran without warnings
coeff.glob.fire <- read.csv("data/3c_global_mod_coefficients_with_P_FIRE.csv", header = TRUE)
coeff.glob.nofire <- read.csv("data/3c_global_mod_coefficients_with_P_NOFIRE.csv", header = TRUE)

# species lists for loops below
species.fire <- coeff.glob.fire %>% 
  group_by(Species) %>% 
  summarise(Species = first(Species))
species.nofire <- coeff.glob.nofire %>% 
  group_by(Species) %>% 
  summarise(Species = first(Species))

# graphs for fire species
elev.vec = as.numeric(seq(0, 2200, by=1))
pred.leg.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec),ncol=100)

col.pal <- c("turquoise4", "red3", "goldenrod1")

for (i in 1:dim(species.fire)[1]) {
  sp = as.list(species.fire[i,1], drop=TRUE)
  mods <- coeff.glob.fire %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           ResurvBurn = Resurvey.Burned.fi,
           ResurvUnburn = Resurvey.Unburned.fi,
           ResurvBurnxElev = Elevation.m.Res.Burn.fi,
           ResurvBurnxElev2 = Elevation.m2.Res.Burn.fi,
           ResurvUnburnxElev = Elevation.m2.Res.Unburn.fi,
           ResurvUnburnXElev2 = Elevation.m2.Res.Unburn.fi
    )
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec
    pred.res.unburn.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$ResurvUnburn[j] + mods$ResurvUnburnxElev[j]*elev.vec + 
      mods$ResurvUnburnXElev2[j]*elev.vec*elev.vec
    pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$ResurvBurn[j] + mods$ResurvBurnxElev[j]*elev.vec + 
      mods$ResurvBurnxElev2[j]*elev.vec*elev.vec
    }
  t1.unburn.reps <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps))
  t1.unburn.reps.tall <- gather(t1.unburn.reps, "rep", "preds", 3:102)
  t1.unburn.summary <- t1.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE),
              lower.resp = unname(quantile(resp, c(0.05), na.rm=TRUE)),
              upper.resp = unname(quantile(resp, c(0.95), na.rm=TRUE)))
  t2.unburn.reps <- as.data.frame(cbind(elev.vec, 'res.unburn', pred.res.unburn.reps))
  t2.unburn.reps.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102)
  t2.unburn.summary <- t2.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE),
              lower.resp = unname(quantile(resp, c(0.05), na.rm=TRUE)),
              upper.resp = unname(quantile(resp, c(0.95), na.rm=TRUE)))
  t2.burn.reps <- as.data.frame(cbind(elev.vec, 'res.burn', pred.res.burn.reps))
  t2.burn.reps.tall <- gather(t2.burn.reps, "rep", "preds", 3:102)
  t2.burn.summary <- t2.burn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE),
              lower.resp = unname(quantile(resp, c(0.05), na.rm=TRUE)),
              upper.resp = unname(quantile(resp, c(0.95), na.rm=TRUE)))
  graph.dat <- bind_rows(t1.unburn.summary, t2.unburn.summary, t2.burn.summary)
  graph.dat$elev.vec <- as.numeric(graph.dat$elev.vec)
  
 gg <- ggplot(graph.dat, aes(x=elev.vec, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    #geom_errorbar(aes(ymin=lower.resp, ymax=upper.resp), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
 ggsave(paste("figures/global.model.preds_",sp,".pdf",sep=""), gg, width=5, height=5)

} 
  

elev.vec = seq(0, 2200, by=1)
pred.leg.reps = matrix(nrow=length(elev.vec),ncol=100)
pred.res.reps = matrix(nrow=length(elev.vec),ncol=100)

col.pal <- c("turquoise4", "goldenrod1")

for (i in 1:dim(species.nofire)[1]) {
  sp = species.nofire[i,]
  mods <- coeffs %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           Year = Data.Type.nofi,
           YearxElev = Data.Type.Elevation.m.nofi,
           YearxElev2 = Data.Type.Elevation.m2.nofi,
    )
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec
    pred.res.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec + mods$Elev2[j]*elev.vec*elev.vec +
      mods$Year[j] + mods$YearxElev[j]*elev.vec + mods$YearxElev2[j]*elev.vec^2
  }
  t1.reps <- as.data.frame(cbind(elev.vec, 'legacy', pred.leg.reps))
  t1.reps.tall <- gather(t1.reps, "rep", "preds", 3:102)
  t1.summary <- t1.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp),
              lower.resp = unname(quantile(resp, c(0.05))),
              upper.resp = unname(quantile(resp, c(0.95))))
  t2.reps <- as.data.frame(cbind(elev.vec, 'resurvey', pred.res.reps))
  t2.reps.tall <- gather(t2.reps, "rep", "preds", 3:102)
  t2.summary <- t2.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec) %>% 
    summarise(mean.resp = mean(resp),
              lower.resp = unname(quantile(resp, c(0.05))),
              upper.resp = unname(quantile(resp, c(0.95))))
  graph.dat <- bind_rows(t1.summary, t2.summary)
  graph.dat$elev.vec <- as.numeric(graph.dat$elev.vec)
  
  gg <- ggplot(graph.dat, aes(x=elev.vec, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=lower.resp, ymax=upper.resp), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
  ggsave(paste("figures/model.preds_",sp,".pdf",sep=""), gg, width=5, height=5)
  
}
