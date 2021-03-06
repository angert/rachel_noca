# Created: Feb. 16, 2021
# Modified: May 29, 2021

#### This script visualizes model-averaged predictions for each rarefied dataset

library(tidyverse)
library(cowplot)

#### Read in and prepare tables of coefficients

# coefficients from all top models for each rarefied dataset that ran without warnings
# these models use the poly() formulaton for orthogonal elevation^2 terms
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)

# filter to just model average for each rarefied dataset
coeff.avgs <- coeff.ALLDAT %>% filter(Type=="Avg") # now we have 100 model averages per species (or, <100 for the species for which some rarefied datasets threw warnings?)

# split into species modeled with fire vs without
coeffs.fire <- coeff.avgs %>% filter(Fire.Included=="Yes")
coeffs.nofire <- coeff.avgs %>% filter(Fire.Included=="No")

# species lists for ugly loops below
species.list.fire <- coeffs.fire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))
species.list.nofire <- coeffs.nofire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))


#### Elevation vectors for multiplying by coefficients

# values in poly-transformed units
dat <- read_csv("data/3c_transformed_polynomials.csv")

# linear term vector
# range based on min/max values 
elev.vec.lin = as.numeric(seq(min(dat$Elevation.m.poly), max(dat$Elevation.m.poly), by=0.0001)) 

# quadratic term vector
# quadratic function is given by this model
poly.mod <- lm(Elevation.m2.poly ~ Elevation.m.poly + I(Elevation.m.poly^2), data=dat)

elev.vec.quad = poly.mod$coefficients[1] + 
  elev.vec.lin*poly.mod$coefficients[2] + 
  elev.vec.lin*elev.vec.lin*poly.mod$coefficients[3]

# elevation list for back-transformed axis labels
# needs to be in raw units (m)
back.mod <- lm(Elevation.m.poly ~ Elevation.m, data=dat)
raw.ticks = c(100, 600, 1100, 1600, 2100)
poly.ticks = back.mod$coefficients[1] + raw.ticks*back.mod$coefficients[2]


#### other prep work

# function for converting predictions to 0-1 response scale
response = function(y) {
  exp(as.numeric(y))/(1+exp(as.numeric(y)))
  }

# color palettes 
# for fire species
col.pal.fire <- c("turquoise4", "red3", "goldenrod1")
# for no-fire species
col.pal.nofire <- c("turquoise4", "goldenrod1")


#### FIRE SPECIES: big loop to calculate predicted values across each rarefaction for each species

# empty matrices for writing best-fit lines into
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)

for (i in 1:dim(species.list.fire)[1]) {
  sp = as.list(species.list.fire[i,1]) 
  mods <- coeffs.fire %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           ResurvBurn = Resurvey.Burned.fi,
           ResurvUnburn = Resurvey.Unburned.fi,
           ResurvBurnxElev = Elevation.m.Res.Burn.fi,
           ResurvBurnxElev2 = Elevation.m2.Res.Burn.fi,
           ResurvUnburnxElev = Elevation.m2.Res.Unburn.fi,
           ResurvUnburnXElev2 = Elevation.m2.Res.Unburn.fi) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad
    
    pred.res.unburn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad + mods$ResurvUnburn[j] + mods$ResurvUnburnxElev[j]*elev.vec.lin + mods$ResurvUnburnXElev2[j]*elev.vec.quad
    
    pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad + mods$ResurvBurn[j] + mods$ResurvBurnxElev[j]*elev.vec.lin 
    + mods$ResurvBurnxElev2[j]*elev.vec.quad
  }
  
  t1.unburn.reps <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps)) %>% 
    mutate(across(c(3:102), response))
  t1.unburn.reps.tall <- gather(t1.unburn.reps, "rep", "preds", 3:102) %>% 
    mutate(elev.vec.lin = as.numeric(elev.vec.lin))
  t1.unburn.summary <- t1.unburn.reps.tall %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(preds),
              lower.resp = unname(quantile(preds, c(0.05))),
              upper.resp = unname(quantile(preds, c(0.95))))
  t2.unburn.reps <- as.data.frame(cbind(elev.vec.lin, 'res.unburn', pred.res.unburn.reps)) %>% 
    mutate(across(c(3:102), response))
  t2.unburn.reps.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102) %>% 
    mutate(elev.vec.lin = as.numeric(elev.vec.lin))
  t2.unburn.summary <- t2.unburn.reps.tall %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(preds),
              lower.resp = unname(quantile(preds, c(0.05))),
              upper.resp = unname(quantile(preds, c(0.95))))
  t2.burn.reps <- as.data.frame(cbind(elev.vec.lin, 'res.burn', pred.res.burn.reps)) %>% 
    mutate(across(c(3:102), response))
  t2.burn.reps.tall <- gather(t2.burn.reps, "rep", "preds", 3:102) %>% 
    mutate(elev.vec.lin = as.numeric(elev.vec.lin))
  t2.burn.summary <- t2.burn.reps.tall %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(preds),
              lower.resp = unname(quantile(preds, c(0.05))),
              upper.resp = unname(quantile(preds, c(0.95))))
  graph.dat.tall <- bind_rows(t1.unburn.reps.tall, t2.unburn.reps.tall, t2.burn.reps.tall)  
  graph.dat.means <- bind_rows(t1.unburn.summary, t2.unburn.summary, t2.burn.summary) %>% 
    mutate(preds = mean.resp)
  
  gg <- ggplot(graph.dat.means, aes(x = elev.vec.lin, y = preds, color = V2)) + 
    geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15, show.legend = FALSE) +
    geom_line(size=2, linetype="dotted", show.legend = FALSE) +
    theme_classic() +
    scale_color_manual("Time x fire", values=col.pal.fire, labels=c("legacy", "resurvey, burned", "resurvey, unburned")) +
    scale_x_continuous(breaks=poly.ticks, labels=raw.ticks) +
    xlab("") + #Elevation (m)
    ylab("") #Probability of presence
  
  #ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)
  
  assign(paste0("preds_graph_",sp), gg)
} 

# repeat last plot with legend so that legend can be saved for multi-panel fig
gg <- ggplot(graph.dat.means, aes(x = elev.vec.lin, y = preds, color = V2)) + 
  geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15) +
  geom_line(size=2, linetype="dotted") +
  scale_color_manual("Time x fire", values=col.pal.fire, labels=c("legacy", "resurvey, burned", "resurvey, unburned")) +
  theme_classic()

legend.fire = get_legend(gg)


#### NO-FIRE SPECIES: big loop to calculate predicted values across each rarefaction for each species

# empty matrices to store predictions
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.reps = matrix(nrow=length(elev.vec.lin),ncol=100)

for (i in 1:dim(species.list.nofire)[1]) {
  sp = as.list(species.list.nofire[i,1])
  mods <- coeffs.nofire %>% 
    filter(Species==sp) %>% 
    select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           Year = Data.Type.nofi,
           YearxElev = Data.Type.Elevation.m.nofi,
           YearxElev2 = Data.Type.Elevation.m2.nofi) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad
    
    pred.res.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad + mods$Year[j] + mods$YearxElev[j]*elev.vec.lin + mods$YearxElev2[j]*elev.vec.quad
  }
  
  t1.reps <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps)) %>% 
    mutate(across(c(3:102), response))
  t1.reps.tall <- gather(t1.reps, "rep", "preds", 3:102) %>% 
    mutate(elev.vec.lin = as.numeric(elev.vec.lin))
  t1.summary <- t1.reps.tall %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(preds),
              lower.resp = unname(quantile(preds, c(0.05))),
              upper.resp = unname(quantile(preds, c(0.95))))
  t2.reps <- as.data.frame(cbind(elev.vec.lin, 'resurvey', pred.res.reps)) %>% 
    mutate(across(c(3:102), response))
  t2.reps.tall <- gather(t2.reps, "rep", "preds", 3:102) %>% 
    mutate(elev.vec.lin = as.numeric(elev.vec.lin))
  t2.summary <- t2.reps.tall %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(preds),
              lower.resp = unname(quantile(preds, c(0.05))),
              upper.resp = unname(quantile(preds, c(0.95))))
  graph.dat.tall <- bind_rows(t1.reps.tall, t2.reps.tall)  
  
  graph.dat.means <- bind_rows(t1.summary, t2.summary) %>% 
    mutate(preds=mean.resp)
  
  gg <- ggplot(graph.dat.means, aes(x = elev.vec.lin, y = preds, color = V2)) + 
    geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15, show.legend = FALSE) +
    geom_line(size=2, linetype="dotted", show.legend = FALSE) +
    theme_classic() +
    scale_color_manual(name="TIME", values=col.pal.nofire, labels=c("legacy", "resurvey")) + 
    scale_x_continuous(breaks=poly.ticks, labels=raw.ticks) +
    xlab("") + #Elevation (m)
    ylab("") #Probability of presence
  
  #ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)  
  
  assign(paste0("preds_graph_",sp), gg)
}


#### assemble example species into multi-panel figure

multi <- plot_grid(preds_graph_ARUV, preds_graph_EPAN, NULL, 
                   preds_graph_MANE, preds_graph_CHUM, preds_graph_OPHO,
                   nrow=2, ncol=3,
                   labels=c("A","B","", "C","D","E")) +
  draw_grob(legend.fire, 2/3, 0.5, 1/3, 0.5) + 
  theme(plot.margin = margin(10, 10, 5, 50))

multi.x <- ggdraw(add_sub(multi, "Elevation (m)", size=14, x=0.5, y=0.05, hjust=0.5, vjust=0)) 
multi.x
multi.xy <- ggdraw(add_sub(multi.x, "Probability of presence", size=14, x=0.05, y=2, hjust=0.5, vjust=0.5, angle=90))
multi.xy

ggsave("figures/model_preds_multipanel.pdf", multi.xy, width=8, height=6)
