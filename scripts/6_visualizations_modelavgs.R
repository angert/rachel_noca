# Created: Feb. 16, 2021
# Modified: May 27, 2021
# This script visualizes model-averaged predictions for each rarefied dataset

library(tidyverse)

#### elevation vectors for multiplying by model coefficients
# in poly-transformed units
dat <- read_csv("data/3c_transformed_polynomials.csv")
elev.vec.lin = as.numeric(seq(min(dat$Elevation.m.poly), max(dat$Elevation.m.poly), by=0.0001)) 
poly.mod <- lm(Elevation.m2.poly ~ Elevation.m.poly + I(Elevation.m.poly^2), data=dat)
elev.vec.quad = poly.mod$coefficients[1] + elev.vec.lin*poly.mod$coefficients[2] + elev.vec.lin*elev.vec.lin*poly.mod$coefficients[3]


#### read in and prepare tables of coefficients

# coefficients from all top models for each rarefied dataset that ran without warnings
# these models use the poly() formulaton for orthogonal elevation^2 terms
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)

## FIRE SPECIES DATA
# filter to just model average for each rarefied dataset
# should have 100 model averages per species (or, <100 for the species for which some rarefied datasets threw warnings?)
coeffs.fire <- coeff.ALLDAT %>% 
  filter(Type=="Avg") %>% 
  filter(Fire.Included=="Yes") %>% 
  # slim down to only columns of interest, rename for convenience
  select(Species,
         Int=Intercept, 
         Elev=Elevation.m, 
         Elev2=Elevation.m2, 
         ResurvBurn = Resurvey.Burned.fi,
         ResurvUnburn = Resurvey.Unburned.fi,
         ResurvBurnxElev = Elevation.m.Res.Burn.fi,
         ResurvBurnxElev2 = Elevation.m2.Res.Burn.fi,
         ResurvUnburnxElev = Elevation.m2.Res.Unburn.fi,
         ResurvUnburnXElev2 = Elevation.m2.Res.Unburn.fi) %>% 
  # replace na coefficients with zeros
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  # and turn the table into nested data  
  group_by(Species) %>% 
  nest()

## NO-FIRE SPECIES DATA
# filter to just model average for each rarefied dataset
# should have 100 model averages per species (or, <100 for the species for which some rarefied datasets threw warnings?)
coeffs.nofire <- coeff.ALLDAT %>% 
  filter(Type=="Avg") %>% 
  filter(Fire.Included=="No") %>% 
  # slim down to only columns of interest, rename for convenience
  select(Species,
         Int=Intercept, 
         Elev=Elevation.m, 
         Elev2=Elevation.m2, 
         Year = Data.Type.nofi,
         YearxElev = Data.Type.Elevation.m.nofi,
         YearxElev2 = Data.Type.Elevation.m2.nofi) %>% 
  # replace na coefficients with zeros
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  # and turn the table into nested data  
  group_by(Species) %>% 
  nest()


#### Other prep work

# functions for calculating best-fit lines

pred.leg.reps <- function(data, elev.vec.lin, elev.vec.quad) {
  data$Int +
    map(data$Elev, ~.*elev.vec.lin) +
    map(data$Elev2, ~.*elev.vec.quad)
  #data$Int + data$Elev*elev.vec.lin + data$Elev2*elev.vec.quad
  }
tmp.int + 
lin <- map(tmp.el, ~.*elev.vec.lin) 
quad <- map(tmp.el2, ~.*elev.vec.quad)

test <- map2(lin, quad, sum)

  map(data$Elev2, ~.*elev.vec.quad)
pred.leg.reps(coeffs.fire$data[[1]], elev.vec.lin, elev.vec.quad)

pred.res.unburn.reps <- function(data, elev.vec.lin, elev.vec.quad) {
  data$Int + data$Elev*elev.vec.lin + data$Elev2*elev.vec.quad + data$ResurvUnburn +     data$ResurvUnburnxElev*elev.vec.lin + data$ResurvUnburnXElev2*elev.vec.quad 
} 

pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad + mods$ResurvBurn[j] + mods$ResurvBurnxElev[j]*elev.vec.lin 
+ mods$ResurvBurnxElev2[j]*elev.vec.quad

# function for converting predictions to 0-1 response scale
response = function(y) {
  exp(as.numeric(y))/(1+exp(as.numeric(y)))
  }
  
# color palettes for graphs
col.pal.fire <- c("turquoise4", "red3", "goldenrod1")
col.pal.nofire <- c("turquoise4", "goldenrod1")

# tick marks for graphs
ticks.custom <- read_csv("data/tickmarks.csv")

# empty matrices for writing best-fit lines into
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)


#### big loop to calculate predicted values across each rarefaction for each species --->
#### attempting to convert to functions across lists of nested data frames




for (i in 1:dim(species.list.fire)[1]) {
  sp = as.list(species.list.fire[i,1]) 
  mods <- coeffs.fire %>% 
    filter(Species==sp) 
  
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
    geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15) +
    geom_line(size=2, linetype="dotted") +
    theme_classic() +
    scale_color_manual("Time x fire", values=col.pal, labels=c("legacy", "resurvey, burned", "resurvey, unburned")) +
    scale_x_continuous(breaks=ticks.custom$poly.ticks.custom, labels=ticks.custom$raw.ticks.custom) +
    xlab("Elevation (m)") +
    ylab("Probability of presence")
  
  ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)

} 

#graph.dat <- bind_rows(t1.unburn.all, t2.unburn.all, t2.burn.all)
#graph.dat$elev.vec.lin <- as.numeric(graph.dat$elev.vec.lin)
#gg <- ggplot(graph.dat, aes(x=elev.vec.lin, y=mean.resp, color=V2)) +
#  theme_classic() +
#  xlab("Elevation (m)") +
#  ylab("Probability of presence") +
  #geom_errorbar(aes(ymin=lower.resp, ymax=upper.resp), alpha=0.05) +
#  geom_line() +
#  scale_color_manual(values=col.pal)



#### now the species without fire

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
    geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15) +
    geom_line(size=2, linetype="dotted") +
    theme_classic() +
    scale_color_manual("Time", values=col.pal, labels=c("legacy", "resurvey")) +
    scale_x_continuous(breaks=ticks.custom$poly.ticks.custom, labels=ticks.custom$raw.ticks.custom) +
    xlab("Elevation (m)") +
    ylab("Probability of presence")
  
  ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)  
}
