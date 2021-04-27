# Created: Feb. 16, 2021
# Modified: Apr. 23, 2021
# This script visualizes model-averaged predictions for each rarefied dataset

library(tidyverse)

#### read in and prepare tables of coefficients

# coefficients from all top models for each rarefied dataset that ran without warnings
# these models use the poly() formulaton for orthogonal elevation^2 terms
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)

# filter to just model average for each rarefied dataset
coeff.avgs <- coeff.ALLDAT %>% filter(Type=="Avg") # now we have 100 model averages per species (or, <100 for the species for which some rarefied datasets threw warnings?)

# split into species modeled with fire vs without
coeffs.fire <- coeff.avgs %>% filter(Fire.Included=="Yes")
coeffs.nofire <- coeff.avgs %>% filter(Fire.Included=="No")

#### species lists

## good spp - fire (n=7)
#"ACMI", "CEVE", "EPAN", "PAMY"
#"ARUV" (but based on 98 datasets without warnings)
#"CARU" (but 21 datasets meet criterion for no fire)
#"VAME" (but cannot run most complex model)species.list.fire <- coeffs.fire %>% 

## good spp - no fire (n=29)
#"ACCI", "ACGL", "ATFI", "CHUM", "CLUN", "COCA", "GASH", "GOOB", "GYDR", "LIBO", "MANE", "MEFE", "OPHO", "POMU", "PTAQ", "RUPA", "RUPE", "RUSP", "SOSI", "TITR", "VASI"
#"AMAL" (but based on 98 datasets without warnings)
#"CAME" (but based on 74 datasets without warnings)
#"GAOV" (but based on 99 datasets without warnings)
#"HIAL" (but based on 99 datasets without warnings)
#"HODI" (but cannot run most complex model)
#"RULA" (but based on 99 datasets without warnings)
#"SPBE" (but 35 datasets meet criterion for fire)
#"TRBO" (but based on 70 datasets without warnings)

## bad spp (n=6)
#"COST", "LUPE", "PHEM", "RHAL", "VAAL", "VADE"

species.list.fire <- coeffs.fire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))

species.list.nofire <- coeffs.nofire %>% 
  group_by(Species) %>% 
  summarise(Species=first(Species))


#### elevation vector for multiplying by coefficients
# needs to be in poly-transformed units

# exploring to determine what poly-tranformed values should be
dat <- read_csv("data/3c_transformed_polynomials.csv")

# relationship between quadratic and linear terms in poly-transformed units is a perfect quadratic
ggplot(data=dat, aes(x=Elevation.m.poly, y=Elevation.m2.poly)) +
  geom_point() +
  geom_smooth(method="lm", formula= y~poly(x,2))

# quadratic function is given by this model
poly.mod <- lm(Elevation.m2.poly ~ Elevation.m.poly + I(Elevation.m.poly^2), data=dat)

# linear vector
# range based on min/max values in dat$Elevation.m.poly
elev.vec.lin = as.numeric(seq(min(dat$Elevation.m.poly), max(dat$Elevation.m.poly), by=0.0001)) 

# quadratic vector
elev.vec.quad = poly.mod$coefficients[1] + 
  elev.vec.lin*poly.mod$coefficients[2] + 
  elev.vec.lin*elev.vec.lin*poly.mod$coefficients[3]
plot(elev.vec.quad ~ elev.vec.lin) + 
  points(dat$Elevation.m.poly, dat$Elevation.m2.poly, col="red") #ok! we have linear and quadratic elevation vectors that match what the models are using


#### elevation list for back-transformed axis labels
# needs to be in raw units (m)
ggplot(data=dat, aes(x=Elevation.m.poly, y=Elevation.m)) +
  geom_point() +
  geom_smooth(method="lm")

poly.ticks.default = seq(-0.08, 0.08, by=0.04)
back.mod.default <- lm(Elevation.m ~ Elevation.m.poly, data=dat)
raw.ticks.default = back.mod.default$coefficients[1] + poly.ticks.default*back.mod.default$coefficients[2]

back.mod.custom <- lm(Elevation.m.poly ~ Elevation.m, data=dat)
raw.ticks.custom = c(100, 600, 1100, 1600, 2100)
poly.ticks.custom = back.mod.custom$coefficients[1] + raw.ticks.custom*back.mod.custom$coefficients[2]

#### other prep work
# empty matrices for writing best-fit lines into
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)

# function for converting predictions to 0-1 response scale
response = function(y) {
  exp(as.numeric(y))/(1+exp(as.numeric(y)))
  }
  
# color palette for fire graphs
col.pal <- c("turquoise4", "red3", "goldenrod1")

#### big loop to calculate predicted values across each rarefaction for each species
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
    geom_line(data=graph.dat.tall, aes(group=interaction(V2, rep), color=V2), alpha=0.15) +
    geom_line(size=2, linetype="dotted") +
    theme_classic() +
    scale_color_manual("Time x fire", values=col.pal, labels=c("legacy", "resurvey, burned", "resurvey, unburned")) +
    scale_x_continuous(breaks=poly.ticks.custom, labels=raw.ticks.custom) +
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

# new color palette
col.pal <- c("turquoise4", "goldenrod1")

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
    scale_x_continuous(breaks=poly.ticks.custom, labels=raw.ticks.custom) +
    xlab("Elevation (m)") +
    ylab("Probability of presence")
  
  ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)  
}
