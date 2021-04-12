# Created: Mar 24, 2021
# Last edited: Apr 12, 2021

library(tidyverse)

### read in model coefficients
# these are from the global (full) model for each rarefied dataset that ran without warnings
# using poly() function for orthogonal quadratic terms
coeff.glob.fire <- read.csv("data/3c_ORTHO_global_mod_coefficients_with_P_FIRE.csv", header = TRUE)
coeff.glob.nofire <- read.csv("data/3c_ORTHO_global_mod_coefficients_with_P_NOFIRE.csv", header = TRUE)


### species lists for loops below
species.fire <- coeff.glob.fire %>% 
  group_by(Species) %>% 
  summarise(Species = first(Species))
species.nofire <- coeff.glob.nofire %>% 
  group_by(Species) %>% 
  summarise(Species = first(Species))


### elevation vector for multiplying by coefficients
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


### graphs for fire species

# empty matrices to store values
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.leg.reps.low = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps.low = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps.low = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.leg.reps.high = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps.high = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps.high = matrix(nrow=length(elev.vec.lin),ncol=100)

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
           ResurvUnburnXElev2 = Elevation.m2.Res.Unburn.fi,
           Int.Low=Intercept.CI.Lower, 
           Elev.Low=Elevation.m.CI.Lower, 
           Elev2.Low=Elevation.m2.CI.Lower, 
           ResurvBurn.Low = Resurvey.Burned.fi.CI.Lower,
           ResurvUnburn.Low = Resurvey.Unburned.fi.CI.Lower,
           ResurvBurnxElev.Low = Elevation.m.Res.Burn.fi.CI.Lower,
           ResurvBurnxElev2.Low = Elevation.m2.Res.Burn.fi.CI.Lower,
           ResurvUnburnxElev.Low = Elevation.m2.Res.Unburn.fi.CI.Lower,
           ResurvUnburnXElev2.Low = Elevation.m2.Res.Unburn.fi.CI.Lower,
           Int.High=Intercept.CI.Upper, 
           Elev.High=Elevation.m.CI.Upper, 
           Elev2.High=Elevation.m2.CI.Upper, 
           ResurvBurn.High = Resurvey.Burned.fi.CI.Upper,
           ResurvUnburn.High = Resurvey.Unburned.fi.CI.Upper,
           ResurvBurnxElev.High = Elevation.m.Res.Burn.fi.CI.Upper,
           ResurvBurnxElev2.High = Elevation.m2.Res.Burn.fi.CI.Upper,
           ResurvUnburnxElev.High = Elevation.m2.Res.Unburn.fi.CI.Upper,
           ResurvUnburnXElev2.High = Elevation.m2.Res.Unburn.fi.CI.Upper
    )
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad
    pred.res.unburn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad +
      mods$ResurvUnburn[j] + mods$ResurvUnburnxElev[j]*elev.vec.lin + 
      mods$ResurvUnburnXElev2[j]*elev.vec.quad
    pred.res.burn.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad +
      mods$ResurvBurn[j] + mods$ResurvBurnxElev[j]*elev.vec.lin + 
      mods$ResurvBurnxElev2[j]*elev.vec.quad
    pred.leg.reps.low[,j] = mods$Int.Low[j] + mods$Elev.Low[j]*elev.vec.lin + mods$Elev2.Low[j]*elev.vec.quad
    pred.res.unburn.reps.low[,j] = mods$Int.Low[j] + mods$Elev.Low[j]*elev.vec.lin + mods$Elev2.Low[j]*elev.vec.quad +
      mods$ResurvUnburn.Low[j] + mods$ResurvUnburnxElev.Low[j]*elev.vec.lin + 
      mods$ResurvUnburnXElev2.Low[j]*elev.vec.quad
    pred.res.burn.reps.low[,j] = mods$Int.Low[j] + mods$Elev.Low[j]*elev.vec.lin + mods$Elev2.Low[j]*elev.vec.quad +
      mods$ResurvBurn.Low[j] + mods$ResurvBurnxElev.Low[j]*elev.vec.lin + 
      mods$ResurvBurnxElev2.Low[j]*elev.vec.quad
    pred.leg.reps.high[,j] = mods$Int.High[j] + mods$Elev.High[j]*elev.vec.lin + mods$Elev2.High[j]*elev.vec.quad
    pred.res.unburn.reps.high[,j] = mods$Int.High[j] + mods$Elev.High[j]*elev.vec.lin + mods$Elev2.High[j]*elev.vec.quad +
      mods$ResurvUnburn.High[j] + mods$ResurvUnburnxElev.High[j]*elev.vec.lin + 
      mods$ResurvUnburnXElev2.High[j]*elev.vec.quad
    pred.res.burn.reps.high[,j] = mods$Int.High[j] + mods$Elev.High[j]*elev.vec.lin + mods$Elev2.High[j]*elev.vec.quad +
      mods$ResurvBurn.High[j] + mods$ResurvBurnxElev.High[j]*elev.vec.lin + 
      mods$ResurvBurnxElev2.High[j]*elev.vec.quad
  }
  t1.unburn.reps <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps))
  t1.unburn.reps.tall <- gather(t1.unburn.reps, "rep", "preds", 3:102)
  t1.unburn.summary <- t1.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE))
  t1.unburn.reps.low <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps.low))
  t1.unburn.reps.low.tall <- gather(t1.unburn.reps.low, "rep", "preds", 3:102)
  t1.unburn.low.summary <- t1.unburn.reps.low.tall %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.lower = mean(lower, na.rm=TRUE))
  t1.unburn.reps.high <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps.high))
  t1.unburn.reps.high.tall <- gather(t1.unburn.reps.high, "rep", "preds", 3:102)
  t1.unburn.high.summary <- t1.unburn.reps.high.tall %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.upper = mean(upper, na.rm=TRUE))
  t2.unburn.reps <- as.data.frame(cbind(elev.vec.lin, 'res.unburn', pred.res.unburn.reps))
  t2.unburn.reps.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102)
  t2.unburn.summary <- t2.unburn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE))
  t2.unburn.reps.low <- as.data.frame(cbind(elev.vec.lin, 'res.unburn', pred.res.unburn.reps.low))
  t2.unburn.reps.low.tall <- gather(t2.unburn.reps, "rep", "preds", 3:102)
  t2.unburn.low.summary <- t2.unburn.reps.low.tall %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.lower = mean(lower, na.rm=TRUE))
  t2.unburn.reps.high <- as.data.frame(cbind(elev.vec.lin, 'res.unburn', pred.res.unburn.reps.high))
  t2.unburn.reps.high.tall <- gather(t2.unburn.reps.high, "rep", "preds", 3:102)
  t2.unburn.high.summary <- t2.unburn.reps.high.tall %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.upper = mean(upper, na.rm=TRUE))
  t2.burn.reps <- as.data.frame(cbind(elev.vec.lin, 'res.burn', pred.res.burn.reps))
  t2.burn.reps.tall <- gather(t2.burn.reps, "rep", "preds", 3:102)
  t2.burn.summary <- t2.burn.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(resp, na.rm=TRUE))
  t2.burn.reps.low <- as.data.frame(cbind(elev.vec.lin, 'res.burn', pred.res.burn.reps.low))
  t2.burn.reps.low.tall <- gather(t2.burn.reps.low, "rep", "preds", 3:102)
  t2.burn.low.summary <- t2.burn.reps.low.tall %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.lower = mean(lower, na.rm=TRUE))
  t2.burn.reps.high <- as.data.frame(cbind(elev.vec.lin, 'res.burn', pred.res.burn.reps.high))
  t2.burn.reps.high.tall <- gather(t2.burn.reps.high, "rep", "preds", 3:102)
  t2.burn.high.summary <- t2.burn.reps.high.tall %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.upper = mean(upper, na.rm=TRUE))
  graph.dat.means <- bind_rows(t1.unburn.summary, t2.unburn.summary, t2.burn.summary)
  graph.dat.lowers <- bind_rows(t1.unburn.low.summary, t2.unburn.low.summary, t2.burn.low.summary)
  graph.dat.uppers <- bind_rows(t1.unburn.high.summary, t2.unburn.high.summary, t2.burn.high.summary)
  graph.dat <- left_join(graph.dat.means, left_join(graph.dat.lowers, graph.dat.uppers))
  graph.dat$elev.vec.lin <- as.numeric(graph.dat$elev.vec.lin)

 gg <- ggplot(graph.dat, aes(x=elev.vec.lin, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=mean.lower, ymax=mean.upper), alpha=0.1) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
 ggsave(paste("figures/global.model.preds_",sp,".pdf",sep=""), gg, width=5, height=5)

} 

# temporary code to inspect predictions from individual rarefied datasets  
t1.unburn.V3 <- t1.unburn.reps.tall %>% filter(rep=="V14") %>% mutate(mean.resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t1.unburn.low.V3 <- t1.unburn.reps.low.tall %>% filter(rep=="V14") %>% mutate(mean.lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t1.unburn.high.V3 <- t1.unburn.reps.high.tall %>% filter(rep=="V14") %>% mutate(mean.upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.unburn.V3 <- t2.unburn.reps.tall %>% filter(rep=="V14") %>% mutate(mean.resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.unburn.low.V3 <- t2.unburn.reps.low.tall %>% filter(rep=="V14") %>% mutate(mean.lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.unburn.high.V3 <- t2.unburn.reps.high.tall %>% filter(rep=="V14") %>% mutate(mean.upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.burn.V3 <- t2.burn.reps.tall %>% filter(rep=="V14") %>% mutate(mean.resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.burn.low.V3 <- t2.burn.reps.low.tall %>% filter(rep=="V14") %>% mutate(mean.lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
t2.burn.high.V3 <- t2.burn.reps.high.tall %>% filter(rep=="V14") %>% mutate(mean.upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% select(-preds)
graph.dat.V3 <- bind_rows(t1.unburn.V3, t2.unburn.V3, t2.burn.V3)
graph.dat.V3.lowers <- bind_rows(t1.unburn.low.V3, t2.unburn.low.V3, t2.burn.low.V3)
graph.dat.V3.uppers <- bind_rows(t1.unburn.high.V3, t2.unburn.high.V3, t2.burn.high.V3)
graph.dat.V3.all <- left_join(graph.dat.V3, left_join(graph.dat.V3.lowers, graph.dat.V3.uppers))
graph.dat.V3.all$elev.vec <- as.numeric(graph.dat.V3.all$elev.vec)



### graphs for no-fire species

# empty matrices to store values
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.leg.reps.low = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.reps.low = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.leg.reps.high = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.reps.high = matrix(nrow=length(elev.vec.lin),ncol=100)

col.pal <- c("turquoise4", "goldenrod1")

for (i in 1:dim(species.nofire)[1]) {
  sp = as.list(species.nofire[i,1], drop=TRUE)
  mods <- coeff.glob.nofire %>% 
    filter(Species==sp) %>% 
    dplyr::select(Int=Intercept, 
           Elev=Elevation.m, 
           Elev2=Elevation.m2, 
           Year = Data.Type.nofi,
           YearxElev = Data.Type.Elevation.m.nofi,
           YearxElev2 = Data.Type.Elevation.m2.nofi,
           Int.Low=Intercept.CI.Lower, 
           Elev.Low=Elevation.m.CI.Lower, 
           Elev2.Low=Elevation.m2.CI.Lower, 
           Year.Low = Data.Type.nofi.CI.Lower,
           YearxElev.Low = Data.Type.Elevation.m.nofi.CI.Lower,
           YearxElev2.Low = Data.Type.Elevation.m2.nofi.CI.Lower,
           Int.High=Intercept.CI.Upper, 
           Elev.High=Elevation.m.CI.Upper, 
           Elev2.High=Elevation.m2.CI.Upper, 
           Year.High = Data.Type.nofi.CI.Upper,
           YearxElev.High = Data.Type.Elevation.m.nofi.CI.Upper,
           YearxElev2.High = Data.Type.Elevation.m2.nofi.CI.Upper) %>% 
        droplevels()
    
  for (j in 1:dim(mods)[1]) {
    pred.leg.reps[,j] = mods$Int[j] + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad
    pred.leg.reps.low[,j] = mods$Int.Low[j] + mods$Elev.Low[j]*elev.vec.lin + mods$Elev2.Low[j]*elev.vec.quad
    pred.leg.reps.high[,j] = mods$Int.High[j] + mods$Elev.High[j]*elev.vec.lin + mods$Elev2.High[j]*elev.vec.quad
    pred.res.reps[,j] = mods$Int[j]  + mods$Elev[j]*elev.vec.lin + mods$Elev2[j]*elev.vec.quad +
      mods$Year[j] + mods$YearxElev[j]*elev.vec.lin + mods$YearxElev2[j]*elev.vec.quad
    pred.res.reps.low[,j] = mods$Int.Low[j]  + mods$Elev.Low[j]*elev.vec.lin + mods$Elev2.Low[j]*elev.vec.quad +
      mods$Year.Low[j] + mods$YearxElev.Low[j]*elev.vec.lin + mods$YearxElev2.Low[j]*elev.vec.quad
    pred.res.reps.high[,j] = mods$Int.High[j]  + mods$Elev.High[j]*elev.vec.lin + mods$Elev2.High[j]*elev.vec.quad +
      mods$Year.High[j] + mods$YearxElev.High[j]*elev.vec.lin + mods$YearxElev2.High[j]*elev.vec.quad
  }
  t1.reps <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps))
  t1.reps.tall <- gather(t1.reps, "rep", "preds", 3:102)
  t1.summary <- t1.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(resp))
  t1.reps.low <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps.low))
  t1.reps.low.tall <- gather(t1.reps.low, "rep", "preds", 3:102)
  t1.summary.low <- t1.reps.low.tall %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.lower = mean(lower))
  t1.reps.high <- as.data.frame(cbind(elev.vec.lin, 'legacy', pred.leg.reps.high))
  t1.reps.high.tall <- gather(t1.reps.high, "rep", "preds", 3:102)
  t1.summary.high <- t1.reps.high.tall %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.upper = mean(upper))
  t2.reps <- as.data.frame(cbind(elev.vec.lin, 'resurvey', pred.res.reps))
  t2.reps.tall <- gather(t2.reps, "rep", "preds", 3:102)
  t2.summary <- t2.reps.tall %>% 
    mutate(resp = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.resp = mean(resp))
  t2.reps.low <- as.data.frame(cbind(elev.vec.lin, 'resurvey', pred.res.reps.low))
  t2.reps.low.tall <- gather(t2.reps.low, "rep", "preds", 3:102)
  t2.summary.low <- t2.reps.low.tall %>% 
    mutate(lower = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.lower = mean(lower))
  t2.reps.high <- as.data.frame(cbind(elev.vec.lin, 'resurvey', pred.res.reps.high))
  t2.reps.high.tall <- gather(t2.reps.high, "rep", "preds", 3:102)
  t2.summary.high <- t2.reps.high.tall %>% 
    mutate(upper = exp(as.numeric(preds))/(1+exp(as.numeric(preds)))) %>% 
    group_by(V2, elev.vec.lin) %>% 
    summarise(mean.upper = mean(upper))
  graph.dat.means <- bind_rows(t1.summary, t2.summary)
  graph.dat.lowers <- bind_rows(t1.summary.low, t2.summary.low)
  graph.dat.uppers <- bind_rows(t1.summary.high, t2.summary.high)
  graph.dat <- left_join(graph.dat.means, left_join(graph.dat.lowers, graph.dat.uppers))
  graph.dat$elev.vec.lin <- as.numeric(graph.dat$elev.vec.lin)
  
  gg <- ggplot(graph.dat, aes(x=elev.vec.lin, y=mean.resp, color=V2)) +
    theme_classic() +
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=mean.lower, ymax=mean.upper), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
  ggsave(paste("figures/global.model.preds_",sp,".pdf",sep=""), gg, width=5, height=5)
  
}
