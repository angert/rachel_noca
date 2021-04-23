# Created: Feb. 16, 2021
# Mofigirf: Apr. 23, 2021
# This script is Amy's first attempt at visualizing model outputs

library(tidyverse)

#### read in and prepare tables of coefficients

# coefficients from all top models for each rarefied dataset that ran without warnings
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)

# filter to just model average for each rarefied dataset
coeff.avgs <- coeff.ALLDAT %>% filter(Type=="Avg") # now we have 100 model averages per species (but shouldn't this be <100 for the species for which some rarefied datasets threw warnings?)

# split into species modeled with fire vs without
coeffs.fire <- coeff.avgs %>% filter(Fire.Included=="Yes")
coeffs.nofire <- coeff.avgs %>% filter(Fire.Included=="No")

#### species lists
species.fire <- coeffs.fire %>% 
  group_by(Species) %>% 
  summarise(species.fire=first(Species))

load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS"] #removing "MOSS"
species.list <- factor(species.list)

### good spp - no fire (n=29)
#"ACCI", "ACGL", "ATFI", "CHUM", "CLUN", "COCA", "GASH", "GOOB", "GYDR", "LIBO", "MANE", "MEFE", "OPHO", "POMU", "PTAQ", "RUPA", "RUPE", "RUSP", "SOSI", "TITR", "VASI"
#"AMAL" (but based on 98 datasets without warnings)
#"CAME" (but based on 74 datasets without warnings)
#"GAOV" (but based on 99 datasets without warnings)
#"HIAL" (but based on 99 datasets without warnings)
#"HODI" (but cannot run most complex model)
#"RULA" (but based on 99 datasets without warnings)
#"SPBE" (but 35 datasets meet criterion for fire)
#"TRBO" (but based on 70 datasets without warnings)

### good spp - fire (n=7)
#"ACMI", "CEVE", "EPAN", "PAMY"
#"ARUV" (but based on 98 datasets without warnings)
#"CARU" (but 21 datasets meet criterion for no fire)
#"VAME" (but cannot run most complex model)

### bad spp (n=6)
#"COST", "LUPE", "PHEM", "RHAL", "VAAL", "VADE"

# drop bad species per above
problems = c("COST", "LUPE", "PHEM", "RHAL", "VAAL", "VADE") 
species.short <- anti_join(as.data.frame(species.list), as.data.frame(problems), by=c("species.list"="problems"))

coeffs <- semi_join(coeff.avgs, species.short, by=c("Species"="species.list")) #should be length 100*n less than coeff.avgs; where n=# problematic species


# focusing on species with updated fire interaction models first
species.fire <- coeffs.fire %>% mutate()
species.fire <- semi_join(species.short, coeffs.fire, by=c("species.list"="Species")) %>% 
  droplevels()
species.fire <- species.fire %>% 
  filter(species.list!="AMAL") %>% 
  filter(species.list!="SPBE") %>% 
  droplevels()

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

### empty matrices for writing best-fit lines into
pred.leg.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.unburn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)
pred.res.burn.reps = matrix(nrow=length(elev.vec.lin),ncol=100)

col.pal <- c("turquoise4", "red3", "goldenrod1")

for (i in 1:dim(species.fire)[1]) {
  sp = species.fire[i,1]
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
    xlab("Elevation (m)") +
    ylab("Probability of presence") +
    geom_errorbar(aes(ymin=lower.resp, ymax=upper.resp), alpha=0.05) +
    geom_line() +
    scale_color_manual(values=col.pal)
  
 ggsave(paste("figures/model.preds_ortho_",sp,".pdf",sep=""), gg, width=5, height=5)

} 
  
# now the species without fire
species.nofire <- semi_join(species.short, coeffs.nofire, by=c("species.list"="Species")) %>% 
  droplevels()
species.nofire <- species.nofire %>% 
  filter(species.list!="CARU") %>% 
  filter(species.list!="ARUV") %>% 
  filter(species.list!="VAME") %>% 
  droplevels()


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
