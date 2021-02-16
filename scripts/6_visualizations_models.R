# Created: Feb. 16, 2020
# This script is Amy's first attempt at visualizing model outputs

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

elev.vec = seq(0, 3000, by=100)

for (i in 1:dim(species.fire)[1]) {
  sp = species.fire[i,]
  mod <- coeffs %>% 
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
           YearxElev2xFire=Data.Type.Elevation.m2.Fires) %>% 
    summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE)))
  pred.leg.unburn = mod$Int + mod$Elev*elev.vec + mod$Elev2*elev.vec*elev.vec
  pred.res.unburn = mod$Int + mod$Elev*elev.vec + mod$Elev2*elev.vec*elev.vec +
                    mod$Year + mod$YearxElev*elev.vec + mod$YearxElev2*elev.vec*elev.vec
  pred.res.burn = mod$Int + mod$Elev*elev.vec + mod$Elev2*elev.vec*elev.vec +
                  mod$Year + mod$YearxElev*elev.vec + mod$YearxElev2*elev.vec*elev.vec + 
                  mod$Fire + mod$ElevxFire*elev.vec + mod$YearxFire + mod$Elev2xFire*elev.vec*elev.vec + 
                  mod$YearxElevxFire*elev.vec + mod$YearxElev2xFire*elev.vec*elev.vec
  t1.unburn <- as.data.frame(cbind(elev.vec, pred.leg.unburn, 'leg.unburn'))
  names(t1.unburn)=c("elev", "pred", "trt")
  t2.unburn <- as.data.frame(cbind(elev.vec, pred.res.unburn, 'res.unburn'))
  names(t2.unburn)=c("elev", "pred", "trt")
  t2.burn <- as.data.frame(cbind(elev.vec, pred.res.burn, 'res.burn'))
  names(t2.burn)=c("elev", "pred", "trt")
  graph.dat <- rbind(t1.unburn, t2.unburn, t2.burn)
  graph.dat$pred <- as.numeric(graph.dat$pred)
  graph.dat$elev <- as.numeric(graph.dat$elev)
  gg <- ggplot(graph.dat, aes(x=elev, y=pred, color=trt)) +
    geom_line() +
    theme_classic()
  ggsave(paste("figures/model.preds_",sp,".pdf",sep=""),gg, width=5, height=5)
}
  