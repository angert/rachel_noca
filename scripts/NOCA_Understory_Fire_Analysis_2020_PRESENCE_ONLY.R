################## THIS is the file to run PRESENCE analyses including fire ############

######## IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections. The original data (2015_data_Oct23.xml) was ONLY corrected once upon initial data entry for mistakes/to-do's on the hard copy data sheets as well as a few obvious typos.

###### Another very important note!!! ALWAYS ALWAYS ALWAYS restart the console if you are switching from cover analyses to presence absence analyses. The cover analysis removes NAs and replaces the original cover file with a new, complete-cases-only file, BUT I DO WANT NAs in the pres/abs analysis!!! Oct. 27 2020 update: cover is now located in a separate script

###### 2020 updates for manuscript flagged as such

#### Always do this first #####
setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry")
library(reshape2)
library(rms)
library(plyr)
library(survival)
library(MuMIn)
und.cover<-read.csv("Understory_All.csv", header=TRUE, na.strings="") # Note that L = Legacy and R = Resurvey
und.cover$Elevation.m<-as.numeric(as.character(und.cover$Elevation.m)) #gives warning - no worries
lat.long<-read.csv("Lat.Long.csv", header=TRUE, na.strings="")
plot.names<-lat.long[,c(2,3,6)]
names(plot.names)<-c("Plot.2015", "Plot.1980", "Elevation.m")
load("Species.List.Rda")

## Then, change WD to fire folder
setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/Fires")
fires<-read.csv("All_Plots_Wildfire_Join.csv", header=TRUE, na.strings="")

########## Adding fires as a covariate (>1983, <1983, unburned) ##########


fires$CAL_YEAR<-ifelse(fires$CAL_YEAR>=1983, "After 1983", "Before 1983")
fires[is.na(fires$CAL_YEAR)==TRUE,6] <- paste(rep("Unburned", times=length(fires[is.na(fires$CAL_YEAR)==TRUE,6]))) #Any NAs are from plots that were not burned.

#Adding prescribed burns
fires$CAL_YEAR[c(39,48,49,59,60)] <- paste(rep("After 1983", times=5))
names(fires)[2] <- paste("Plot.2015")

#Fixing naming errors
fires[fires$Plot.2015 == "Thor225-m", 2] <- paste("Thor225")

#Adding missing data
fires[nrow(fires) + 1, ] <- c(2014, "Thor221", NA, NA, NA, "Unburned", rep(NA, times = length(fires) - 6))

#Preparing to merge with list of 2015 plot names
fires.covariate <- fires[,c(2,6)]
names.fires <- merge(fires.covariate, plot.names, by="Plot.2015", all.y=TRUE)




#haven't fixed past this point
#Adding plots that missing from All_Plots_Wildfire_Join.csv but are present in Understory_all.csv
names.fires[c(373:378),2]<-paste(c("Unburned","Unburned","After 1983","Unburned","Unburned","Unburned"))
list.fires<-melt(names.fires, id.vars=c("Elevation.m", "CAL_YEAR"), measure.vars=c("Plot.2015", "Plot.1980"))
names(list.fires)<-c("Elevation.m", "Fires","Plot.Year", "Plot")
list.fires.nodup<-list.fires[!duplicated(list.fires$Plot),] #getting rid of duplicates
und.cover.fires<-merge(und.cover, list.fires.nodup, by="Plot")

#2020 update: change fire to 2-level variable (after 1983 only)
und.cover.fires$Fires <- ifelse(und.cover.fires$Fires == "After 1983", "Burned", "Unburned")

#### HERE is where you are overwriting the original und.cover file. USE WITH CARE! ###
und.cover<-und.cover.fires[,c(1,2,3,4,5,8)]
names(und.cover)[4]<-paste("Elevation.m")
# Remove supplemental plots
und.cover<-und.cover[!und.cover$Plot=="Supp2026" & !und.cover$Plot=="Supp5127" & !und.cover$Plot=="ROSS4001REF",]
und.cover$Data.Type<-ifelse(und.cover$Year==1980,"Legacy","Resurvey")
und.cover$Data.Type<-as.factor(und.cover $Data.Type)
und.cover<-und.cover[!und.cover$Plot=="Thor223" & !und.cover$Plot=="8017" & !und.cover$Plot=="4044" & !und.cover$Plot=="Bak494",] #I believe these plots have a history of logging




## Are fires correlated with elevation?
names.fires$Elevation.m<-as.numeric(as.character(names.fires$Elevation.m))
names.fires$CAL_YEAR <- as.factor(names.fires$CAL_YEAR)
names.fires$CAL_YEAR<-relevel(names.fires$CAL_YEAR, ref="Unburned")
#2020 update: change fire to 2-level variable (after 1983 only)
names.fires$CAL_YEAR <- ifelse(names.fires$CAL_YEAR == "After 1983", "Burned", "Unburned")
summary(lm(Elevation.m~CAL_YEAR, data=names.fires))
par(mfrow=c(2,2), mar=c(5,5,2,2))
hist(names.fires$Elevation.m, xlab="Elevation", main="All plots")
hist(names.fires[names.fires$CAL_YEAR=="Unburned",4], xlab="Elevation", main="Unburned")
hist(names.fires[names.fires$CAL_YEAR=="After 1983",4], xlab="Elevation", main="After 1983")
hist(names.fires[names.fires$CAL_YEAR=="Before 1983",4], xlab="Elevation", main="Before 1983")
#Answer: Yes, fires before 1983 correlate strongly and negatively with elevation




###################### Species presence/absence ##################### 

# Important note!! The presabs code DOES INCLUDE presences where the cover was recorded as "NA". This is fine as these species were definitely present, I just forgot to record their cover. HOWEVER, in cover file below, I include an important line of code to remove the NAs, since they otherwise will be accidentally encoded as cover class 6.

#Use this to remove MOSS
species.list<-shifts$Species.Code[!shifts$Species.Code=="MOSS"] 
species.list<-factor(species.list)

coeff.SPEC<-list()
	
# Creating a new data frame from Understory_All of 1s/0s presence/absence
pres.abs<-table(und.cover$Plot, und.cover$Species.Code)
und.presence.small<-melt(pres.abs, id.vars=c("Plot", "Species.Code"))
names(und.presence.small)<-c("Plot", "Species.Code", "Pres.Abs")
und.cover$Elevation.m<-as.numeric(as.character(und.cover$Elevation.m))
und.cover$Fires<-as.factor(und.cover$Fires)
und.cover.small<-aggregate(und.cover[c("Year","Elevation.m")],und.cover[c("Plot", "Fires")],mean)
und.cover.small$Data.Type<-ifelse(und.cover.small$Year==1980,"Legacy","Resurvey")
und.presence<-merge(und.presence.small, und.cover.small, by="Plot")
und.presence$Data.Type<-as.factor(und.presence$Data.Type)
und.presence$Pres.Abs<-ifelse(und.presence$Pres.Abs>=1, 1, 0)
und.presence<-und.presence[complete.cases(und.presence),]

#New: removes clearcut plots that don't make it to cover analysis but were accidentally included for pres/abs
und.presence<-und.presence[!und.presence$Plot=="Thor223" & !und.presence$Plot=="8017" & !und.presence$Plot=="4044" & !und.presence$Plot=="Bak494",] 


### Now, you are ready to do the analysis! If you just need to pick out individual species, code S as a number (below) and skip the loop part

(numbered.species <- data.frame(Species=species.list, No.=rep(1:42)))

for(S in 1:length(species.list)) { #RUN TIME: ~ 30 sec

  und.presence.SPEC=subset(und.presence, Species.Code==levels(species.list)[S])
  und.presence.SPEC$Fires<-relevel(und.presence.SPEC$Fires, ref="Unburned")
  und.presence.SPEC$Elevation.m2<-und.presence.SPEC$Elevation.m^2
  und.presence.SPEC<-und.presence.SPEC[complete.cases(und.presence.SPEC),]
  
  # 2020 update: 3way fire interaction if 5+ plots burned

  ### Did the species occur in burned plots 5+ times?
  num.burns <- table(und.presence.SPEC$Pres.Abs, und.presence.SPEC$Fires, und.presence.SPEC$Data.Type) # 2-part table split by survey year
  if(num.burns[2,2,2] >= 5 | num.burns[2,2,1] >= 5) {
    mod.globfi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2) * Fires, data = und.presence.SPEC, family = "binomial", na.action = na.fail) #global fire model
    dredge.globfi <- dredge(mod.globfi, rank = AIC, subset = dc(Elevation.m, Elevation.m2))
    (model.avg(dredge.globfi, subset = delta <= 2))
  }
  
  #if not, exclude fire from model averaging
  if(num.burns[2,2,2] < 5 | num.burns[2,2,1] < 5) {
    mod.globnofi <- glm(Pres.Abs ~ Data.Type * (Elevation.m + Elevation.m2), data = und.presence.SPEC, family = "binomial", na.action = na.fail) #global w/o fire
    dredge.globnofi <- dredge(mod.globnofi, rank = AIC, subset = dc(Elevation.m, Elevation.m2))
    print(model.avg(dredge.globnofi, subset = delta <= 2))
  }
}
  
  
  
  
  

  ###### Old code - handwritten model selection w fire as independent predictor ######
  #A list of all possible models WITHOUT FIRE:
  mod.a<-glm(Pres.Abs~Elevation.m, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev only
  mod.b<-glm(Pres.Abs~Elevation.m+Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + year
  mod.c<-glm(Pres.Abs~Elevation.m*Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev*year (linear)
  mod.d<-glm(Pres.Abs~Elevation.m + Elevation.m2, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + elev^2
  mod.e<-glm(Pres.Abs~Data.Type+Elevation.m+Elevation.m2, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + elev^2 + year
  mod.f<-glm(Pres.Abs~Data.Type*(Elevation.m + Elevation.m2), data=und.presence.SPEC, family="binomial", na.action=na.fail) #GLOBAL MODEL
  mod.g<-glm(Pres.Abs~1, data=und.presence.SPEC, family="binomial", na.action=na.fail) #null model
  mod.h<-glm(Pres.Abs~Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail) #year only
  
  #A list of all possible models WITH FIRE: (f)
  mod.af<-glm(Pres.Abs~Elevation.m+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + fire
  mod.bf<-glm(Pres.Abs~Elevation.m+Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + year + fire
  mod.cf<-glm(Pres.Abs~Elevation.m*Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev*year + fire
  mod.df<-glm(Pres.Abs~Elevation.m + Elevation.m2+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + elev^2 + fire
  mod.ef<-glm(Pres.Abs~Data.Type+Elevation.m+Elevation.m2+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #elev + elev^2 + year + fire
  mod.ff<-glm(Pres.Abs~Data.Type*(Elevation.m + Elevation.m2)+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #GLOBAL MODEL with fire
  mod.gf<-glm(Pres.Abs~Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #fire only
  mod.hf<-glm(Pres.Abs~Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) # year + fire

  
  
  #### STEP THREE: Sort by deltaAIC, code zeroes for missing coefficients ####
  
  mod.list<-list(mod.a,mod.b,mod.c,mod.d,mod.e,mod.f,mod.g,mod.h,mod.af,mod.bf,mod.cf,mod.df,mod.ef,mod.ff,mod.gf,mod.hf)
  AIC<-c(AIC(mod.a), AIC(mod.b),AIC(mod.c),AIC(mod.d),AIC(mod.e),AIC(mod.f),AIC(mod.g),AIC(mod.h),AIC(mod.af), AIC(mod.bf),AIC(mod.cf),AIC(mod.df),AIC(mod.ef),AIC(mod.ff),AIC(mod.gf),AIC(mod.hf))
  mod.list<-mod.list[order(AIC)]
  mod.top<-list() #reducing to delta < 2
  for(i in 1:16) {
    if (-(mod.list[[1]]$aic-mod.list[[i]]$aic) <=2) mod.top[[i]]<-mod.list[[i]]
    }
  
  ## This part should be greyed out if you are running the loop
  #summary(mod.top[[1]])
  
  for(i in 1:length(mod.top)){
    if(is.na(mod.top[[i]] $coeff["Elevation.m:Data.TypeResurvey"])==FALSE) names(mod.top[[i]]$coeff)[names(mod.top[[i]]$coeff)=="Elevation.m:Data.TypeResurvey"]<-"Data.TypeResurvey:Elevation.m"
    if(is.na(mod.top[[i]] $coeff ["Elevation.m2:Data.TypeResurvey"])==FALSE) names(mod.top[[i]]$coeff)[names(mod.top[[i]]$coeff)=="Elevation.m:Data.TypeResurvey2"]<-"Data.TypeResurvey:Elevation.m2"	
    #setting missing coefficients to 0:
    if(is.na(mod.top[[i]] $coeff ["Data.TypeResurvey"])) mod.top[[i]] $coeff ["Data.TypeResurvey"]<-0
    if(is.na(mod.top[[i]] $coeff ["Elevation.m"])) mod.top[[i]] $coeff ["Elevation.m"]<-0
    if(is.na(mod.top[[i]] $coeff ["Elevation.m2"])) mod.top[[i]] $coeff ["Elevation.m2"]<-0
    if(is.na(mod.top[[i]] $coeff ["FiresAfter 1983"])) mod.top[[i]] $coeff ["FiresAfter 1983"]<-0
    if(is.na(mod.top[[i]] $coeff["FiresBefore 1983"])) mod.top[[i]] $coeff ["FiresBefore 1983"]<-0
    if(is.na(mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m"])) mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m"]<-0
    if(is.na(mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m2"])) mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m2"]<-0}
  
  #### STEP FOUR: Calculate Aikake weights, perform model averaging, store output ####
  
  denominator<-vector()
  for(i in 1:length(mod.top)){
    denominator[i]<-exp(-.5*(-(mod.top[[1]]$aic-mod.top[[i]]$aic)))
    }
  
  sum.denom<-sum(denominator)
  Weight<-vector()
  for(i in 1:length(mod.top)){
    Weight[i]<-exp(-.5*(-(mod.top[[1]]$aic-mod.top[[i]]$aic)))/sum.denom
    }
  r2<-vector()
  for(i in 1:length(mod.top)){
    r2[i]<-1-(logLik(mod.top[[i]])/logLik(mod.g))
    }
  
  Mods.list<-list()
  for(i in 1:length(mod.top)){
    Mods.list[[i]] <- data.frame(Species=levels(species.list)[S],  L.Occ=length(und.presence.SPEC[und.presence.SPEC$Pres.Abs=="1" & und.presence.SPEC$Data.Type=="Legacy","Pres.Abs"]), R.Occ=length(und.presence.SPEC[und.presence.SPEC$Pres.Abs=="1" & und.presence.SPEC$Data.Type=="Resurvey","Pres.Abs"]), Type="Unavg", deltaAIC=(mod.top[[i]]$aic-mod.top[[1]]$aic), Weight=Weight[i], Rsquared=r2[i], Intercept= mod.top[[i]] $coeff ["(Intercept)"] , Data.Type= mod.top[[i]] $coeff ["Data.TypeResurvey"], Elevation.m= mod.top[[i]] $coeff ["Elevation.m"], Elevation.m2= mod.top[[i]] $coeff ["Elevation.m2"] , FiresAfter1983= mod.top[[i]] $coeff ["FiresAfter 1983"] , FiresBefore1983= mod.top[[i]] $coeff ["FiresBefore 1983"], DataType.Elevation.m= mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m"], DataType.Elevation.m2= mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m2"], row.names=NULL) #storing results of top models
    }
  Mods<-ldply(Mods.list, data.frame)
  
  Intercept<-vector()
  for(i in 1:length(mod.top)) Intercept[i]<-mod.top[[i]] $coeff ["(Intercept)"] * Weight[i]
  Intercept <-sum(Intercept)
  Data.Type<-vector()
  for(i in 1:length(mod.top)) Data.Type[i]<-mod.top[[i]] $coeff ["Data.TypeResurvey"] * Weight[i]
  Data.Type<-sum(Data.Type)
  Elevation.m<-vector()
  for(i in 1:length(mod.top)) Elevation.m[i]<-mod.top[[i]] $coeff ["Elevation.m"] * Weight[i]
  Elevation.m <-sum(Elevation.m)
  Elevation.m2<-vector()
  for(i in 1:length(mod.top)) Elevation.m2[i]<-mod.top[[i]] $coeff ["Elevation.m2"] * Weight[i]
  Elevation.m2 <-sum(Elevation.m2)
  FiresAfter1983 <-vector()
  for(i in 1:length(mod.top)) FiresAfter1983[i]<-mod.top[[i]] $coeff ["FiresAfter 1983"] * Weight[i]
  FiresAfter1983 <-sum(FiresAfter1983)
  FiresBefore1983 <-vector()
  for(i in 1:length(mod.top)) FiresBefore1983[i]<-mod.top[[i]] $coeff ["FiresBefore 1983"] * Weight[i]
  FiresBefore1983 <-sum(FiresBefore1983)
  DataType.Elevation.m <-vector()
  for(i in 1:length(mod.top)) DataType.Elevation.m[i]<-mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m"] * Weight[i]
  DataType.Elevation.m <-sum(DataType.Elevation.m)
  DataType.Elevation.m2 <-vector()
  for(i in 1:length(mod.top)) DataType.Elevation.m2[i]<-mod.top[[i]] $coeff ["Data.TypeResurvey:Elevation.m2"] * Weight[i]
  DataType.Elevation.m2 <-sum(DataType.Elevation.m2)
  Avg<-data.frame(Species=levels(species.list)[S],  L.Occ=length(und.presence.SPEC[und.presence.SPEC$Pres.Abs=="1" & und.presence.SPEC$Data.Type=="Legacy","Pres.Abs"]), R.Occ=length(und.presence.SPEC[und.presence.SPEC$Pres.Abs=="1" & und.presence.SPEC$Data.Type=="Resurvey","Pres.Abs"]), Type="Avg", deltaAIC="NA", Weight="NA", Rsquared="NA", Intercept, Data.Type, Elevation.m, Elevation.m2, FiresAfter1983, FiresBefore1983, DataType.Elevation.m, DataType.Elevation.m2) #storing N per survey STORE1
  
  coeff.SPEC[[S]]<-rbind(Mods, Avg)
}
system("say Your loop is done, great work")

coeff<-ldply(coeff.SPEC, data.frame)
write.csv(coeff, file="Pres_Abs_Coeff_Fire_check_Oct20.csv", row.names=FALSE) # for some reason, the linear interaction term keeps getting stored as 0. record coeff and weighted coeff by hand for these species until you have time to correct

# Summarizing
coeff.avg<-coeff[coeff$Type=="Avg",]
length(coeff.avg$Intercept[coeff.avg$Intercept<0])




########### Makin' graphs. Can be adjusted to view burned plots ############

par(mfrow=c(1,1), oma=c(2, 3, 0, 0), mar=c(3,4,1,1)) 

und.presence.SPEC=subset(und.presence, Species.Code=="ACMI")
und.presence.SPEC$Fires<-relevel(und.presence.SPEC$Fires, ref="Unburned")
und.presence.SPEC$Elevation.m2<-und.presence.SPEC$Elevation.m^2
und.presence.SPEC<-und.presence.SPEC[complete.cases(und.presence.SPEC),]

plot(Pres.Abs~Elevation.m, data= und.presence.SPEC, type="n", xaxt="n", yaxt="n", ylab="", xlab="")
axis(side=2, las=1, at=seq(0,1,1), labels=c("Absence", "Presence"), cex.axis=1.5)
axis(side=1, cex.axis=1.2)
mtext(side=1, line=3, "Elevation (m)", cex=1.4)

#Without distinguishing between burned/unburned
und.presence.SPEC.L<-und.presence.SPEC[und.presence.SPEC$Data.Type=="Legacy",]
x.L<-seq(from=min(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), by=0.01)
y.L<-predict(lm.L, list(Elevation.m=x.L, data= und.presence.SPEC.L), type="response")
lines(y.L~x.L, col="Blue")
und.presence.SPEC.R<-und.presence.SPEC[und.presence.SPEC$Data.Type=="Resurvey",]
lm.R<-glm(Pres.Abs~Elevation.m+I(Elevation.m^2), data=und.presence.SPEC.R, family="binomial")
x.R<-seq(from=min(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), by=0.01)
y.R<-predict(lm.R, list(Elevation.m=x.R, data=und.presence.SPEC.R), type="response")
points(max(y.L<-predict(lm.L, list(Elevation.m=x.L, data= und.presence.SPEC.L), type="response")))
points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Legacy",], col="Blue", pch=2)
points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Resurvey",], col="Red", pch=1)
lines(y.R~x.R, col="Red")
legend(300, 0.8, legend=c("1980", "2015"), col=c("Blue", "Red"), pch=c(17,16), cex=1.2, bty = "n", y.intersp = 0.5)

#Distinguishing between burned/unburned
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Legacy" & und.presence.SPEC$Fires=="Unburned",], col="Blue", pch=2)
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Legacy" & und.presence.SPEC$Fires=="Before 1983",], col="Blue", pch=2)
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Legacy" & und.presence.SPEC$Fires=="After 1983",], col="Red", pch=2)
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Resurvey" & und.presence.SPEC$Fires=="Unburned",], col="Blue", pch=1)
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Resurvey" & und.presence.SPEC$Fires=="Before 1983",], col="Blue", pch=1)
#points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="Resurvey" & und.presence.SPEC$Fires=="After 1983",], col="Red", pch=1)
#und.presence.SPEC.L<-und.presence.SPEC[und.presence.SPEC$Data.Type=="Legacy",]
#lm.L<-glm(Pres.Abs~Elevation.m+I(Elevation.m^2), data=und.presence.SPEC.L, family="binomial")
#x.L<-seq(from=min(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), by=1)
#y.L<-predict(lm.L, list(Elevation.m=x.L, data= und.presence.SPEC.L), type="response")
#lines(y.L~x.L, col="Blue", lty=2)
#und.presence.SPEC.R<-und.presence.SPEC[und.presence.SPEC$Data.Type=="Resurvey",]
#lm.R<-glm(Pres.Abs~Elevation.m+I(Elevation.m^2), data=und.presence.SPEC.R, family="binomial")
#x.R<-seq(from=min(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), by=0.1)
#y.R<-predict(lm.R, list(Elevation.m=x.R, data=und.presence.SPEC.R), type="response")
#lines(y.R~x.R, col="Blue")
#legend(300, 0.8, legend=c("", "","Fire < 1983", ""), col=c("White", "White", "Orange", "Red"), pch=c(17,16,1,1), cex=1.2)
#legend(300, 0.8, legend=c("1980", "2015","", "Fire > 1983"), col=c("Blue", "Blue", "Orange", "Red"), pch=c(2,1,2,2), cex=1.2)

#Adding text to display significance of interactions (must be manually modified)
text(1500, 0.8, expression(P[year]<"0.05*"), cex=1.4)
text(1500, 0.7, expression(P[elevation]<"0.01**"), cex=1.4)
text(1500, 0.6, expression(P[elevation^{2}]<"0.01**"), cex=1.4)
text(1500, 0.5, expression(P[yearXelevation]<"0.05*"), cex=1.4)
text(1500, 0.4, expression(P[yearXelevation^{2}]<"0.05*"), cex=1.4)
text(1500, 0.8, expression("P"[data.type]=="NS"), cex=1.4)
text(1500, 0.7, expression("P"[data.typeXelevation]=="NS"), cex=1.4)
text(1500, 0.8, "All NS", cex=1.4)




#### Creating a histogram of frequency of elevational differences for min and max elevation ### OCT 20 to-do: add in 95 percentile

#species.list<-shifts$Species.Code[!shifts$Species.Code=="MARA"]
species.list<-factor(species.list)

focalsp.pres<-und.cover[und.cover$Species %in% species.list,]
focalsp.pres<-focalsp.pres[!focalsp.pres$Plot=="Thor223" & !focalsp.pres$Plot=="8017" & !focalsp.pres$Plot=="4044" & !focalsp.pres$Plot=="Bak494",] 

min.pres<-ddply(focalsp.pres, c("Data.Type", "Species.Code"), summarize, min=min(Elevation.m, na.rm=TRUE))
min.pres2<-cbind(min.pres[min.pres $Data.Type=="Legacy",], min.pres[min.pres $Data.Type=="Resurvey",])
min.pres3<-min.pres2[,c(2, 3, 6)]
names(min.pres3)<-c("Species.Code", "Leg.Min", "Res.Min")
min.pres3$dif<-min.pres3$Res.Min-min.pres3$Leg.Min #min=-376, max=216, mean=-33.23

max.pres<-ddply(focalsp.pres, c("Data.Type", "Species.Code"), summarize, max=max(Elevation.m, na.rm=TRUE))
max.pres2<-cbind(max.pres[max.pres$Data.Type=="Legacy",], max.pres[max.pres$Data.Type=="Resurvey",])
max.pres3<-max.pres2[,c(2, 3, 6)]
names(max.pres3)<-c("Species.Code", "Leg.Max", "Res.Max")
max.pres3$dif<-max.pres3$Res.Max-max.pres3$Leg.Max #min-544, max=669, mean=13

par(mfrow=c(1,2), mar=c(5, 3.9, 0, 0), oma=c(0,1,1,1))
hist(min.pres3$dif, xlab="Lower range edge displacement (m)", ylab = "Number of species", main=NULL, breaks=5, xlim=c(-600, 600), ylim=c(0, 25), cex.axis=1.2, cex.lab=1.2)
abline(v=mean(min.pres3 $dif), col="Red")
text(-500, 23, "a", cex=2, font=3)
text(-150, 23, "~", cex=2)
#legend(100, 20, legend=c("Mean", "CI"), lty=c(1,2), col=c("Red", "Black"), bty="n")
(t.test.min<-t.test(min.pres3 $dif, y=NULL))
abline(v= t.test.min $conf.int, col="Black", lty="dashed")
hist(max.pres3$dif, xlab="Upper range edge displacement (m)", main=NULL, breaks=10, xlim=c(-600, 700), ylim=c(0, 25), ylab="", yaxt="n", cex.axis=1.2, cex.lab=1.2)
abline(v=mean(max.pres3$dif), col="Red")
(t.test.max<-t.test(max.pres3$dif, y=NULL))
abline(v= t.test.max $conf.int, col="Black", lty="dashed")
text(-500, 23, "b", cex=2, font=3)
#text(200, 20, "NS", cex=1.5, font=3)

# this is old code from before - keeping just in case. Needs to be updated
#all.dif<-data.frame(Species.Code=min.pres3$Species.Code, Lower.Dif.temp=min.pres3$dif, Upper.Dif.temp=max.pres3$dif)
#species.all.test<-merge(species.all, all.dif, by="Species.Code", all.x=TRUE)
#species.dif<-species.all.test[,c(27:28)]
#names(species.dif)<-c("Lower.Lim.Dif", "Upper.Lim.Dif")
#species.all$Lower.Lim.Dif<-species.dif$Lower.Lim.Dif

setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/TRY/")
#save(species.all, file="Species.All.Rda")



###### What are the range limits in each year based on model predictions?#####
setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/TRY/")
load("Species.All.Rda")

## BEGINNING OF LOOP 

pred.elev<-list()

for(x in 1:length(species.list)) {
S<-levels(species.list)[x]
und.presence.SPEC=subset(und.presence, Species.Code==S)
und.presence.SPEC$Fires<-relevel(und.presence.SPEC$Fires, ref="Unburned")
und.presence.SPEC$Elevation.m2<-und.presence.SPEC$Elevation.m^2
und.presence.SPEC<-und.presence.SPEC[complete.cases(und.presence.SPEC),]

###### METHOD 2: ADDING FIRE AS A COVARIATE ######
#A list of all possible models WITHOUT FIRE:
mod.a<-glm(Pres.Abs~Elevation.m, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.b<-glm(Pres.Abs~Elevation.m+Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.c<-glm(Pres.Abs~Elevation.m*Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.d<-glm(Pres.Abs~Elevation.m + I(Elevation.m^2), data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.e<-glm(Pres.Abs~Data.Type+Elevation.m+I(Elevation.m^2), data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.f<-glm(Pres.Abs~Data.Type*(Elevation.m + I(Elevation.m^2)), data=und.presence.SPEC, family="binomial", na.action=na.fail) #GLOBAL MODEL
mod.g<-glm(Pres.Abs~1, data=und.presence.SPEC, family="binomial", na.action=na.fail) #null model
mod.h<-glm(Pres.Abs~Data.Type, data=und.presence.SPEC, family="binomial", na.action=na.fail)

#A list of all possible models WITH FIRE:
mod.af<-glm(Pres.Abs~Elevation.m+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.bf<-glm(Pres.Abs~Elevation.m+Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.cf<-glm(Pres.Abs~Elevation.m*Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.df<-glm(Pres.Abs~Elevation.m + I(Elevation.m^2) +Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.ef<-glm(Pres.Abs~Data.Type+Elevation.m+I(Elevation.m^2)+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)
mod.ff<-glm(Pres.Abs~Data.Type*(Elevation.m + I(Elevation.m^2))+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #GLOBAL MODEL with fire
mod.gf<-glm(Pres.Abs~Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail) #just fire
mod.hf<-glm(Pres.Abs~Data.Type+Fires, data=und.presence.SPEC, family="binomial", na.action=na.fail)

#### STEP THREE: Sort by deltaAIC, code zeroes for missing coefficients ####

mod.list<-list(mod.a=mod.a,mod.b= mod.b,mod.c= mod.c,mod.d= mod.d,mod.e= mod.e,mod.f= mod.f,mod.g=mod.g,mod.h= mod.h,mod.af= mod.af,mod.bf= mod.bf,mod.cf= mod.cf,mod.df= mod.df,mod.ef= mod.ef,mod.ff= mod.ff,mod.gf= mod.gf,mod.hf= mod.hf)
AIC<-c(AIC(mod.a), AIC(mod.b),AIC(mod.c),AIC(mod.d),AIC(mod.e),AIC(mod.f),AIC(mod.g),AIC(mod.h),AIC(mod.af), AIC(mod.bf),AIC(mod.cf),AIC(mod.df),AIC(mod.ef),AIC(mod.ff),AIC(mod.gf),AIC(mod.hf)) 
mod.list<-mod.list[order(AIC)] # i literally cannot believe something this dumb works
mod.top<-list() #reducing to delta < 2
for(i in 1:16) {
	if (-(mod.list[[1]]$aic-mod.list[[i]]$aic) <=2) {
		mod.top[[i]]<-mod.list[[i]]
		names(mod.top)[i]<-names(mod.list)[i] }
}
denominator<-vector()
for(i in 1:length(mod.top)){
  denominator[i]<-exp(-.5*(-(mod.top[[1]]$aic-mod.top[[i]]$aic)))
}
sum.denom<-sum(denominator)
Weight<-vector()
for(i in 1:length(mod.top)){
  Weight[i]<-exp(-.5*(-(mod.top[[1]]$aic-mod.top[[i]]$aic)))/sum.denom
}

mod.top.elev<-list()

for(m in 1:length(mod.top)) {
  
  #Adding ifelse statement here for models that don't include elev
  if(is.na(mod.top[[m]] $coeff["Elevation.m"])==TRUE) mod.top.elev[[m]]<-data.frame(Species.Code=S, Peak.Pres.L=Weight[m]*mean(und.presence.SPEC[und.presence.SPEC$Pres.Abs==1,6]), Lower.Lim.L=Weight[m]*min(und.presence.SPEC$Elevation.m), Upper.Lim.L=Weight[m]*max(und.presence.SPEC$Elevation.m), Peak.Pres.R=Weight[m]*mean(und.presence.SPEC[und.presence.SPEC$Pres.Abs==1,6]), Lower.Lim.R=Weight[m]*min(und.presence.SPEC$Elevation.m), Upper.Lim.R=Weight[m]*max(und.presence.SPEC$Elevation.m), Lower.Lim.Dif=0, Upper.Lim.Dif=0, Peak.Pres.Dif=0) else {
newdat.L<-data.frame(Data.Type=factor("Legacy", levels=c("Legacy", "Resurvey")), Elevation.m=seq(from=min(und.presence.SPEC$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC$Elevation.m, na.rm=TRUE), by=0.1), Fires=factor("Unburned", levels=c("Unburned", "After 1983", "Before 1983")))
newdat.R<-data.frame(Data.Type=factor("Resurvey", levels=c("Legacy", "Resurvey")), Elevation.m=seq(from=min(und.presence.SPEC$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC$Elevation.m, na.rm=TRUE), by=0.1), Fires=factor("Unburned", levels=c("Unburned", "After 1983", "Before 1983")))

pred<-predict(mod.top[[m]], newdata=newdat.L, type="response", terms=c("Data.Type", "Elevation.m"))
ugh<-as.data.frame(cbind(Predicted = pred, Elevation.m = newdat.L$Elevation.m))

# At which elevation does probability of presence peak?
(Peak.Pres.L<-ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)]) #Peak abundance

# At which elevations does probabilty of presence become 0.05? Note: for one-sided distributions or smiley-face distributions, only one half will contain all the data.
lower.half<-ugh[ugh$Elevation.m < ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)],]
upper.half<-ugh[ugh$Elevation.m > ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)],]

if(Peak.Pres.L == min(ugh$Elevation.m)) {
	Lower.Lim.L<-Peak.Pres.L
	Upper.Lim.L<-upper.half$Elevation.m[which(abs(upper.half $Predicted - 0.05) == 		min(abs(upper.half $Predicted - 0.05)))] 
	} else {
		if(Peak.Pres.L == max(ugh$Elevation.m)) {
			Lower.Lim.L<-lower.half$Elevation.m[which(abs(lower.half $Predicted - 0.05) 			== min(abs(lower.half $Predicted - 0.05)))]
			Upper.Lim.L<-Peak.Pres.L 
			} else {
				Lower.Lim.L<-lower.half$Elevation.m[which(abs(lower.half 							$Predicted - 0.05) == min(abs(lower.half $Predicted - 0.05)))]
				Upper.Lim.L<-upper.half$Elevation.m[which(abs(upper.half $Predicted - 				0.05) == min(abs(upper.half $Predicted - 0.05)))] }
}

pred<-predict(mod.top[[m]], newdata=newdat.R, type="response", terms=c("Data.Type", "Elevation.m"))
ugh<-as.data.frame(cbind(Predicted = pred, Elevation.m = newdat.L$Elevation.))

# At which elevation does probability of presence peak?
(Peak.Pres.R<-ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)]) #Peak abundance

# At which elevations does probabilty of presence become 0.05? Note: for one-sided distributions or smiley-face distributions, only one half will contain all the data.
lower.half<-ugh[ugh$Elevation.m < ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)],]
upper.half<-ugh[ugh$Elevation.m > ugh$Elevation.m[ugh$Predicted==max(ugh$Predicted)],]

if(Peak.Pres.R == min(ugh$Elevation.m)) {
	Lower.Lim.R<-Peak.Pres.R
	Upper.Lim.R<-upper.half$Elevation.m[which(abs(upper.half $Predicted - 0.05) == 		min(abs(upper.half $Predicted - 0.05)))] 
	} else {
		if(Peak.Pres.R == max(ugh$Elevation.m)) {
			Lower.Lim.R<-lower.half$Elevation.m[which(abs(lower.half $Predicted - 0.05) 			== min(abs(lower.half $Predicted - 0.05)))]
			Upper.Lim.R<-Peak.Pres.R 
			} else {
				Lower.Lim.R<-lower.half$Elevation.m[which(abs(lower.half 							$Predicted - 0.05) == min(abs(lower.half $Predicted - 0.05)))]
				Upper.Lim.R<-upper.half$Elevation.m[which(abs(upper.half $Predicted - 				0.05) == min(abs(upper.half $Predicted - 0.05)))] }
	}
mod.top.elev[[m]]<-data.frame(Species.Code=S, Peak.Pres.L=Weight[m]*Peak.Pres.L, Lower.Lim.L=Weight[m]*Lower.Lim.L, Upper.Lim.L=Weight[m]*Upper.Lim.L, Peak.Pres.R=Weight[m]* Peak.Pres.R, Lower.Lim.R=Weight[m]*Lower.Lim.R, Upper.Lim.R=Weight[m]*Upper.Lim.R, Lower.Lim.Dif=Weight[m]*(Lower.Lim.R-Lower.Lim.L), Upper.Lim.Dif=Weight[m]*(Upper.Lim.R-Upper.Lim.L), Peak.Pres.Dif=Weight[m]*(Peak.Pres.R-Peak.Pres.L))
}
}
pred.elev.df<-ldply(mod.top.elev, data.frame)
pred.elev[[x]]<-aggregate(pred.elev.df[c(2:10)], pred.elev.df["Species.Code"], sum)
}
# problem comes from models that DON'T include elevation. if it doesn't include elevation, just coerce to highest and lowest occurrence
### end of loop

all.pred.elev<-ldply(pred.elev, data.frame)
pred.elev.small<-all.pred.elev[,c(1, 8:10)]
names(pred.elev.small)<-c("Species.Code", "Lower.Dif.Mod", "Upper.Dif.Mod", "Peak.Dif.Mod")
#species.all.test<-merge(species.all, pred.elev.small, by="Species.Code", all.x=TRUE)
#species.mod<-species.all.test[,c(27:29)]
#names(species.mod)<-c("Lower.Dif.Mod", "Upper.Dif.Mod", "Peak.Dif.Mod")
#species.all$Peak.Dif.Mod<-species.mod$Peak.Dif.Mod

setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/TRY/")
#save(species.all, file="Species.All.Rda")

quartz()
par(mfrow=c(1,3), mar=c(5, 1.5, 1, 0), oma=c(0,4,0,0))
hist(species.all$Lower.Dif.Mod, xlab="Lower range edge displacement (m)", main=NULL, breaks=10, xlim=c(-600, 600), ylim=c(0, 40), cex.axis=1.3, cex.lab=1.5)
abline(v=mean(species.all$Lower.Dif.Mod), col="Red")
text(-500, 38, "a", cex=2, font=3)
text(-150, 38, "*", cex=3)
(t.test.min<-t.test(species.all$Lower.Dif.Mod, y=NULL))
abline(v= t.test.min$conf.int, col="Black", lty="dashed")
hist(species.all$Upper.Dif.Mod, xlab="Upper range edge displacement (m)", main=NULL, breaks=10, xlim=c(-600, 700), ylim=c(0, 40), ylab="", yaxt="n", cex.axis=1.3, cex.lab=1.5)
abline(v=mean(species.all$Upper.Dif.Mod), col="Red")
(t.test.max<-t.test(species.all$Upper.Dif.Mod, y=NULL))
abline(v= t.test.max $conf.int, col="Black", lty="dashed")
text(-500, 38, "b", cex=2, font=3)
text(200, 38, "*", cex=3)

hist(species.all$Peak.Dif.Mod, xlab="Peak-presence displacement (m)", main=NULL, breaks=10, xlim=c(-600, 700), ylim=c(0, 40), cex.axis=1.3, cex.lab=1.5, ylab="", yaxt="n")
abline(v=mean(species.all$Peak.Dif.Mod), col="Red")
(t.test.mid<-t.test(species.all$Peak.Dif.Mod, y=NULL))
abline(v= t.test.mid $conf.int, col="Black", lty="dashed")
text(-500, 38, "c", cex=2, font=3)
mtext("Number of species", outer=TRUE, side=2, at=0.6, line=1.7, cex=1.2)




























