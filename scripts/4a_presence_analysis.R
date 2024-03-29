# Created: Dec. 11, 2020
# Updated: Mar. 22, 2021

# This script will be used to summarize results of the PRESENCE analyses - coefficients

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:
library(dplyr)

# Functions needed:

# Turning averaged coefficients into +/-/0
simplify.fun <- function(varib) {
  coeff.summary.SPEC <- coeff.summary.empty[varib]
  dataset.vect <- as.vector(coeff.SPEC.avg$Dataset)
  for(d in dataset.vect) {
    if(coeff.SPEC.avg[coeff.SPEC.avg$Dataset == d, varib] > 0) {
          coeff.summary.SPEC[d, varib] <- paste("+")
          }
    if(coeff.SPEC.avg[coeff.SPEC.avg$Dataset == d, varib] == 0) {
          coeff.summary.SPEC[d, varib] <- paste("0")
          }
    if(coeff.SPEC.avg[coeff.SPEC.avg$Dataset == d, varib] < 0) {
          coeff.summary.SPEC[d, varib] <- paste("-")
    }
  }
  return(coeff.summary.SPEC)
}

# Summing numbers of +/-/0 for each coeff for one species
# Empty data frame
coeff.count.empty <- data.frame(Elevation.m = rep(NA, times = 4), 
                                Elevation.m2 = rep(NA, times = 4), 
                                Resurvey.Burned.fi = rep(NA, times = 4), 
                                Resurvey.Unburned.fi = rep(NA, times = 4), 
                                Elevation.m.Res.Burn.fi = rep(NA, times = 4), 
                                Elevation.m.Res.Unburn.fi = rep(NA, times = 4), 
                                Elevation.m2.Res.Burn.fi = rep(NA, times = 4), 
                                Elevation.m2.Res.Unburn.fi = rep(NA, times = 4), 
                                Data.Type.nofi = rep(NA, times = 4), 
                                Data.Type.Elevation.m.nofi = rep(NA, times = 4), 
                                Data.Type.Elevation.m2.nofi = rep(NA, times = 4),
                                row.names = c("+", "-", 0, "Ignore"))

# Collapsing 100 rows of +/-/0 into summary
count.fun <- function(varib) {
  df.count <- coeff.count.empty[varib]
  if(is.na(table(simple.coeffs.SPEC[varib])["+"]) == FALSE) {
    df.count["+", varib] <- table(simple.coeffs.SPEC[varib])["+"]
  } 
  if(is.na(table(simple.coeffs.SPEC[varib])["-"]) == FALSE) {
    df.count["-", varib] <- table(simple.coeffs.SPEC[varib])["-"]
  } 
  if(is.na(table(simple.coeffs.SPEC[varib])["0"]) == FALSE) {
    df.count["0", varib] <- table(simple.coeffs.SPEC[varib])["0"]
  } 
  if(is.na(table(simple.coeffs.SPEC[varib])["Ignore"]) == FALSE) {
    df.count["Ignore", varib] <- table(simple.coeffs.SPEC[varib])["Ignore"]
  } 
  return(df.count)
}


#### STEP 1: Import data ####

warn.ALLDAT <- read.csv("data/3_presence_ALLDAT_ALLSPEC_warnings.csv", header = TRUE)

# Can use either top_mod input or new_coefficients input
coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)
coeff.ALLDAT[is.na(coeff.ALLDAT)] <- paste(0)
load("data/Species.List.Rda") #TODO this file was made in an undocumented step
species.list <- shifts$Species.Code[!shifts$Species.Code=="MOSS" &
                                      !shifts$Species.Code=="COST" &
                                      !shifts$Species.Code=="LUPE" &
                                      !shifts$Species.Code=="PHEM" &
                                      !shifts$Species.Code=="RHAL" &
                                      !shifts$Species.Code=="VAAL" &
                                      !shifts$Species.Code=="VADE"] #removing problematic species
species.list <- factor(species.list)

#### STEP 2 (OPTIONAL): Exploratory visualizations of warnings ####

(numbered.species <- data.frame(Species=species.list, No.=rep(1:length(species.list))))

# Choose species of interest
warn.SPEC <- warn.ALLDAT[warn.ALLDAT$Species == "VAME", ]

# Assess  warnings
table(warn.SPEC$Has_warning, warn.SPEC$Dataset)
table(table(warn.SPEC$Has_warning, warn.SPEC$Dataset))
head(warn.SPEC[warn.SPEC$Has_warning == TRUE, ])

# (Optional): How did rarefaction vary between species? #
#par(mar=c(5, 1.5, 1, 0), oma=c(0,4,0,0)) # 3-paneled graph
#hist(coeff.SPEC$R.Occ, main = "RHAL", xlab = "No. resurvey occ", ylab = "Frequency")


#### STEP 3: Exploring coefficients ####

coeff.count.LIST <- list()
coeff.perc.LIST <- list()

for(S in 1:nrow(numbered.species)) {
  #coeff.SPEC.avg <- coeff.ALLDAT[coeff.ALLDAT$Species == levels(numbered.species$Species)[S], ]
  coeff.SPEC.avg <- coeff.ALLDAT[coeff.ALLDAT$Species == levels(numbered.species$Species)[S] & 
                                   coeff.ALLDAT$Type == "Avg", ]
  
  coeff.summary.empty <- data.frame(Elevation.m = rep(NA, times = 100), 
                                    Elevation.m2 = rep(NA, times = 100), 
                                    Resurvey.Burned.fi = rep(NA, times = 100), 
                                    Resurvey.Unburned.fi = rep(NA, times = 100), 
                                    Elevation.m.Res.Burn.fi = rep(NA, times = 100), 
                                    Elevation.m.Res.Unburn.fi = rep(NA, times = 100), 
                                    Elevation.m2.Res.Burn.fi = rep(NA, times = 100), 
                                    Elevation.m2.Res.Unburn.fi = rep(NA, times = 100), 
                                    Data.Type.nofi = rep(NA, times = 100), 
                                    Data.Type.Elevation.m.nofi = rep(NA, times = 100), 
                                    Data.Type.Elevation.m2.nofi = rep(NA, times = 100))
  
  simple.coeffs.SPEC <- bind_cols(lapply(names(coeff.summary.empty), simplify.fun))
  simple.coeffs.SPEC[is.na(simple.coeffs.SPEC)] <- paste("Ignore")
  
  # Summarizing the summary
  
  coeff.count.SPEC <- bind_cols((lapply(names(coeff.count.empty), count.fun)))
  coeff.count.SPEC$Species <- levels(factor(coeff.SPEC.avg$Species))
  if(length(levels(factor(coeff.SPEC.avg$Fire.Included))) == 1) {
    coeff.count.SPEC$Fire.Included <- levels(factor(coeff.SPEC.avg$Fire.Included))
  } else(coeff.count.SPEC$Fire.Included <- "Sometimes")
  coeff.count.SPEC$Effect <- row.names(coeff.count.SPEC)
  coeff.count.LIST[[S]] <- coeff.count.SPEC
  
  # Converting to % of good datasets
  good.sets.SPEC <- 
    sum(coeff.count.SPEC$Elevation.m[!coeff.count.SPEC$Effect == "Ignore"], na.rm = TRUE)
  coeff.perc.SPEC.A <- data.frame(Species = coeff.count.SPEC$Species,
                                Fire.Included = coeff.count.SPEC$Fire.Included,
                                Total.Sets = rep(good.sets.SPEC, times = nrow(coeff.count.SPEC)),
                                Effect = coeff.count.SPEC$Effect, 
                                Elevation.m = coeff.count.SPEC$Elevation.m / good.sets.SPEC,
                                Elevation.m2 = coeff.count.SPEC$Elevation.m2 / good.sets.SPEC, 
                                Resurvey.Burned.fi = 
                                  coeff.count.SPEC$Resurvey.Burned.fi / good.sets.SPEC, 
                                Resurvey.Unburned.fi = 
                                  coeff.count.SPEC$Resurvey.Unburned.fi / good.sets.SPEC, 
                                Elevation.m.Res.Burn.fi = 
                                  coeff.count.SPEC$Elevation.m.Res.Burn.fi / good.sets.SPEC, 
                                Elevation.m.Res.Unburn.fi = 
                                  coeff.count.SPEC$Elevation.m.Res.Unburn.fi / good.sets.SPEC, 
                                Elevation.m2.Res.Burn.fi = 
                                  coeff.count.SPEC$Elevation.m2.Res.Burn.fi / good.sets.SPEC, 
                                Elevation.m2.Res.Unburn.fi = 
                                  coeff.count.SPEC$Elevation.m2.Res.Unburn.fi / good.sets.SPEC, 
                                Data.Type.nofi =
                                  coeff.count.SPEC$Data.Type.nofi / good.sets.SPEC, 
                                Data.Type.Elevation.m.nofi =
                                  coeff.count.SPEC$Data.Type.Elevation.m.nofi / good.sets.SPEC,  
                                Data.Type.Elevation.m2.nofi = 
                                  coeff.count.SPEC$Data.Type.Elevation.m2.nofi / good.sets.SPEC)
  coeff.perc.SPEC.B <- coeff.perc.SPEC.A[!coeff.perc.SPEC.A$Effect == "Ignore", ]
  coeff.perc.LIST[[S]] <- coeff.perc.SPEC.B
  
}

coeff.count <- ldply(coeff.count.LIST, data.frame)
coeff.perc <- ldply(coeff.perc.LIST, data.frame)

write.csv(coeff.count, 
          file = "data/4_presence_coefficients_count.csv", 
          row.names = FALSE)

# Separate into fire / no fire (currently for perc only)

coeff.perc.fire <- coeff.perc[coeff.perc$Fire.Included == "Yes", 1:12]
coeff.perc.nofire <- coeff.perc[coeff.perc$Fire.Included == "No", c(1:6, 13:15)]

write.csv(coeff.perc.fire, 
          file = "data/4_pres_coefficients_percent_fire.csv", row.names = FALSE)
write.csv(coeff.perc.nofire, 
          file = "data/4_pres_coefficients_percent_NOfire.csv", row.names = FALSE)


#### Step 4: Visualizing summary output ####
#TODO fix this nonsense

# Split into yes/no fire, remove row specifying term wasn't included

rowsum(as.data.frame(sapply(coeff.count[, c("Elevation.m", 
                                            "Elevation.m2",
                                             "Resurvey.Burned.fi",
                                             "Resurvey.Unburned.fi",
                                             "Elevation.m.Res.Burn.fi",
                                             "Elevation.m.Res.Unburn.fi",
                                             "Elevation.m2.Res.Burn.fi",
                                             "Elevation.m2.Res.Unburn.fi")], 
                         as.numeric)), group = coeff.count$Species, na.rm = TRUE)

coeff.nozero.fi <- coeff.count[coeff.count$Fire.Included == "Yes" & !coeff.count$Effect == 0, ]
coeff.nozero.nofi <- coeff.count[coeff.count$Fire.Included == "No" & !coeff.count$Effect == 0, ]
coeff.nozero.fi[is.na(coeff.nozero.fi)] <- paste(0)
coeff.nozero.fi[is.na(coeff.nozero.fi)] <- paste(0)

# Summing across +/-
coeff.sum.fi <- rowsum(as.data.frame(sapply(coeff.nozero.fi[, c("Elevation.m", 
                                                    "Elevation.m2",
                                                    "Resurvey.Burned.fi",
                                                    "Resurvey.Unburned.fi",
                                                    "Elevation.m.Res.Burn.fi",
                                                    "Elevation.m.Res.Unburn.fi",
                                                    "Elevation.m2.Res.Burn.fi",
                                                    "Elevation.m2.Res.Unburn.fi")], 
                              as.numeric)), group = coeff.nozero.fi$Species)
names(coeff.sum.fi) <- paste("Inc", colnames(coeff.sum.fi), sep = ".")
coeff.sum.fi$Species <- row.names(coeff.sum.fi)
coeff.sum.nofi <- rowsum(as.data.frame(sapply(coeff.nozero.nofi[, c("Elevation.m", 
                                                                "Elevation.m2",
                                                                "Data.Type.nofi",
                                                                "Data.Type.Elevation.m.nofi",
                                                                "Data.Type.Elevation.m2.nofi")], 
                                            as.numeric)), group = coeff.nozero.nofi$Species)
names(coeff.sum.nofi) <- paste("Inc", colnames(coeff.sum.nofi), sep = ".")
coeff.sum.nofi$Species <- row.names(coeff.sum.nofi)

# Note: I got very carried away here

# Joining to # of times coeff was +
coeff.pos.fi <- coeff.nozero.fi[coeff.nozero.fi$Effect == "+", c(1:8)]
names(coeff.pos.fi) <- paste("Pos", colnames(coeff.pos.fi), sep = ".")
coeff.pos.nofi <- coeff.nozero.nofi[coeff.nozero.nofi$Effect == "+", c(1:2, 9:11)]
names(coeff.pos.nofi) <- paste("Pos", colnames(coeff.pos.nofi), sep = ".")

coeff.all.fi <- cbind(coeff.sum.fi, coeff.pos.fi)
coeff.all.nofi <- cbind(coeff.sum.nofi, coeff.pos.nofi)


plot(Inc.Resurvey.Burned.fi ~ Pos.Resurvey.Burned.fi, 
     data = coeff.all.fi)
text(abs_losses, percent_losses, labels=namebank, cex= 0.7)




test <- lm(Inc.Resurvey.Burned.fi ~ Pos.Resurvey.Burned.fi, data = coeff.all.fi)
summary(test)



