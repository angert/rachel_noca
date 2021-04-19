# Created: Apr. 15, 2020
# Updated: Apr. 15, 2021

# This script will be used to create a heatmap of the model-averaged coefficients

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(ggplot2)
library(tidyr)
library(RColorBrewer)

coeff.ALLDAT <- read.csv("data/3b_new_coefficients.csv", header = TRUE)
coeff.fire <- coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "Yes" & coeff.ALLDAT$Type == "Avg",
                               c(1:12, 16:21)]
coeff.nofire <- coeff.ALLDAT[coeff.ALLDAT$Fire.Included == "No" & coeff.ALLDAT$Type == "Avg",
                               c(1:12, 13:15)]
#TODO - should we code NA values as 0? Remember that some species will actually be NA
coeff.avg.fire <- aggregate(coeff.fire[c("Intercept", 
                                             "Elevation.m", 
                                             "Elevation.m2", 
                                             "Resurvey.Burned.fi", 
                                             "Resurvey.Unburned.fi", 
                                             "Elevation.m.Res.Burn.fi", 
                                             "Elevation.m.Res.Unburn.fi",
                                             "Elevation.m2.Res.Burn.fi",
                                             "Elevation.m2.Res.Unburn.fi")], 
                            by = c(coeff.fire["Species"]), mean, na.rm = TRUE)

coeff.avg.nofire <- aggregate(coeff.nofire[c("Intercept", 
                                         "Elevation.m", 
                                         "Elevation.m2", 
                                         "Data.Type.nofi", 
                                         "Data.Type.Elevation.m.nofi", 
                                         "Data.Type.Elevation.m2.nofi")], 
                            by = c(coeff.nofire["Species"]), mean, na.rm = TRUE)

mat <- as.matrix(t(subset(coeff.avg.fire, select = -Species)))
colnames(mat) <- coeff.avg.fire$Species
rownames(mat) <- c("Intercept", "Elevation", "Elevation^2", "Burn After 1983", "Unburned After 1983", "Elevation * Burned", "Elevation * Unburned", "Elevation^2 * Burned", "Elevation^2 * Unburned")

# Fire visualization - try better heatmaps

col.pallette <- brewer.pal(7, "RdBu")
heatmap(mat, 
        Rowv = NA, Colv = NA, scale = "column", col = col.pallette)
legend(x = "left", legend = c(round(min(mat, na.rm = TRUE)), round(max(mat, na.rm = TRUE))), 
       fill = colorRampPalette(col.pallette)(2))


