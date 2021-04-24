# Created: Apr. 15, 2020
# Updated: Apr. 15, 2021

# This script will be used to create a heatmap of the model-averaged coefficients

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(tidyr)
library(RColorBrewer)

# Loading and tidying average coefficient data

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

# Loading and tidying vote count data

perc.fire <- read.csv("data/4_pres_coefficients_percent_fire.csv", header = TRUE)
perc.nofire <- read.csv("data/4_pres_coefficients_percent_NOfire.csv", header = TRUE)

mat.percfire.large <- t(subset(perc.fire, select = -c(Species, Fire.Included, Total.Sets, Effect)))
colnames(mat.percfire.large) <- paste(perc.fire$Species, perc.fire$Effect)
mat.percfire.sm <- mat.percfire.large[, !grepl(0, colnames(mat.percfire.large))]
mat.percfire.sm[, grepl("-", colnames(mat.percfire.sm))] <- mat.percfire.sm[, grepl("-", colnames(mat.percfire.sm))] * -1
mat.percfire.PLOT <- mat.percfire.sm[nrow(mat.percfire.sm):1,]

# Fire visualization - try better heatmaps

col.pallette <- rev(brewer.pal(9, "RdBu"))
y.vector <- c(expression("Elevation" ^ 2 * " * Unburned ca. 1983"), 
              expression("Elevation" ^ 2 * " * Burned ca. 1983"), 
              "Elevation * Unburned ca. 1983",
              "Elevation * Burned ca. 1983",
              "Unburned ca. 1983",
              "Burned ca. 1983",
              expression("Elevation" ^ 2), 
              "Elevation")
x.vector <- c("ACMI", "ARUV", "CARU", "CEVE", "EPAN", "PAMY", "VAME")

heatmap(mat.percfire.PLOT, 
        Rowv = NA, Colv = NA, scale = "none", col = col.pallette,
        labRow = y.vector, labCol = "")
legend(x = 0, y = 2, legend = c("+ 100%", "", "+ 50%", "", "0%", "", "- 50%", "", "- 100%"), 
       fill = colorRampPalette(brewer.pal(9, "RdBu"))(9), 
       border = "white", bty = "n", pt.cex = 3, y.intersp= 0.5)


pos2 <- structure(list(x = c(0.17, 0.635), 
                       y = c(-1.4, -1.4)),
                  .Names = c("x", "y"))
text(x = seq(pos2$x[1], pos2$x[2], len=7), y=rep(pos2$y[1], 7),
     xpd=TRUE, adj = 0,
     labels = x.vector, cex = 1.2)



