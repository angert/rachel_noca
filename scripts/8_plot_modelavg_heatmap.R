# Created: Apr. 15, 2020
# Updated: Apr. 15, 2021

# This script will be used to create a heatmap of the model-averaged coefficients

# IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections.

# Packages needed:

library(tidyr)
library(RColorBrewer)
library(pheatmap)

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
mat.percfire.sm[is.na(mat.percfire.sm)] <- 0
mat.percfire.sm[c("Elevation.m2.Res.Burn.fi", "Elevation.m2.Res.Unburn.fi"), grepl("VAME", colnames(mat.percfire.sm))] <- NA

# If you need the order reversed
mat.percfire.rev <- mat.percfire.sm[nrow(mat.percfire.sm):1,]

# Setting visualization parameters

col.pallette <- rev(brewer.pal(9, "RdBu"))
col.vector <- c("      ACMI", "", "      ARUV", "", 
                "      CARU", "", "      CEVE", "", 
                "      EPAN", "", "      PAMY", "", "      VAME", "")
row.vector <- c("Elevation", 
              expression("Elevation" ^ 2), 
              "Burned",
              "Unburned",
              "Elevation * Burned",
              "Elevation * Unburned",
              expression("Elevation" ^ 2 * " * Burned"), 
              expression("Elevation" ^ 2 * " * Unburned")
              )
leg.label.breaks <- c(-1, -0.5, 0, 0.5, 1)
leg.label.vector <- c("100% Negative", "50% Negative", "0%", "50% Positive", "100% Positive")

col.breaks <- c(2, 4, 6, 8, 10, 12, 14)

# Visualization with pheatmap()

col.pallette <- rev(brewer.pal(9, "RdBu"))
pheatmap(mat.percfire.sm,
         color = col.pallette,
         cellwidth = 30,
         cellheight = 30,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         legend = TRUE,
         legend_breaks = leg.label.breaks,
         legend_labels = leg.label.vector, #doesn't work
         # display_numbers = TRUE,
         number_color = "black",
         na_col = "grey",
         gaps_col = col.breaks,
         labels_row = row.vector,
         labels_col = col.vector,
         fontsize_row = 12,
         fontsize_col = 15,
         angle_col = 0
         )






# Visualization with heatmap() - base R

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



