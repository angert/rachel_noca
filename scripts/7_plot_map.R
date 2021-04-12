#################################################################################
### SCRIPT PURPOSE: plot map of NOCA plots with fire history overlain
# Modified from Angert et al. 2018, American Naturalist
# Author: Amy Angert
# last update:  12 Apr 2021

##################################################################################### LOAD LIBRARIES AND PREPARE INPUTS

## Clear workspace
rm(list = ls(all.names = TRUE))

## Libraries needed for spatial stuff (others for predict functions called as needed below)
library(tidyverse)
library(raster)
library(maptools)
library(rgdal)
library(rgeos)

## Projections
prj.wgs = "+proj=longlat + type=crs"
prj.lcc <- "+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77 +type=crs"


## Plot locations
plots <- read_csv("data/Lat.Long.csv") %>% 
  drop_na() %>% 
  mutate(Longitude=ifelse(Longitude>0, -Longitude, Longitude)) #input file accidentally has some longitudes in E instead of W
coordinates(plots) <- ~Longitude+Latitude #convert to spatial data
projection(plots) <- CRS('+proj=longlat') #define projection
plots.lcc <- spTransform(plots, CRS=CRS(prj.lcc)) #transform projection so points layer matches SDM projections


## State polygons 
# All of USA
sta = readOGR("data/shapefiles/states/gz_2010_us_040_00_500k.shp")
projection(sta) = CRS(prj.wgs)
# Define extent of study area
ext <- extent(min(plots$Longitude)-0.5, max(plots$Longitude)+0.5, min(plots$Latitude)-0.5, max(plots$Latitude)+0.5)
bbox = as(ext, "SpatialPolygons") #convert coordinates to a bounding box
# Crop state lines to study area
sta.crop <- crop(sta, bbox)
sta.lcc = spTransform(sta.crop, CRS=CRS(prj.lcc))

## Park boundary
park <- readOGR("data/shapefiles/NOCA_Park_boundary.shp")

################################################################################




################################################################################
### Pretty map of quantitative ensemble

## Set up gridlines & lat/lon labels	
frame.grd = gridlines(sta.crop)
frame.grd.lcc <- spTransform(frame.grd, CRS=CRS(prj.lcc))
gridatt <- gridat(frame.grd, side="EN")
gridat.lcc = spTransform(gridatt, CRS=CRS(prj.lcc))

## Set up color ramp 
library(colorspace)
rbPal <- diverge_hcl(10)

## Save plot
#LCC projection
pdf(file="figures/map_lcc.pdf", width=15, height=8)
plot(plots.lcc, pch=1, cex=0.8) #add presence points
plot(sta.lcc, add=T) #add state lines
plot(frame.grd.lcc, add=TRUE, lty="dashed", col="darkgrey", lwd=1) #add gridlines
text(coordinates(gridat.lcc), labels=parse(text=as.character(gridat.lcc$labels)), pos=gridat.lcc$pos, offset=0.5, col="black", cex=0.7) #add lat-long labels to gridlines
#legend("bottomleft", legend="Weighted Ensemble", bty="n", cex=1.5) #add title
#plot(wtd.ensem.lcc, legend.only=TRUE, legend.width=1, legend.shrink=0.75, col=rbPal, axis.args=list(at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), cex.axis=0.8)) #add legend for color ramp
dev.off()

#unprojected
pdf(file="figures/map_unprj.pdf", width=15, height=8)
plot(plots, pch=1, cex=0.8) #add presence points
plot(sta, add=T) #add state lines
plot(frame.grd, add=TRUE, lty="dashed", col="darkgrey", lwd=1) #add gridlines
text(coordinates(gridat), labels=parse(text=as.character(gridat$labels)), pos=gridat$pos, offset=0.5, col="black", cex=0.7) #add lat-long labels to gridlines
#legend("bottomleft", legend="Weighted Ensemble", bty="n", cex=1.5) #add title
#plot(wtd.ensem.lcc, legend.only=TRUE, legend.width=1, legend.shrink=0.75, col=rbPal, axis.args=list(at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), cex.axis=0.8)) #add legend for color ramp
dev.off()


################################################################################
