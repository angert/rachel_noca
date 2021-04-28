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
library(sf)

## Projections
prj.wgs = "+proj=longlat + type=crs"
prj.lcc <- "+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77 +type=crs"


## Plot locations
plots <- read_csv("data/Lat.Long.csv") %>% 
  drop_na() %>% 
  mutate(Longitude=ifelse(Longitude>0, -Longitude, Longitude)) #input file accidentally has some longitudes in E instead of W

## Plot fire history
fire.points <- read_csv("data/All_Plots_Wildfire_Join.csv") %>% 
  mutate(FireHistory = ifelse(CAL_YEAR>=1983, "Burned", "Unburned")) %>% 
  mutate(FireHistory = replace_na(FireHistory, "Unburned"))
           
## Master plot data frame
fire.plots <- left_join(plots, fire.points, by=c("2015.Plot.Name"="Name"))
burned.plots <- fire.plots %>% filter(FireHistory=="Burned")
unburned.plots <- fire.plots %>% filter(FireHistory=="Unburned")

coordinates(burned.plots) <- ~Longitude+Latitude #convert to spatial data
projection(burned.plots) <- CRS('+proj=longlat') #define projection
burned.plots <- spTransform(burned.plots, CRS=CRS(prj.wgs)) #transform projection so points layer matches SDM 
burned.plots.lcc <- spTransform(burned.plots, CRS=CRS(prj.lcc)) #transform projection so points layer matches SDM projections

coordinates(unburned.plots) <- ~Longitude+Latitude #convert to spatial data
projection(unburned.plots) <- CRS('+proj=longlat') #define projection
unburned.plots <- spTransform(unburned.plots, CRS=CRS(prj.wgs))
unburned.plots.lcc <- spTransform(unburned.plots, CRS=CRS(prj.lcc)) #transform projection so points layer matches SDM projections


## State polygons 
# All of USA
sta = readOGR("data/shapefiles/states/gz_2010_us_040_00_500k.shp")
projection(sta) = CRS(prj.wgs)
sta <- st_as_sf(sta) %>% 
  filter(NAME=="Washington")
sta.sp <- as(sta, "Spatial")
sta.lcc <- spTransform(sta.sp, CRS=CRS(prj.lcc))

# Define extent of study area
ext <- extent(min(fire.plots$Longitude)-0.5, max(fire.plots$Longitude)+0.5, min(fire.plots$Latitude)-0.5, max(fire.plots$Latitude)+0.5)
bbox = as(ext, "SpatialPolygons") #convert coordinates to a bounding box
# Crop state lines to study area
sta.crop <- crop(sta, bbox)
sta.lcc = spTransform(sta.crop, CRS=CRS(prj.lcc))

## Park boundary
park <- readOGR("data/shapefiles/park/NOCA_Park_boundary.shp")
park <- spTransform(park, CRS=CRS(prj.wgs))
park.lcc <- spTransform(park, CRS=CRS(prj.lcc))

## Fire polygons
fires <- readOGR("data/shapefiles/fires/NOCA_Wildfire_History.shp")
fires <- spTransform(fires, CRS=CRS(prj.wgs))
fires <- st_as_sf(fires) %>% 
  filter(CAL_YEAR>=1983)
fires.sp <- as(fires, "Spatial")
fires.lcc <- spTransform(fires.sp, CRS=CRS(prj.lcc))

burns <- readOGR("data/shapefiles/fires/Prescribed_burn_history.shp")
burns <- spTransform(burns, CRS=CRS(prj.wgs))
burns <- st_as_sf(burns) %>% 
  filter(CAL_YEAR>=1983)
burns.sp <- as(burns, "Spatial")
burns.lcc <- spTransform(burns.sp, CRS=CRS(prj.lcc))

trtmts <- readOGR("data/shapefiles/fires/Fire_treatment_history.shp")
trtmts <- spTransform(trtmts, CRS=CRS(prj.wgs))
trtmts <- st_as_sf(trtmts) %>% 
  filter(TreatYear>=1983)
trtmts.sp <- as(trtmts, "Spatial")
trtmts.lcc <- spTransform(trtmts.sp, CRS=CRS(prj.lcc))

################################################################################




################################################################################
### Pretty map 

## Set up gridlines & lat/lon labels	
frame.grd = gridlines(sta.crop)
frame.grd.lcc <- spTransform(frame.grd, CRS=CRS(prj.lcc))
gridatt <- gridat(frame.grd, side="EN")
gridat.lcc = spTransform(gridatt, CRS=CRS(prj.lcc))

## Zoomed out of state
#LCC projection
#pdf(file="figures/map_lcc.pdf", width=15, height=8)
plot(sta.lcc, border="darkgrey")
plot(park.lcc, border="black", add=T) # park boundary
plot(fires.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) 
plot(burns.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer 
plot(trtmts.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer
#dev.off()


## Zoomed in of plots
#LCC projection
#pdf(file="figures/map_lcc.pdf", width=15, height=8)
plot(park.lcc, border="darkgrey") # park boundary
plot(fires.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) 
plot(burns.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer 
plot(trtmts.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer
plot(unburned.plots.lcc, pch=4, col="black", add=T) #add plots that didn't burn between surveys
plot(burned.plots.lcc, pch=1, col="black", add=T) #add plots that burned between surveys
#plot(sta.lcc, add=T) #add state lines
plot(frame.grd.lcc, add=TRUE, lty="dashed", col="grey", lwd=1) #add gridlines
text(coordinates(gridat.lcc), labels=parse(text=as.character(gridat.lcc$labels)), pos=gridat.lcc$pos, offset=0.5, col="black", cex=0.7) #add lat-long labels to gridlines
#legend("bottomleft", legend="Weighted Ensemble", bty="n", cex=1.5) #add title
#plot(wtd.ensem.lcc, legend.only=TRUE, legend.width=1, legend.shrink=0.75, col=rbPal, axis.args=list(at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), cex.axis=0.8)) #add legend for color ramp
#dev.off()


#unprojected
#pdf(file="figures/map_lcc.pdf", width=15, height=8)
plot(park, border="grey") # park boundary
#plot(sta.crop) #add state lines
plot(fires["STATE"], col=rgb(1,0,0,0.7), border="red4", add=T) 
plot(burns["AGENCY"], col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer 
plot(trtmts["Shape_Leng"], col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer
plot(unburned.plots, pch=4, col="black", add=T) #add plots that didn't burn between surveys
plot(burned.plots, pch=1, col="black", add=T) #add plots that burned between surveys
plot(frame.grd, add=TRUE, lty="dashed", col="darkgrey", lwd=1) #add gridlines
text(coordinates(gridat), labels=parse(text=as.character(gridat.lcc$labels)), pos=gridat.lcc$pos, offset=0.5, col="black", cex=0.7) #add lat-long labels to gridlines
#legend("bottomleft", legend="Weighted Ensemble", bty="n", cex=1.5) #add title
#plot(wtd.ensem.lcc, legend.only=TRUE, legend.width=1, legend.shrink=0.75, col=rbPal, axis.args=list(at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), cex.axis=0.8)) #add legend for color ramp
#dev.off()

################################################################################
