#################################################################################
### SCRIPT PURPOSE: plot map of NOCA plots with fire history overlain
# Modified from Angert et al. 2018, American Naturalist
# Author: Amy Angert
# last update:  12 Apr 2021

##################################################################################### LOAD LIBRARIES AND PREPARE INPUTS

## Clear workspace
rm(list = ls(all.names = TRUE))

## Libraries needed for spatial stuff 
library(tidyverse)
library(raster)
library(maptools)
library(rgdal)
library(rgeos)
library(sf)
library(maps)
library(mapdata)
library(mapproj)
library(ggstar)

## Projections
prj.wgs <- "+proj=longlat + type=crs"
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
burned.plots <- spTransform(burned.plots, CRS=CRS(prj.wgs)) #transform projection 
burned.plots.lcc <- spTransform(burned.plots, CRS=CRS(prj.lcc)) #transform projection 

coordinates(unburned.plots) <- ~Longitude+Latitude #convert to spatial data
projection(unburned.plots) <- CRS('+proj=longlat') #define projection
unburned.plots <- spTransform(unburned.plots, CRS=CRS(prj.wgs))
unburned.plots.lcc <- spTransform(unburned.plots, CRS=CRS(prj.lcc)) #transform projection 


## State polygons 
# WA only
sta <- readOGR("data/shapefiles/states/gz_2010_us_040_00_500k.shp")
projection(sta) <- CRS(prj.wgs)
sta <- st_as_sf(sta) %>% 
  filter(NAME=="Washington")
sta.sp <- as(sta, "Spatial")
sta.lcc <- spTransform(sta.sp, CRS=CRS(prj.lcc))

# USA outline
usa <- map_data("usa")

# Define extent of study area
ext <- extent(min(fire.plots$Longitude)-0.5, max(fire.plots$Longitude)+0.5, min(fire.plots$Latitude)-0.5, max(fire.plots$Latitude)+0.5)
bbox <- as(ext, "SpatialPolygons") #convert coordinates to a bounding box
# Crop state lines to study area
sta.crop <- crop(sta.sp, bbox)
sta.lcc <- spTransform(sta.crop, CRS=CRS(prj.lcc))

## Elevation shading
dem.raster <- getData("SRTM", lat = mean(fire.plots$Latitude), lon = mean(fire.plots$Longitude), download = TRUE)

dem.raster <- crop(dem.raster, as(my_bbox_buff_25000.sf, 'Spatial'), snap='out')

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
frame.grd <- gridlines(sta.crop)
frame.grd.lcc <- spTransform(frame.grd, CRS=CRS(prj.lcc))
gridatt <- gridat(frame.grd, side="EN")
gridat.lcc <- spTransform(gridatt, CRS=CRS(prj.lcc))


## Zoomed out inset (world)
dot.plot <- data.frame(mean.lat=mean(plots$Latitude), mean.long=mean(plots$Longitude))

map_world <- borders("world", colour="black", fill="grey")

ggplot(dot.plot, aes(x = mean.long, y = mean.lat)) +
  map_world +
  geom_star(size=5, pch=21, fill="black") +
  #scale_colour_manual(values=siteColors) +
  coord_map(projection="ortho", orientation=c(48,-121,0)) +
  #scale_shape_manual(values = c(4, 24, 25)) +
  labs(x = "Longitude") +
  labs(y = "Latitude") +
  ggtitle("B") +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +
  theme_classic()
  #legend("topleft", legend="B", bty="n") 
  #theme(panel.background=element_rect(fill="#c7eae5"))
ggsave("figures/map_world_inset.png", width=8, height=5)

## Zoomed in of plots
#LCC projection
pdf(file="figures/map_fire_plots.pdf", width=10, height=8)
plot(park.lcc, border="darkgrey") # park boundary
plot(fires.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) 
plot(burns.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer 
plot(trtmts.lcc, col=rgb(1,0,0,0.7), border="red4", add=T) #prescribed burns layer
plot(unburned.plots.lcc, pch=1, col="black", add=T) #add plots that didn't burn between surveys
plot(burned.plots.lcc, pch=4, col="black", cex=2, add=T) #add plots that burned between surveys
#plot(sta.lcc, add=T) #add state lines
plot(frame.grd.lcc, add=TRUE, lty="dashed", col="grey", lwd=1) #add gridlines
#text(coordinates(gridat.lcc), labels=parse(text=as.character(gridat.lcc$labels)), pos=gridat.lcc$pos, offset=0.5, col="black", cex=0.7) #add lat-long labels to gridlines
#legend("topleft", legend=c("A", "unburned","burned","fire"), pch=c(4,1,4,22), col=c("white","black","black","red4"), pt.bg=c("white","white","white",rgb(1,0,0,0.7)), bg="white", box.col="white") #add title
dev.off()
 

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
