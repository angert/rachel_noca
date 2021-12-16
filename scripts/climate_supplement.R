library(tidyverse)
library(patchwork)

### read in climate files from ClimateWNA
seasonal <- read_csv("data/All_Plots_RNW_1970-2015ST.csv")
annual <- read_csv("data/All_Plots_RNW_1970-2015YT.csv")

### merge elevation and turn it to numeric
plot.names <- load("~/data/plot.names.Rda") #why isn't this loading??
# loading it manually by double-clicking from finder window

seasonal <- left_join(seasonal, plot.names, by=c("ID1" = "Plot.2015"))
annual <- left_join(annual, plot.names, by=c("ID1" = "Plot.2015"))

seasonal$Elevation.m <- as.numeric(seasonal$Elevation.m)
annual$Elevation.m <- as.numeric(annual$Elevation.m)

### filter climate data to random subset of plots
plots <- sample(seasonal$ID1, size=10, replace=FALSE)
seasonal.sub <- seasonal %>% filter(ID1 %in% plots)
annual.sub <- annual %>% filter(ID1 %in% plots)

### filter climate data to select plots that capture the breadth of elevations in the data
# 5 equal intervals for elevation
elev.vec <- seq(min(annual$Elevation.m, na.rm=T), max(annual$Elevation.m, na.rm=T), length.out=5)
# which plots match?

annual$ID1[which(abs(annual$Elevation.m-elev.vec[1])==min(abs(annual$Elevation.m-elev.vec[1]), na.rm=T))] #Bac208
annual$ID1[which(abs(annual$Elevation.m-elev.vec[2])==min(abs(annual$Elevation.m-elev.vec[2]), na.rm=T))] #Ross3020
annual$ID1[which(abs(annual$Elevation.m-elev.vec[3])==min(abs(annual$Elevation.m-elev.vec[3]), na.rm=T))] #Sour4003
annual$ID1[which(abs(annual$Elevation.m-elev.vec[4])==min(abs(annual$Elevation.m-elev.vec[4]), na.rm=T))] #Cari280
annual$ID1[which(abs(annual$Elevation.m-elev.vec[5])==min(abs(annual$Elevation.m-elev.vec[5]), na.rm=T))] #Ste2033

plots=c("Bac208","Ross3020","Sour4003","Cari280","Ste2033")
seasonal.sub <- seasonal %>% filter(ID1 %in% plots)
annual.sub <- annual %>% filter(ID1 %in% plots)

### filter climate data to select plots that capture the max range of differences in temperature among plots
dat2015 <- annual %>% filter(Year==2015)
temp.vec <- seq(min(dat2015$MAT, na.rm=T), max(dat2015$MAT, na.rm=T), length.out=5)
# which plots match?
dat2015$ID1[which(abs(dat2015$MAT-temp.vec[1])==min(abs(dat2015$MAT-temp.vec[1]), na.rm=T))] #Cas5109, Ste2019, Ste2023, Ste2021
dat2015$ID1[which(abs(dat2015$MAT-temp.vec[2])==min(abs(dat2015$MAT-temp.vec[2]), na.rm=T))] #What6021, What6043, What6005, Ste2031, Ste2032, RaRi5021
dat2015$ID1[which(abs(dat2015$MAT-temp.vec[3])==min(abs(dat2015$MAT-temp.vec[3]), na.rm=T))]#"HB5158" "STE2024" "Copp6022" "Copp6028" "Copp6024" "Copp6030" "Copp6026" "Ste2078"  "Ste2062"  "Thun7120" "Thun7119" "Thun7118" "Ross3022" "Ross3024" "Ross3018" "Ste2008"  "Ruby1057" "Ruby1059" "Pan1018"  "Pan1021"  "Sour4023" "Sour4033" "Sour4021" "Cari449" "Hozo140"
dat2015$ID1[which(abs(dat2015$MAT-temp.vec[4])==min(abs(dat2015$MAT-temp.vec[4]), na.rm=T))] #"ROSS4001" "STE2042" "Pyra1019" "Ross3008" "Ross3031" "Ross3010" "Thor217"
dat2015$ID1[which(abs(dat2015$MAT-temp.vec[5])==min(abs(dat2015$MAT-temp.vec[5]), na.rm=T))] #Bac208

plots=c("Cas5109","What6021","HB5158","ROSS4001","Bac208")
seasonal.sub <- seasonal %>% filter(ID1 %in% plots)
annual.sub <- annual %>% filter(ID1 %in% plots)


### pull out survey-year data alone
seasonal.survey <- seasonal.sub %>% filter(Year==1983|Year==2015)
annual.survey <- annual.sub %>% filter(Year==1983|Year==2015)

MAT <- ggplot(annual.sub, aes(x=Year, y=MAT, by=ID1)) +
  geom_line(aes(x=Year, y=MAT, color=Elevation.m)) +
  geom_point(aes(x=Year, y=MAT, color=Elevation.m), data=annual.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Mean annual temperature (\u00B0C)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("A")

Twin <- ggplot(seasonal.sub, aes(x=Year, y=Tmax_wt, by=ID1)) +
  geom_line(aes(x=Year, y=Tmax_wt, color=Elevation.m)) +
  geom_point(aes(x=Year, y=Tmax_wt, color=Elevation.m), data=seasonal.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Winter max temperature (\u00B0C)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("B")

Tsum <- ggplot(seasonal.sub, aes(x=Year, y=Tmax_sm, by=ID1)) +
  geom_line(aes(x=Year, y=Tmax_sm, color=Elevation.m)) +
  geom_point(aes(x=Year, y=Tmax_sm, color=Elevation.m), data=seasonal.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Summer max temperature (\u00B0C)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("C")

MAP <- ggplot(annual.sub, aes(x=Year, y=MAP, by=ID1)) +
  geom_line(aes(x=Year, y=MAP, color=Elevation.m)) +
  geom_point(aes(x=Year, y=MAP, color=Elevation.m), data=annual.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Mean annual precipitation (mm)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("D")

Psum <- ggplot(seasonal.sub, aes(x=Year, y=PPT_sm, by=ID1)) +
  geom_line(aes(x=Year, y=PPT_sm, color=Elevation.m)) +
  geom_point(aes(x=Year, y=PPT_sm, color=Elevation.m), data=seasonal.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Summer precipitation (mm)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("E")

Pwin <- ggplot(annual.sub, aes(x=Year, y=PAS, by=ID1)) +
  geom_line(aes(x=Year, y=PAS, color=Elevation.m)) +
  geom_point(aes(x=Year, y=PAS, color=Elevation.m), data=annual.survey, cex=2) +
  scale_color_gradient(low="red", high="blue", name="Elevation (m)") +
  ylab("Precipitation as snow (mm)") +
  xlab("Year") +
  theme_classic() +
  ggtitle("F")

# combine into multi-panel
all <- (MAT | Tsum | Twin) / (MAP | Psum | Pwin) + 
  plot_layout(guides="collect") 
ggsave("figures/climate.pdf", all, width=11, height=8)
#+
#  plot_annotation(tag_levels = 'A')



