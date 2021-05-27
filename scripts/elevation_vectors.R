# exploring to determine what poly-tranformed values should be

# rachel saved these from model runs
dat <- read_csv("data/3c_transformed_polynomials.csv")

# relationship between quadratic and linear terms in poly-transformed units is a perfect quadratic
ggplot(data=dat, aes(x=Elevation.m.poly, y=Elevation.m2.poly)) +
geom_point() +
geom_smooth(method="lm", formula= y~poly(x,2))

# quadratic function is given by this model
poly.mod <- lm(Elevation.m2.poly ~ Elevation.m.poly + I(Elevation.m.poly^2), data=dat)

# linear vector
# range based on min/max values in dat$Elevation.m.poly
elev.vec.lin = as.numeric(seq(min(dat$Elevation.m.poly), max(dat$Elevation.m.poly), by=0.0001)) 

# quadratic vector
elev.vec.quad = poly.mod$coefficients[1] + 
  elev.vec.lin*poly.mod$coefficients[2] + 
  elev.vec.lin*elev.vec.lin*poly.mod$coefficients[3]
plot(elev.vec.quad ~ elev.vec.lin) + 
points(dat$Elevation.m.poly, dat$Elevation.m2.poly, col="red") 
#ok! we have linear and quadratic elevation vectors that match what the models are using


#### elevation list for back-transformed axis labels
# needs to be in raw units (m)
ggplot(data=dat, aes(x=Elevation.m.poly, y=Elevation.m)) +
  geom_point() +
  geom_smooth(method="lm")

poly.ticks.default = seq(-0.08, 0.08, by=0.04)
back.mod.default <- lm(Elevation.m ~ Elevation.m.poly, data=dat)
raw.ticks.default = back.mod.default$coefficients[1] + poly.ticks.default*back.mod.default$coefficients[2]

back.mod.custom <- lm(Elevation.m.poly ~ Elevation.m, data=dat)
raw.ticks.custom = c(100, 600, 1100, 1600, 2100)
poly.ticks.custom = back.mod.custom$coefficients[1] + raw.ticks.custom*back.mod.custom$coefficients[2]

# save these for graphing
ticks.custom <- as.data.frame(cbind(raw.ticks.custom, poly.ticks.custom))
write_csv(ticks.custom, "data/tickmarks.csv")
