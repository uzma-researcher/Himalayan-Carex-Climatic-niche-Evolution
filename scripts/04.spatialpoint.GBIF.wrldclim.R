library(raster)
library(sp)
worldclim <- raster:::getData("worldclim", var="bio",res=10)

## 2018-01-05: in line below, changed 'x' to 'dat.combo'
dat.gbif <- cbind(dat.combo, extract(worldclim, SpatialPoints(dat.combo[ , c('decimalLongitude', 'decimalLatitude')])))

## 2018-01-05: if you are going to save, save here:
write.csv(dat.gbif, 'dat.gbif.csv')
