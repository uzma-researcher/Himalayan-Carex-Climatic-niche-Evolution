library(raster)
library(sp)
library(ape)
worldclim <- raster:::getData("worldclim", var="bio",res=10)

## 2018-01-05: in line below, changed 'x' to 'dat.combo'
dat.gbif <- cbind(dat.combo, extract(worldclim, SpatialPoints(dat.combo[ , c('decimalLongitude', 'decimalLatitude')])))

## 2018-01-05: getting rid of NAs
dat.gbif <- dat.gbif[!is.na(dat.gbif$decimalLatitude), ]

## 2018-01-05: if you are going to save, save here:
write.csv(dat.gbif, 'dat.gbif.csv')
## First, we'll split the data by species:
dat.bio.bySp <- split(dat.gbif, dat.gbif$species)

## estimate of the mean and measurement error for each species
numCols <- grep('decimalLatitude|decimalLongitude|bio', species(dat.gbif), value = T)
dat.bio.means <- t(sapply(dat.bio.bySp, function(x) apply(x[, numCols], 2, mean, na.rm = T)))
head(dat.bio.means)
write.csv(dat.bio.means, 'dat.bio.means.csv')

## a matrix holding the standard error of the mean for each value
dat.bio.sem <- t(sapply(dat.bio.bySp, function(x) {
    apply(x[, numCols], 2, sd, na.rm = T) / sqrt(dim(x)[1])
    } # close function
                        ) # close sapply
                 ) # close it
head(dat.bio.sem)

## some of our species are represented by only one specimen, they can't have a standard deviation, as
temp.na = names(which(table(dat.gbif$species) == 1))
print(temp.na)
dat.bio.sem[temp.na, ]
## Solution
singletons = names(which(sapply(dat.bio.bySp, function(x) sum(!is.na(x$bio1)))==1))
# dat.bio.sem[singletons, ]
## Solution
temp.sd <- apply(t(sapply(dat.bio.bySp[-singletons, ], function(x) apply(x[, numCols], 2, sd, na.rm = T))),
2,
mean,
na.rm = T) # the mean of the standard deviation for all these variables
dat.bio.sem[singletons, ] <- matrix(temp.sd, length(singletons), length(temp.sd), byrow = T)
dat.bio.sem[singletons, ] ## did it work?
write.csv(dat.bio.sem, 'dat.bio.sem.v1.csv')
plot(dat.bio.mds$points)