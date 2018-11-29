library(ape)
datDir = '../CLEANED.GBIF.DATA'

dat.list <- lapply(dir('../CLEANED.GBIF.DATA', patt = 'cleaned.csv', full = T), read.csv, as.is = T)

dat.list2 <- lapply(dat.list, function(x) x[, c('decimalLatitude', 'decimalLongitude', 'name')]) ## here you should put whatever columns you want to keep
dat.combo <- do.call(rbind, dat.list2)

## 2018-01-05: getting rid of NAs
dat.combo <- dat.combo[!is.na(dat.combo$decimalLatitude), ]

write.csv(dat.combo, 'all.species.cleaned.gbif.csv')
