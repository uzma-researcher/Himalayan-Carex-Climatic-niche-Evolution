## read data from gbif
require(rgbif)
require(maps)
taxa <- readLines('../DATA/taxa')
dat.gbif <- vector('list', length(taxa))
names(dat.gbif) <- taxa
for(i in taxa)
  {
  message(paste('Reading data for', i))
  temp <- try(occ_data(scientificName = i, limit = 20000))
  if(class(temp) == 'try-error') next
  write.csv(temp$data, paste('../GBIF.DATA/',
                        gsub(" ", "_", i, fixed = T),
                        '.csv', sep = '')
                        )
  dat.gbif[[i]] <- temp
  temp <- temp$data
  message(paste('Mapping', i))
  mapTry <- try(map(xlim = range(temp$decimalLongitude, na.rm = T),
      ylim = range(temp$decimalLatitude, na.rm = T)))
  if(class(mapTry) == 'try-error') message(' -- FAILED MAP')
  else {
    pdf(paste('../MAPS/', gsub(" ", "_", i, fixed = T), '.pdf', sep = ''))
    map(xlim = range(temp$decimalLongitude, na.rm = T),
        ylim = range(temp$decimalLatitude, na.rm = T))
    points(temp$decimalLongitude, temp$decimalLatitude, pch = 19, col = 'red')
    text(x=seq(from = -180, to = 180, by = 10), y = min(temp$decimalLatitude, na.rm = T) + 1, seq(from = -180, to = 180, by = 10), cex = 0.5)
    text(y=seq(from = -180, to = 180, by = 10), x = min(temp$decimalLongitude, na.rm = T) + 1, seq(from = -180, to = 180, by = 10), cex = 0.5)
    dev.off()
  }
}
