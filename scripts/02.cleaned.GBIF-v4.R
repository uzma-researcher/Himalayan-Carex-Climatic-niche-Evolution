### if already have gbif data file

library(dismo)
library(ape)
library(magrittr)

gbifDir = '../GBIF.DATA' # put here the directory that your gbif files are in
gbifFiles = dir(gbifDir, patt = '.csv')

for(i in gbifFiles) {
  temp <- read.csv(paste(gbifDir, '/', i, sep = ''), as.is = T)
  if(!('decimalLatitude' %in% names(temp) & 'decimalLongitude' %in% names(temp))) {
    message(paste('File', i, 'apparently has no decimalLatitude field'))
    next
  } # this goes to the next file if there is not decimalLatitude and decimalLongitude
  acgeo <- temp[which(!is.na(temp$decimalLongitude) &
                      !is.na(temp$decimalLatitude)
                      ), ]
  # lonzero = subset(acgeo, lon==0)

  # the following line filters out specimens that are within 0.001 degrees of each other;
  # this is identical to Hipp et al. 2018 (New Phytologist) and has the effect of thinning specimens to a min of ca. 1-1.6 km of each other
  acgeo.noDups <- which(!duplicated(paste(round(temp$decimalLongitude,2), round(temp$decimalLatitude,2))))

  dups2 <- duplicated(acgeo[, c('decimalLatitude', 'decimalLongitude')])
  acg <- acgeo[!dups2, ]
  write.csv(acg,
            paste(gbifDir,
                  '/',
                  gsub('.csv', '', i, fixed = T),
                  '.cleaned.csv',
                  sep = '')
            )
}
