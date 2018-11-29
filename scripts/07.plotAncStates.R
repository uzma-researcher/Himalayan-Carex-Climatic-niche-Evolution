## do the metaMDS

doMDS = FALSE
testMDS = FALSE
library(vegan)
library(parallel)

if(testMDS) {
  anc.mds.k.gradient <- mclapply(1:10, function(x) metaMDS(scale(anc.all.bio), 'euclidean', k = x), mc.cores = 10)
  pdf('anc.mds.k.gradient.pdf')
  anc.mds.stress <- sapply(anc.mds.k.gradient, function(x) x$stress)
  plot(anc.mds.stress, xlab = 'k', ylab = 'stress', main = 'Stress plot, bioclim ordination')
  dev.off()
}

if(doMDS) {
  anc.mds <- metaMDS(scale(anc.all.bio), 'euclidean', k = 5)  ###*** No convergence -- monoMDS stopping criteria:20: stress ratio > sratmax, Warning message:In metaMDS(scale(dat.bio.means3[, paste("bio", 1:19, sep = "")]),  :'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
}

for(i in 2:5) {
  pdf(paste('anc.mds.1vs', i, '.pdf', sep = ''), 10, 10)
  ordisurf(anc.mds, anc.all.bio[, 'bio15'], col = 'gray90', main = "Ancestral state reconstructions, BIOCLIM")
  points(anc.mds$points[, c(1, i)], col = 'gray95')
  points(anc.mds$points[grep('Carex', row.names(anc.mds$points)), c(1, i)], col = 'gray80')
  points(matrix(anc.mds$points[as.character(mrca.all.species[c('kobresia2')]), c(1, i)], ncol = 2),
         col = 'orange', pch = 19, cex = 2)
  points(anc.mds$points[dat.bio.spp.kob, c(1, i)], col = 'red', pch = 19)
  points(anc.mds$points[as.character(mrca.all.species), c(1, i)], col = 'lightblue', pch = 19)
  points(anc.mds$points[setdiff(dat.spp.him, dat.bio.spp.kob), c(1, i)], col = 'blue', pch = 19)
  text(anc.mds$points[as.character(mrca.all.species[c('kobresia1', 'kobresia2')]), c(1, i)],
       c('Kobresia clade 1 (minor)', 'Kobresia clade 2 (major)'),
        pos = 4)
  legend('topleft',
         c('Ancestral nodes for Carex, not Himalayan radiations',
           'All tips, not Himalayan species',
           'Ancestral node for Kobresia clade',
           'Himalayan Kobresia, tips',
           'Ancestral nodes of Himalayan radiations',
           'Other Himalayan species, tips',
           'BIO15 isoclines (precipitation seasonality)'),
         col = c('gray95', 'gray80', 'orange', 'red', 'lightblue', 'blue', 'gray90'),
         pch = c(1, 1, 19, 19, 19, 19, NA),
         lty = c(rep(NA, 6), 'solid'),
         cex = 0.6,
         pt.cex = 2,
         bty = 'n'
       )
  dev.off()
}


