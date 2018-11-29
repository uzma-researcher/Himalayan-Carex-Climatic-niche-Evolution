
library(vegan)

do.mds <- F
extendBy <- 0.2

anc.only.bio <- anc.all.bio[grep('Carex', row.names(anc.all.bio), invert = T), ]
spp.only.bio <- anc.all.bio[grep('Carex', row.names(anc.all.bio), invert = F), ]

if(do.mds) {
  anc.mds.2 <- metaMDS(scale(anc.only.bio), 'euclidean', k = 2)
  spp.mds.2 <- metaMDS(scale(spp.only.bio), 'euclidean', k = 2)
  spp.mds.5 <- metaMDS(scale(spp.only.bio), 'euclidean', k = 5)

  anc.mds.2.r <- cor(anc.only.bio, anc.mds.2$points)
  ## correlations > 0.7
  anc.mds.2.r[abs(anc.mds.2.r[, 1]) >= 0.7 , 1]
  anc.mds.2.r[abs(anc.mds.2.r[, 2]) >= 0.7 , 2]

  spp.mds.2.r <- cor(spp.only.bio, spp.mds.2$points)
  ## correlations > 0.7
  spp.mds.2.r[abs(spp.mds.2.r[, 1]) >= 0.7 , 1]
  spp.mds.2.r[abs(spp.mds.2.r[, 2]) >= 0.7 , 2]
## same results for both ordinations
# axis 1; 8 bioclim variables, temp (1,4,6,9,11) and precipitation (12,13,16)
# axis 2; 3 bioclim variables related to drought and precip seasonality (14, 15, 17)
}



groups.anc = matrix(rep("Carex", dim(anc.mds.2$points)[1]), dimnames = list(row.names(anc.mds.2$points), NULL))
groups.anc[as.character(mrca.all.species[grep('kobresia', names(mrca.all.species), invert = T)]), ] <- 'Himalaya'
groups.anc[as.character(mrca.all.species[grep('kobresia', names(mrca.all.species), invert = F)]), ] <- 'Kobresia'
ga <- as.factor(groups.anc[, 1])

groups.spp <- matrix(rep('Carex', dim(spp.mds.2$points)[1]), dimnames = list(row.names(spp.mds.2$points), NULL))
groups.spp[dat.spp.him, ] <- 'Himalaya'
groups.spp[grep('Kobresia', row.names(groups.spp)), ] <- 'Kobresia'
gs <- as.factor(groups.spp[, 1])

anc.cols = c(all = 'gray70',
             him = 'blue',
             kob = 'red',
             bio = 'gray80',
             ellipse = 'black')

## plot anc ordination
pdf('Fig6.v3-2.ordination.pdf', 7, 11) # formatting to 7 x 11 inches
layout(matrix(1:2, 2))
par(mai = c(0.5,0.5,0.1,0.5))
ordisurf(anc.mds.2, anc.only.bio[, 'bio12'], col = anc.cols['bio'],
         ylim = c(range(anc.mds.2$points[, 2]) + c(0, extendBy * diff(range(anc.mds.2$points[, 2])))),
         #main = "NMDS ordination, BIOCLIM ancestral states",
         main = '',
         cex = 0
       )
i = 2
points(anc.mds.2$points[, c(1, i)], col = anc.cols['all'])
points(anc.mds.2$points[as.character(mrca.all.species), c(1, i)], col = anc.cols['him'], pch = 19)
points(matrix(anc.mds.2$points[as.character(mrca.all.species[grep('kobresia', names(mrca.all.species))]), c(1, i)], ncol = 2),
       col = anc.cols['kob'], pch = 19, cex = 1)
ordiellipse(anc.mds.2, ga,
            show.groups = "Himalaya",
            kind = 'sd', conf = 0.95, add = T,
          lty = 'dashed')

text(anc.mds.2$points[as.character(mrca.all.species[c('kobresia1', 'kobresia2')]), c(1, i)],
     c('Kobresia clade 1 MRCA (minor)', 'Kobresia clade 2 MRCA (major)'),
      pos = 4,
    cex = 0.8)
legend('topleft',
       c('Ancestral nodes for Carex, not Himalayan radiations',
         'Ancestral nodes for Himalayan radiations other than Kobresia',
         'Ancestral node for two primary Kobresia clades',
         'BIO12 isoclines (mean annual precipitation)',
         '95% confidence interval (s.d.) around Himalayan ancestral nodes, excluding Kobresia'),
       col = c(anc.cols),
       pch = c(1, 19, 19, NA, NA),
       lty = c(rep(NA, 3), 'solid', 'dashed'),
       cex = 0.7,
       pt.cex = 0.7,
       bty = 'n',
       y.intersp = 1.2
     )
legend('topright', 'A', , cex = 1.2, bty = 'n')
#dev.off()

## plot spp ordination
#pdf('Fig6b-spp.v2.ordination.pdf', 7, 7) # formatting to 7 inches
par(mai = c(0.5,0.5,0.1,0.5))
ordisurf(spp.mds.2, spp.only.bio[, 'bio12'], col = anc.cols['bio'],
         ylim = c(range(spp.mds.2$points[, 2]) + c(0, extendBy * diff(range(spp.mds.2$points[, 2])))),
         #main = "NMDS ordination, BIOCLIM tip states",
         main = '',
         cex = 0
       )
i = 2
points(spp.mds.2$points[which(gs == 'Carex'), c(1, i)], col = anc.cols['all'])
points(spp.mds.2$points[which(gs == 'Himalaya'), c(1, i)], col = anc.cols['him'], pch = 19)
points(spp.mds.2$points[which(gs == 'Kobresia'), c(1, i)], col = anc.cols['kob'], pch = 19)
ordiellipse(spp.mds.2, gs,
          #  show.groups = "Himalaya",
            col = anc.cols[c('all', 'him', 'kob')],
            kind = 'sd', conf = 0.95, add = T,
            lty = 'dashed')

legend('topleft',
       c('Carex tips, not Himalayan',
         'Himalayan tips other than Kobresia',
         'Kobresia tips',
         'BIO12 isoclines (mean annual precipitation)',
         '95% confidence intervals (s.d.)'),
       col = c(anc.cols),
       pch = c(1, 19, 19, NA, NA),
       lty = c(rep(NA, 3), 'solid', 'dashed'),
       cex = 0.7,
       pt.cex = 0.7,
       bty = 'n',
       y.intersp = 1.2
     )
legend('topright', 'B', cex = 1.2, bty = 'n')

dev.off()
