## niche analyses part 2 -- OU models
## make sure you run 08 first!

library(ape)
#library(l1ou)
#dat.l1ou <- adjust_data(tr3, spp.mds.2$points)
#dat.ou <- estimate_shift_configuration(dat.l1ou$tree, dat.l1ou$Y, nCores = 10)
library(PhylogeneticEM)
library(magrittr)
library(phangorn)
library(doParallel)

dat.em <- PhyloEM(
  phylo = tr3,
  Y_data = t(spp.mds.2$points),
  process = 'scOU',
  parallel_alpha = TRUE,
  Ncores = 14
)

dat.em.5 <- PhyloEM(
  phylo = tr3,
  Y_data = t(spp.mds.5$points),
  process = 'scOU',
  parallel_alpha = TRUE,
  nbr_alpha = 15,
  Ncores = 16
)

dat.em.5.highAlpha <- PhyloEM(
  phylo = tr3,
  Y_data = t(spp.mds.5$points),
  process = 'scOU',
  parallel_alpha = TRUE,
  alpha = c(1,3,5),
  Ncores = 3
)


save(dat.em, dat.em.5, file = 'ou.dat.em.2018-04-06.Rdata')

dat.em.nodes <-
  params_process(dat.em)$shifts$edges %>%
  tr3$edge[., 2]

dat.em.children <- Descendants(tr3, dat.em.nodes, 'tips') %>%
  lapply(., function(x) tr3$tip.label[x])

dat.em.5.highAlpha.nodes <-
    params_process(dat.em.5.highAlpha)$shifts$edges %>%
    tr3$edge[., 2]

dat.em.5.highAlpha.children <- Descendants(tr3, dat.em.5.highAlpha.nodes, 'tips') %>%
    lapply(., function(x) tr3$tip.label[x])


pdf('FigSx.PhylogeneticEM.pdf', 8.5, 11)
plot(dat.em)
pX <- get("last_plot.phylo",envir=.PlotPhyloEnv)
text(rep(pX$x.lim[1], 3),
     pX$yy[dat.em.nodes[1:3]],
     c('Core unispicate', 'Indicae', 'Uncinia'),
     pos = 4,
     cex = 0.6
   )
dev.off()

pdf('FigSx.OrdinationWithPhyloEM.colors.pdf')
plot(spp.mds.2$points, col = 'gray')
points(spp.mds.2$points[dat.em.children[[1]], ], pch = 19, col = 'red')
points(spp.mds.2$points[dat.em.children[[2]], ], pch = 19, col = 'yellow')
points(spp.mds.2$points[dat.em.children[[3]], ], pch = 19, col = 'blue')
#points(spp.mds.2$points[grep('Kobresia', row.names(spp.mds.2$points)), ], cex = 1.4)
points(spp.mds.2$points[dat.spp.him, ], cex = 1)
legend('bottomleft',
       legend = c('Core unispicate clade',
                  'Predominantly Indicae clade',
                  'Uncinia clade',
                  'All other Carex, not Himalayan',
                  'All Himalayan Carex'),
        pch = c(19,19,19,1,1),
        col = c('red', 'yellow', 'blue', 'gray','black'),
        bty = 'n')
dev.off()
