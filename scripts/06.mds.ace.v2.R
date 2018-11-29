library(geiger)
library(ape)
library(raster)
library(sp)
library(vegan)
library(phytools)

excludes = grep('breviprophylla', tr2$tip.label, value = T)
dat.bio.means3 <- read.csv('dat.bio.means3.csv', as.is = T, row.names = 1)
dat.bio.spp <- intersect(tr2$tip.label, row.names(dat.bio.means3))
dat.bio.spp <- setdiff(dat.bio.spp, excludes)

message(paste(length(dat.bio.spp), 'species being used'))
tr3 <- drop.tip(tr2, which(!tr2$tip.label %in% dat.bio.spp))
dat.bio.means3 <- dat.bio.means3[tr3$tip.label, ]
pak.nodes.list <- list(kobresia1 = c("Carex_gammiei|NA|spm00003640|Kobresia","Carex_pseudogammiei|NA|spm00003639|Kobresia"),
                           kobresia2 = c("Carex_prainii|EHM|spm00005923|Kobresia", "Carex_setschwanensis|NA|spm00003620|Kobresia"),
                           vignea1 = c("Carex_physodes|TZK|spm00005427|Physodeae","Carex_pachystylis|UZB|spm00003472|Physodeae"),
						   vignea2 = c("Carex_alta|CH?|spm00002237|Planatae","Carex_pseudobrizoides|POL|spm00002177|Ammoglochin"),
						   vignea3 = c("Carex_echinata|GRB|spm00005186|Stellulatae", "Carex_interior|MIC|spm00000972|Stellulatae"),
						   vignea4 = c("Carex_curaica|TVA|spm00002340|Holarrhenae","Carex_sartwellii|BRC|spm00001982|Holarrhenae"),
						   vignea5 = c("Carex_fluviatilis|CHC|spm00002407|Phleoideae", "Carex_nubigena|PAK|183085|Phleoideae"),
						   vignea6 = c("Carex_canescens|PAK|183104|Glareosae", "Carex_arctiformis|BRC|spm00001851|Glareosae"),
						   vignea7 = c("Carex_diandra|CAL|spm00000840|Heleoglochin", "Carex_prairea|BRC|spm00001951|Heleoglochin"),
						   vignea8 = c("Carex_divisa|MAN|spm00005374|Divisae", "Carex_erythrorrhiza|ETH|spm00000191|Stenorhynchae"),
						   vignea9 = c("Carex_leporina|WIS|spm00001780|Ovales", "Carex_longii|ARK|spm00001001|Ovales"),
						   vignea10 = c("Carex_otrubae|SPA|spm00000559|Vulpinae", "Carex_glomerabilis|CPP|spm00000193|Stenorhynchae"),
						   vignea11 = c("Carex_pseudofoetida|PAK|183071|Foetidae", "Carex_maritima|YUK|spm00002500|Foetidae"),
						   vignea12 = c("Carex_stenophylla|TCS|spm00003603|Divisae", "Carex_duriuscula|ARK|spm00001653|Divisae"),
						   vignea13 = c("Carex_vulpinaris|AFG|spm00003473|Phaestoglochin", "Carex_appropinquata|GER|spm00002248|Heleoglochin"),
						   vignea14 = c("Carex_wallichiana|PAK|183066|Echinochloomorphae", "Carex_leersii|SPA|spm00004393|Phaestoglochin"),
						   corecarex1 = c("Carex_indica|QLD|spm00002191|Indicae", "Carex_longipes|EHM|spm00005772|Graciles"),
						   corecarex2 = c("Carex_munda|EHM|spm00003372|Mundae", "Carex_gongshanensis|CHT|spm00003361|Graciles"),
						   corecarex3 = c("Carex_daltonii|EHM|spm00005702|Decorae", "Carex_pulchra|EHM|spm00005828|Decorae"),
						   corecarex4 = c("Carex_filicina|PAK|183073|Indicae", "Carex_echinochloe|NA|spm00004701|Indicae"),
						   corecarex5 = c("Carex_borii|WHM|spm00003355|Aulocystis", "Carex_baccans|TAI|spm00005149|Polystachyae"),
						   corecarex6 = c("Carex_radicalis|EHM|spm00005831|Radicales", "Carex_laeta|CHT|spm00003394|Clandestinae"),
						   corecarex7 = c("Carex_alajica|CHX|spm00003353|Careyanae", "Carex_oligocarya|PAK|spm00003395|Clandestinae"),
						   corecarex8 = c("Carex_tangulashanensis|CHQ|spm00003591|Lamprochlaenae", "Carex_turkestanica|KGZ|spm00003476|Lamprochlaenae"),
						   corecarex9 = c("Carex_flacca|CAL|spm00001809|Thuringiaca", "Carex_spissa|MXE|spm00000296|Thuringiaca"),
						   corecarex10 = c("Carex_stenocarpa|CHC|spm00003385|Aulocystis", "Carex_fuliginosa|ARK|spm00001627|Aulocystis"),
						   corecarex11 = c("Carex_finitima|CHC|spm00005719|Debiles", "Carex_fusiformis|EHM|spm00005727|Debiles"),
						   corecarex12 = c("Carex_jackiana|EHM|spm00005760|Paniceae", "Carex_laxa|ARK|spm00001643|Paniceae"),
						   corecarex13 = c("Carex_capillaris|NWT|spm00002094|Chlorostachyae", "Carex_krausei|ARK|spm00001639|Chlorostachyae"),
						   corecarex14 = c("Carex_atrata|AUT|spm00002172|Racemosae", "Carex_stevenii|COL|spm00001710|Racemosae"),
						   corecarex15 = c("Carex_pseudobicolor|PAK|spm00003380|Racemosae", "Carex_infuscata|PAK|183079|Racemosae"),
						   corecarex16 = c("Carex_lehmannii|CHC|spm00003367|Racemosae", "Carex_sabulosa|YUK|spm00001646|Racemosae"),
						   corecarex17 = c("Carex_cruenta|EHM|spm00003357|Aulocystis", "Carex_atrofusca|PAK|183078|Aulocystis"),
						   corecarex18 = c("Carex_burchelliana|CPP|spm00004368|Spirostachyae", "Carex_diluta|TCS|spm00004364|Spirostachyae"),
						   corecarex19 = c("Carex_acutiformis|TVL|spm00002122|Paludosae", "Carex_nemostachys|CHS|spm00000088|Confertiflorae"),
						   corecarex20 = c("Carex_alopecuroides|KOR?|spm00000321|Molliculae", "Carex_doniana|CHS|spm00000047|Molliculae"),
						   corecarex21 = c("Carex_pruinosa|CHC|spm00005813|Phacocystis", "Carex_rhodesiaca|NA|spm00004696|Phacocystis"),
						   corecarex22 = c("Carex_comosa|CAL|spm00000826|Vesicariae", "Carex_pseudocyperus|MIC|spm00001179|Vesicariae"),
						   corecarex23 = c("Carex_obscuriceps|EHM|spm00005802|Vesicariae", "Carex_sphaerogyna|MDG|spm00000237|Vesicariae"),
						   corecarex24 = c("Carex_songorica|TZK|spm00003384|Paludosae", "Carex_gotoi|KOR|spm00000020|Paludosae"),
						   corecarex25 = c("Carex_fedia|PAK|spm00003461|Carex", "Carex_glabrescens|KOR?|spm00000332|Carex"),
						   corecarex26 = c("Carex_rostrata|WAS|spm00001600|Vesicariae", "Carex_membranacea|ASK|spm00001050|Vesicariae"),
						   corecarex27 = c("Carex_fucata|CHC|spm00006432|Phacocystis", "Carex_haydenii|WIS|spm00002433|Phacocystis"),
						   corecarex28 = c("Carex_nigra|NFL|spm00001495|Phacocystis", "Carex_acuta|Italy|spm00003084|Phacocystis"),
						   corecarex29 = c("Carex_stricta|TEX|spm00001296|Phacocystis", "Carex_bigelowii|NA|spm00003230|Phacocystis"))

### get nodes of interest
mrca.all.species <- sapply(pak.nodes.list,
                      getMRCA,
                      phy = tr3)

### get ancestral character states for each 19 bioclim variables
anc.all.bio <- sapply(paste('bio', 1:19, sep = ''), function(x) {
  message(paste('doing ancestral state reconstruction for', x))
  dat.temp <- dat.bio.means3[[x]]
  names(dat.temp) <- row.names(dat.bio.means3)
  fastAnc(tr3, dat.temp)
})

dat.spp.him <- read.delim('../DATA/carex-two-regions-narrow.txt', as.is = T, row.names = 1)
dat.spp.him <- row.names(dat.spp.him)[dat.spp.him$state %in% c(0, 2)]
dat.spp.him <- intersect(dat.spp.him, dat.bio.spp)
dat.bio.spp.kob <- intersect(
                             grep('Kobresia', tr3$tip.label, value = T),
                             dat.spp.him
                           )

anc.all.bio <- rbind(anc.all.bio,
                     dat.bio.means3)

message('done with ancestral char state reconstruction')
