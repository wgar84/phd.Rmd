require (geomorph)
require (shapes)
require (RColorBrewer)
require (ape)
require (evolqg)
require (expm)
require (plyr)
require (plotrix)
require (doMC)
require (reshape2)
require (ggplot2)
require (phytools)
require (geiger)
require (mvtnorm)
require (MCMCglmm)
require (grid)
# require (gridExtra)
# require (gridBase)
# require (surface)
require (rmarkdown)
require (knitr)
require (pander)
require (shape)
require (adephylo)
require (dplyr)
require (magrittr)
require (tidyr)
require (slidify)
require (slidifyLibraries)
require (StatMatch)
require (scales)
require (cowplot)
require (ggtree)
require (phylobase)

registerDoMC (cores = 3)

require (devtools)
#install_github('jjallaire/revealjs')

## install_github('muschellij2/slidify')
## install_github('ramnathv/slidifyLibraries')
## install_github('uyedaj/bayou')

attach ('../Databases/Reference.RData')
attach ('../Databases/ED.RData')
attach ('../Databases/Sym.RData')
attach ('../Databases/OneDef.RData')
attach ('../Databases/Tree.RData')
attach ('../Databases/Aux.RData')
attach ('../Databases/LifeHistory.RData')

options(contrasts = c('contr.sum', 'contr.poly'))

.source.files <- dir('Func', pattern = '.R', full.names = TRUE)
.source.files <- .source.files [!grepl ('~', .source.files)]
for (i in 1:length (.source.files))
  source (.source.files [i])

attach ('Data/modcomp.RData')
## attach ('Data/modsim.hetero.RData')
## attach ('Data/modsim.orig.RData')
attach ('Data/modcomp.Results.RData')
attach ('Data/allo.results.RData')
## search()
## detach (pos = 2)
attach ('Data/ppca.RData')
attach ('Data/post.ppca.RData')
attach ('Data/tree.plots.RData')
attach ('Data/etd.def3.RData')

captions <- list()

## TEX

### render('tese.Rmd', output_file = 'tese.pdf')
render('allo.Rmd', output_file = 'allo.pdf')
render('sup_allo.Rmd', output_file = 'sup_allo.pdf')
render('ppca.Rmd', output_file = 'ppca.pdf')
render('sup_ppca.Rmd', output_file = 'sup_ppca.pdf')
render('modcomp.Rmd', output_file = 'modcomp.pdf')
render('sup_modcomp.Rmd', output_file = 'sup_modcomp.pdf')
#render('sup_base.Rmd', output_file = 'sup_base.pdf')
#render('rel_gen.Rmd', output_file = 'rel_gen.pdf')
#system('evince rel_gen.pdf &')

knit('allo.Rmd')

system('evince tese.pdf &')
system('evince allo.pdf &')
system('evince sup_allo.pdf &')
system('evince ppca.pdf &')
system('evince sup_ppca.pdf &')
system('evince modcomp.pdf &')
system('evince sup_modcomp.pdf &')
system('evince sup_base.pdf &')

## DATA OUT

posterior_P <- post.ppca $ W

save(posterior_P, file = 'posterior_P.RData')

### PRES

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'ioslides_presentation',
       output_file = 'Garcia_Evolution_2015.html')

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'beamer_presentation',
       output_file = 'Garcia_Evolution_2015.pdf')

### source ('altplot.modcomp.R')

### source ('ppca.extra.R')

### source ('parc.allo.R')

### source ('re-tree.R')

riem.tab <- sapply(ppca.Data$riem.decdiv.def.table, as.numeric)
rownames (riem.tab) <- rownames (ppca.Data$riem.decdiv.def.table)
riem.tab <- round (riem.tab, 3)
riem.tab <- data.frame(riem.tab)
riem.tab [, 'P-value'] <- '< 10^-4^'
riem.tab <- riem.tab[, -4]

author('Presentation/Evolution2015', F, F)
slidify('index.Rmd')
browseURL('index.html', 'firefox')

author('Presentation/Miudoless', F, F)
slidify('index.Rmd')
browseURL('index.html', 'google-chrome')


#### GGTREE
source("https://bioconductor.org/biocLite.R")
biocLite("ggtree")
#### fftw-devel, libtiff-devel

## var <- ddply (post.ppca $ plot.ppca.eval $ data, .(axis), plyr::summarise,
##               'mean.var' = mean(var))

## var <- var $ mean.var

## sum (var [var > 0]) / sum (abs (var))

### poe isso no ppca.extra.R depois

gradient <- colorRampPalette(rev(brewer.pal (10, 'Spectral')))

cac.root <-
  ggshape(
  allo.Data $ CAC.shapes[, , 3],
  Aux $ single.tessel.38, allo.Data $ root.CAC,
  palette = brewer.pal(10, 'Spectral'), rotation = c(1, -1, 1),
  culo = 0.03, thickness = 1.5) +
  ggtitle('Common Allometric Component') +
  scale_color_gradientn(
    'Trait Loadings',
    limits = c(-0.45, 0.45), colours = gradient(10),
    breaks = c(-0.3, 0, 0.3)) +
  scale_fill_gradientn(
    'Trait Loadings',
    limits = c(-0.45, 0.45), colours = gradient(10),
    breaks = c(-0.3, 0, 0.3)) +
  theme (legend.position = 'bottom') +
  guides(colour = guide_colorbar(title.position = "top"),
         fill = guide_colorbar(title.position = "top"))

cac.homo <-
  ggshape(
    Reference [, , 'Homo_sapiens'],
    Aux $ single.tessel.38, allo.Data $ wAC.all.Def ['Homo_sapiens', ],
    palette = brewer.pal(10, 'Spectral'), rotation = c(-1, 1, -1),
    culo = 0.03, thickness = 1.5) +
  ggtitle(expression (italic('Homo sapiens'))) +
  scale_color_gradientn(
    limits = c(-0.45, 0.45), colours = gradient(10)) +
  scale_fill_gradientn(
    limits = c(-0.45, 0.45), colours = gradient(10)) +
  guides (color = FALSE, fill = FALSE)

cac.gor <- 
  ggshape(
    Reference [, , 'Gorilla_gorilla'],
    Aux $ single.tessel.38, allo.Data $ wAC.all.Def ['Gorilla_gorilla', ],
    palette = brewer.pal(10, 'Spectral'), rotation = c(1, 1, 1),
    culo = 0.03, thickness = 1.5) +
  ggtitle(expression (italic('Gorilla gorilla')))+
    scale_color_gradientn(
      limits = c(-0.45, 0.45), colours = gradient(10)) +
  scale_fill_gradientn(
    limits = c(-0.45, 0.45), colours = gradient(10)) +
  guides (color = FALSE, fill = FALSE)

cac.ber <- 
  ggshape(
    Reference [, , 'Gorilla_beringei'],
    Aux $ single.tessel.38, allo.Data $ wAC.all.Def ['Gorilla_beringei', ],
    palette = brewer.pal(10, 'Spectral'), rotation = c(1, 1, 1),
    culo = 0.03, thickness = 1.5) +
  ggtitle(expression (italic ('Gorilla beringei'))) +
  scale_color_gradientn(
      limits = c(-0.45, 0.45), colours = gradient(10)) +
  scale_fill_gradientn(
    limits = c(-0.45, 0.45), colours = gradient(10)) +
  guides (color = FALSE, fill = FALSE)

cac.homi <- 
  ggdraw() +
  draw_plot(cac.homo, 0, 0.5, 0.5, 0.5) +
  draw_plot(cac.gor, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(cac.root, 0, 0, 0.5, 0.5) +
  draw_plot(cac.ber, 0.5, 0.1, 0.5, 0.5) +
  draw_line(c(0.52, 0.52), c(0, 0.55)) +
  draw_line(c(0, 0.52), c(0.55, 0.55)) +
  draw_line(c(0, 0), c(0, 0.55)) +
  draw_line(c(0, 0.52), c(0, 0))
