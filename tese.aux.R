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
# require (grid)
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

registerDoMC (cores = 10)

## require (devtools)
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

captions <- list()

## TEX

render('tese.Rmd', output_file = 'tese.pdf')
render('allo.Rmd', output_file = 'allo.pdf')
render('sup_allo.Rmd', output_file = 'sup_allo.pdf')
render('ppca.Rmd', output_file = 'ppca.pdf')
render('sup_ppca.Rmd', output_file = 'sup_ppca.pdf')
render('modcomp.Rmd', output_file = 'modcomp.pdf')
render('sup_modcomp.Rmd', output_file = 'sup_modcomp.pdf')
#render('sup_base.Rmd', output_file = 'sup_base.pdf')

system('evince tese.pdf &')
system('evince allo.pdf &')
system('evince sup_allo.pdf &')
system('evince ppca.pdf &')
system('evince sup_ppca.pdf &')
system('evince modcomp.pdf &')
system('evince sup_modcomp.pdf &')
system('evince sup_base.pdf &')

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

#### GGTREE
### source("https://bioconductor.org/biocLite.R")
### biocLite("ggtree")
#### fftw-devel, libtiff-devel





