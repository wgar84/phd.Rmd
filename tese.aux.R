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
require (gridExtra)
require (gridBase)
require (surface)
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

registerDoMC (cores = 3)

## require (devtools)
## install_github('muschellij2/slidify')
## install_github('ramnathv/slidifyLibraries')
## install_github('uyedaj/bayou')
## install_github ('mkoohafkan/kfigr')

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

attach ('Data/modcomp.Results.RData')
attach ('Data/allo.Results.RData')
attach ('Data/ppca.RData')

captions <- list()

render('tese.Rmd', output_file = 'tese.pdf')
## system('evince tese.pdf &')

render('sup_base.Rmd', output_file = 'sup_base.pdf')
## system('evince sup_base.pdf &')

render('allo.Rmd', output_file = 'allo.pdf')
## system('evince allo.pdf &')
render('sup_allo.Rmd', output_file = 'sup_allo.pdf')
## system('evince sup_allo.pdf &')

render('ppca.Rmd', output_file = 'ppca.pdf')
## system('evince ppca.pdf &')

render('modcomp.Rmd', output_file = 'modcomp.pdf')
## system('evince modcomp.pdf &')

render('sup_modcomp.Rmd', output_file = 'sup_modcomp.pdf')
## system('evince sup_modcomp.pdf &')

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'ioslides_presentation',
       output_file = 'Garcia_Evolution_2015.html')

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'beamer_presentation',
       output_file = 'Garcia_Evolution_2015.pdf')

### source ('altplot.modcomp.R')

### source ('parc.allo.R')

riem.tab <- sapply(ppca.Data$riem.decdiv.def.table, as.numeric)
rownames (riem.tab) <- rownames (ppca.Data$riem.decdiv.def.table)
riem.tab <- round (riem.tab, 3)
riem.tab <- data.frame(riem.tab)
riem.tab [, 'P-value'] <- '< 10^-4^'
riem.tab <- riem.tab[, -4]

author('Presentation/Evolution2015', F, F)
slidify('index.Rmd')
browseURL('index.html', 'firefox')

test <-
  ddply (modsim.Data $ cor.wb.df, .(size, type, wb), summarize,
         'med' = median (value),
         'mean' = mean (value),
         'sq.mean' = mean (value) ^ 2)

test $ mean [seq (2, 12, 2)] / test $ mean [seq (1, 11, 2)]

test $ sq.mean [seq (2, 12, 2)] / test $ sq.mean [seq (1, 11, 2)]

RandomSkewers(OneDef [['Homo_sapiens']] $ ml.vcv, OneDef [['Gorilla_gorilla']] $ ml.vcv)

RandomSkewers(ED [['Homo_sapiens']] $ ed.vcv, ED [['Gorilla_gorilla']] $ ed.vcv)

RandomSkewers(ED [['Papio_anubis']] $ ed.vcv, ED [['Homo_sapiens']] $ ed.vcv)

RandomSkewers(OneDef [['Papio_anubis']] $ ml.vcv [-1, -1],
              OneDef [['Homo_sapiens']] $ ml.vcv [-1, -1])

render ('Presentation/Allo/presAllo.Rmd')

