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

registerDoMC (cores = 10)

## require (devtools)
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
render('allo.Rmd', output_file = 'allo.pdf')
render('ppca.Rmd', output_file = 'ppca.pdf')
render('modcomp.Rmd', output_file = 'modcomp.pdf')

render('sup_base.Rmd', output_file = 'sup_base.pdf')

system('okular tese.pdf &')
system('okular allo.pdf &')
system('okular ppca.pdf &')
system('okular modcomp.pdf &')

### Parcimony Allo

## write.csv(allo.Data $ integra.df [1:109, c('node', 'W.inter', 'W.slope')],
##           row.names = FALSE, col.names = TRUE, file = 'Data/allo.csv', quote = FALSE)

## write.nexus(Tree [[1]], file = 'Data/tree.nex')

## allo.Parc <- data.frame (cbind (111:217, which (!(Tree [[1]] $ edge [, 2] %in% 1:109)) + 2),
##                          read.csv('Data/allo_parc.csv') [-1, 2:3])

## head (allo.Parc)

## head (allo.Data $ model.slopes)

## allo.Data $ model.slopes %>%
##   filter(pos == 'Ancestor') %>%
##   mutate('parc' = allo.Parc $ slope) %>%
##   ggplot (.) +
##   geom_text(aes (x = parc, y = post.mean, label = node, color = pMCMC < 0.05),
##             size = 3) +
##   theme_bw() +
##   geom_abline(intercept = 0, slope = 1, linetype = 'dashed')
  
## allo.Data $ model.intercepts %>%
##   filter(pos == 'Ancestor') %>%
##   mutate('parc' = allo.Parc $ inter) %>%
##   ggplot (.) +
##   geom_text(aes (x = parc, y = post.mean, label = node, color = pMCMC < 0.05),
##             size = 3) +
##   theme_bw() +
##   geom_abline(intercept = 0, slope = 1, linetype = 'dashed')


