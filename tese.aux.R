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

system('okular sup_base.pdf &')

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


ggplot (subset (modcomp.Data $ Summ, type == 'RV')) +
  geom_tile(aes(x = data, y = otu, fill = value)) +
  facet_grid(size ~ hyp) +
  theme_minimal() +
  scale_fill_continuous(name = 'RV', high = 'yellow', low = 'blue', space = 'Lab',
                        limits = c(0, 1), 
                        breaks = c(0.1, 0.5, 0.9)) +
  geom_point (aes (x = data, y = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  xlab ('Hypothesis') + ylab ('OTU') + labs(title = 'RV Coefficent') +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ otu))) +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))

ggplot (subset (modcomp.Data $ Summ, type == 'MI')) +
  geom_tile(aes(x = data, y = otu, fill = value)) +
  facet_grid(size ~ hyp) +
  theme_minimal() +
  scale_fill_continuous(name = 'AVG Index',
                        high = 'blue', low = 'yellow', space = 'Lab',
                        breaks = c(-.3, 0, .3), limits = c(-.4, .4)) +
  geom_point (aes (x = data, y = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  xlab ('Hypothesis') + ylab ('OTU') + labs(title = 'AVG Index') +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ otu))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))
