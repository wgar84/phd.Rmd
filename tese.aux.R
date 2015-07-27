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
render('allo.Rmd', output_file = 'allo.pdf')
render('ppca.Rmd', output_file = 'ppca.pdf')

render('modcomp.Rmd', output_file = 'modcomp.pdf')
render('sup_modcomp.Rmd', output_file = 'sup_modcomp.pdf')
# system('evince modcomp.pdf &')
# system('evince sup_modcomp.pdf &')

render('sup_base.Rmd', output_file = 'sup_base.pdf')

system('evince tese.pdf &')
system('evince allo.pdf &')
system('evince ppca.pdf &')
system('evince sup_base.pdf &')

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'ioslides_presentation',
       output_file = 'Garcia_Evolution_2015.html')

render('Presentation/PhyloComp/pres_PhyloComp.Rmd',
       output_format = 'beamer_presentation',
       output_file = 'Garcia_Evolution_2015.pdf')


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

### ALT DIST SIM

alt.dist.sim <- 
ggplot (modsim.Data $ values.df) +
  geom_boxplot(aes(x = interaction (variable, type, sep = ' - '), y = value,
                   fill = interaction (variable, type, sep = ' - '),
                   color = interaction (variable, type, sep = ' - ')),
               alpha = 0.5, position = 'identity',
               outlier.shape = '+', outlier.size = 1) +
  coord_flip() +
  facet_grid(size ~ stat, scales = 'free') +
  scale_fill_brewer(name = 'Simulated Matrix Type', palette = 'Paired') +
  scale_color_brewer(name = 'Simulated Matrix Type', palette = 'Paired') +
  ylab('Value') + xlab ('') +
  scale_x_discrete(breaks = NULL) +
  theme_bw() 

### PRES PPCA

riem.tab <- sapply(ppca.Data$riem.decdiv.def.table, as.numeric)
rownames (riem.tab) <- rownames (ppca.Data$riem.decdiv.def.table)
riem.tab <- round (riem.tab, 3)
riem.tab <- data.frame(riem.tab)
riem.tab [, 'P-value'] <- '< 10^-4^'
riem.tab <- riem.tab[, -4]

author('Presentation/Evolution2015', F, F)
slidify('index.Rmd')
browseURL('index.html', 'firefox')

## mahala.shape <-
##   cmdscale (
##     as.dist (
##       mahalanobis.dist(laply (OneDef, function (L) L $ mean),
##                        vc = allo.Data $ W.node.Def [['Platyrrhini']])))

## mahala.shape <-
##   data.frame ('OTU' = names (OneDef),
##               'pco' = mahala.shape)

## ggplot(mahala.shape) +
##   geom_text (aes(x = pco.1, y = pco.2, label = OTU))

## sqrt (Evolvability(solve (allo.Data $ W.node.Def [['Anthropoidea']]) %*%
##                    (OneDef [[108]] $ mean - OneDef [[109]] $ mean),
##                    allo.Data $ W.node.Def [['Anthropoidea']]))

## as.dist (
##   mahalanobis.dist(laply (OneDef, function (L) L $ mean),
##                    vc = allo.Data $ W.node.Def [['Platyrrhini']]))

## mahala.ed <-
##   cmdscale (
##     as.dist (
##       mahalanobis.dist(laply (ED, function (L) L $ ed.mean) [, -20],
##                        vc = allo.Data $ W.node.ED [['Anthropoidea']])))

## mahala.ed <-
##   data.frame ('OTU' = names (OneDef),
##               'pco' = mahala.ed)

## ggplot(mahala.ed) +
##   geom_text (aes(x = pco.1, y = pco.2, label = OTU))

## sqrt (Evolvability(solve (allo.Data $ W.node.Def [['Anthropoidea']]) %*%
##                    (OneDef [[108]] $ mean - OneDef [[109]] $ mean),
##                    allo.Data $ W.node.Def [['Anthropoidea']]))

## as.dist (
##   mahalanobis.dist(laply (OneDef, function (L) L $ mean),
##                    vc = allo.Data $ W.node.Def [['Platyrrhini']]))


## sym.mean <- laply (Sym, function (L) L $ sym.mean)
## sym.mean <- aperm (sym.mean, c(2, 3, 1))
## sym.gpa <- procGPA(sym.mean, pcaoutput = FALSE)

## tan <- t (sym.gpa $ tan)

## pco.to.shape.1 <- coef (lm (tan ~ mahala.shape [, 2])) [2, ]
## pco.to.shape.2 <- coef (lm (tan ~ mahala.shape [, 3])) [2, ]

## Norm(pco.to.shape.1)

## dim (pco.to.shape.1) <- c(36, 3)
## dim (pco.to.shape.2) <- c(36, 3)

## pco.shape.1 <- array (0, c(36, 3, 5))
## pco.value <- c(-2, -1, 0, 1, 2)
## for (i in 1:5)
##   pco.shape.1 [, , i] <-
##   allo.Data $ sym.gpa $ mshape + pco.value [i] * pco.to.shape.1
  
## dimnames (pco.shape.1) [1:2] <- dimnames (Sym [[1]] $ sym) [1:2]

## for (i in 1:5)
##   {
##     coleurs <- colorRampPalette(c ('blue', 'red'))(5)
##     points3d(pco.shape.1 [, , i], rgl.open = ifelse (i == 1, T, F),
##              col = coleurs [i])
##     for (j in 1:(dim (Aux $ wireframe) [1]))
##       lines3d (allo.Data $ CAC.shape [Aux $ wireframe [j, ], , i], col = coleurs [i])
##   }

## pco.shape.2 <- array (0, c(36, 3, 5))
## pco.value <- c(-2, -1, 0, 1, 2)
## for (i in 1:5)
##   pco.shape.2 [, , i] <-
##   allo.Data $ sym.gpa $ mshape + pco.value [i] * pco.to.shape.2
  
## dimnames (pco.shape.2) [1:2] <- dimnames (Sym [[1]] $ sym) [1:2]

## for (i in 1:5)
##   {
##     coleurs <- colorRampPalette(c ('blue', 'red'))(5)
##     points3d(pco.shape.2 [, , i], rgl.open = ifelse (i == 1, T, F),
##              col = coleurs [i])
##     for (j in 1:(dim (Aux $ wireframe) [1]))
##       lines3d (allo.Data $ CAC.shape [Aux $ wireframe [j, ], , i], col = coleurs [i])
##   }

print (arrangeGrob(modcomp.Plots $ MI.Func + guides (size = FALSE),
                   modcomp.Plots $ RV.Func + scale_y_discrete(labels = NULL) + ylab(''),
                   ncol = 2, widths = c(1.3, 1)))

print (arrangeGrob(modcomp.Plots $ MI.Dev + guides (size = FALSE),
                   modcomp.Plots $ RV.NeuroFace, ncol = 2, widths = c(2, 1)))

modcomp.Plots $ RV.Func <- 
  ggplot (subset (modcomp.Data $ Summ, type == 'RV')) +
  geom_tile(aes(x = otu, y = hyp, fill = value)) +
  facet_grid(data ~ size) +
  theme_minimal() +
  scale_fill_continuous(name = 'RV', high = 'yellow', low = 'blue', space = 'Lab',
                        limits = c(0, 1), 
                        breaks = c(0.1, 0.5, 0.9)) +
  geom_point (aes (x = otu, y = hyp,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'RV Coefficent') +
  #scale_x_discrete(limits = rev(levels(modcomp.Data $ Summ $ otu))) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ hyp))) +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))

modcomp.Plots $ MI.Func <- 
  ggplot (subset (modcomp.Data $ Summ, type == 'MI')) +
  geom_tile(aes(y = hyp, x = otu, fill = value)) +
  facet_grid(data ~ size) +
  theme_minimal() +
  scale_fill_continuous(name = 'AVG Index',
                        high = 'blue', low = 'yellow', space = 'Lab',
                        breaks = c(-.3, 0, .3), limits = c(-.4, .4)) +
  geom_point (aes (y = hyp, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'AVG Index') +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  ##scale_x_discrete(limits = rev(levels(modcomp.Data $ Summ $ otu))) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ hyp))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0)) +
  guides (size = FALSE)

modcomp.Plots $ RV.NeuroFace <- 
  ggplot (subset (modcomp.Data $ Summ.Dev, type == 'RV')) +
  geom_tile(aes(y = data, x = otu, fill = value)) +
  facet_wrap(~ size, ncol = 2) +
  theme_minimal() +
  scale_fill_continuous (name = 'RV', high = 'yellow', low = 'blue', space = 'Lab',
                         limits = c(0, 1), breaks = c(0.1, 0.5, 0.9)) +
  geom_point (aes (y = data, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('') + xlab ('') + labs(title = 'Face/Neuro RV') +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ data))) +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0)) + guides(size = FALSE)

modcomp.Plots $ MI.Dev <- 
  ggplot (subset (modcomp.Data $ Summ.Dev, type == 'MI')) +
  geom_tile(aes(y = hyp, x = otu, fill = value)) +
  facet_grid(data ~ size) +
  theme_minimal() +
  scale_fill_continuous(name = 'AVG Index',
                        high = 'blue', low = 'yellow', space = 'Lab',
                        breaks = c(-.3, 0, .3), limits = c(-.4, .4)) +
  geom_point (aes (y = hyp, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'Face/Neuro AVG Index') +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ.Dev $ hyp))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))

