require (ggtree)
require (phylobase)

Tree.Plots <- list()

str (Tree [[1]])

#### Rect

tree.df <-
  phylo4(x = Tree [[1]] $ edge,
         edge.length = Tree [[1]] $ edge.length,
         tip.label = Tree [[1]] $ tip.label,
         node.label = 110:217,
         order = 'preorder')

taxo.list <- llply (dlply(allo.Data $ onedef.mean, .(taxo.group)),
                    function (L) L $ animal)


node.vec <- unlist (llply (taxo.list, getMRCA, phy = Tree [[1]]))

allo.tree <- ggtree(tree.df, aes (color = post.dev), size = 1.2)

for (i in 1:length (node.vec))
  allo.tree <-
  annotation_clade(allo.tree, node = node.vec [i], angle = 0, color = '#777777FF',
                   label = names (node.vec) [i], offset = 0.008, hjust = -0.01)

intercept.df <-
  data.frame (
    'taxa' = c(Tree [[1]] $ tip.label, 110:217),
    'post.dev' = allo.Data $ phylo.inter.df $ post.mean,
    'pMCMC' = as.numeric (allo.Data $ phylo.inter.df $ pMCMC),
    'sig' = ifelse(as.numeric (allo.Data $ phylo.inter.df $ pMCMC < 0.05), '*', ''))

slope.df <-
  data.frame (
    'taxa' = c(Tree [[1]] $ tip.label, 110:217),
    'post.dev' = allo.Data $ phylo.slope.df $ post.mean,
    'pMCMC' = as.numeric (allo.Data $ phylo.slope.df $ pMCMC),
    'sig' = ifelse(as.numeric (allo.Data $ phylo.slope.df $ pMCMC < 0.05), '*', ''))

gradient <- colorRampPalette(rev(brewer.pal(10, 'Spectral')), space = 'Lab')

Tree.Plots $ inter.tree <-
  allo.tree %<+% intercept.df +
  geom_point(aes(shape = sig), color = 'black', size = 6, alpha = 0.6) +
  scale_shape_identity() +
  scale_color_gradientn('Intercept\nDeviation', colours = gradient(10)) +
  scale_x_continuous(limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4)) +
  theme(legend.position = c(0.01, 0.75))

Tree.Plots $ slope.tree <-
  allo.tree %<+% slope.df +
  geom_point(aes(shape = sig), color = 'black', size = 6, alpha = 0.6) +
  scale_shape_identity() +
  scale_color_gradientn('Slope\nDeviation', colours = gradient(10)) +
  scale_x_continuous(limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4)) +
  theme(legend.position = c(0.01, 0.75))

Tree.Plots $ interslope <-
  plot_grid (Tree.Plots $ inter.tree, Tree.Plots $ slope.tree, nrow = 2,
             labels = c('a', 'b'), label_size = 32)

### models, sample.sizes

models.df <-
  data.frame (
    'taxa' = c(Tree [[1]] $ tip.label, node.vec),
    'tip' =  c(Tree [[1]] $ tip.label, names(node.vec)),
    'sample.size' = c(Aux $ sample.size, rep (NA, times = length(node.vec))),
    'model' = c(Aux $ data.man.sp [, 2], rep (NA, times = length(node.vec))))

models.df <-
  data.frame (
    'taxa' = Tree [[1]] $ tip.label,
    'tip' =  Tree [[1]] $ tip.label,
    'sample.size' = Aux $ sample.size, 
    'model' = Aux $ data.man.sp [, 2])


models.df $ model [30] <- 'SUB'

models.df $ model <- factor (as.character (models.df $ model))

levels (models.df $ model) <- c('-', 'Sex', 'Subspecies/Population',
                                'Sub/Pop * Sex', 'Sub/Pop + Sex')

models.df $ tip <- gsub ('_', ' ', models.df $ tip)

models.df $ tip <- paste (models.df $ tip, ' (', models.df $ sample.size, ')', sep = '')

model.tree <- ggtree(tree.df, color = 'black', size = 1, alpha = 0.8)

for (i in 1:length (node.vec))
  model.tree <- 
  annotation_clade(model.tree, node = node.vec [i], angle = 0, color = '#777777FF',
                   label = names (node.vec) [i], offset = 0.105, hjust = -0.01)

Tree.Plots $ model <-
  model.tree %<+% models.df +
  geom_text(aes(label = tip, color = model), size = 3, hjust = -0.01) +
  scale_color_brewer('Model', palette = 'Set1', na.value = 'black') +
  scale_x_continuous(limits = c(0, 0.6)) +
  theme(legend.position = c(0.05, 0.83))

### ppcs

ppca.df <- 
  data.frame (
    'ppc' = etd.def.ppca $ li [, c(1, 2, 4, 3)])

decdiv.df <-
  data.frame(
    'taxa' = 110:217,
    'div' = ppca.Data $ div.nodes.def * 100 / sum (ppca.Data $ div.nodes.def))

colnames(ppca.df) <- c('G1', 'G2', 'L1', 'L2')

div.tree <- ggtree(tree.df, color = 'black', size = 1.2, alpha = 0.2)

div.tree <-
  div.tree %<+% decdiv.df +
  geom_point(aes(size = div), alpha = 0.8, shape = 21, fill = 'grey')
  
### div.tree <- div.tree %<+% ppca.df

div.tree <-
  gheatmap(div.tree, ppca.df, colnames_position = 'top', offset = 0, width = 0.3) +
  scale_fill_gradientn(name = 'pPC\nValue',
                       colours = gradient(10)) +
  scale_size_continuous(name = 'Matrix\nDiversity',
                        range = c(1, 15), breaks = c(2, 5, 10, 15),
                        labels = paste0(c(2, 5, 10, 15), '%')) +
  guides(fill = guide_colorbar(),
         size = guide_legend()) +
  scale_x_continuous(limits = c(0, 0.7)) +
  theme(legend.position = 'right')

for (i in 1:length (node.vec))
  div.tree <- 
  annotation_clade(div.tree, node = node.vec [i], angle = 0, color = '#777777FF',
                   label = names (node.vec) [i], offset = 0.16, hjust = -0.01)

Tree.Plots $ ppca <- div.tree

save(Tree.Plots, file = 'Data/tree.plots.RData')

