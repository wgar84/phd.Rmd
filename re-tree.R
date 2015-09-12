require (ggtree)
require (phylobase)

Tree.Plots <- list()

str (Tree [[1]])

tree.df <-
  phylo4(x = Tree [[1]] $ edge,
         edge.length = Tree [[1]] $ edge.length,
         tip.label = Tree [[1]] $ tip.label,
         node.label = 110:217,
         order = 'preorder')

taxo.list <- llply (dlply(allo.Data $ onedef.mean, .(taxo.group)),
                    function (L) L $ animal)

### basic tree
inter.tree <- ggtree(tree.df, aes (color = group), size = 1.2, alpha = 0.7, layout = 'fan')
inter.tree <- groupOTU(inter.tree, taxo.list)
inter.tree <- inter.tree +
  scale_color_manual('Group', values = c('lightgrey', gradient(10)),
                     breaks = 1:10, labels = names (taxo.list))

### intercept
intercept.df <-
  data.frame (
    'taxa' = c(Tree [[1]] $ tip.label, 110:217),
    'post.dev' = allo.Data $ phylo.inter.df $ post.mean,
    'pMCMC' = as.numeric (allo.Data $ phylo.inter.df $ pMCMC),
    'abs.post.dev' = abs (allo.Data $ phylo.inter.df $ post.mean),
    'signal.post.dev' = allo.Data $ phylo.inter.df $ post.mean /
    abs (allo.Data $ phylo.inter.df $ post.mean * 2) + 21.5,
    'sig' = ifelse(as.numeric (allo.Data $ phylo.inter.df $ pMCMC) < 0.05, 'S', ''))


Tree.Plots $ intercept <-
  inter.tree %<+% intercept.df +
  geom_point(aes(color = NULL, shape = signal.post.dev, size = abs.post.dev * 5),
             alpha = 0.6) +
  geom_text(aes (label = sig, color = NULL), size = 2.5, alpha = 0.7) +
  scale_shape_identity('Direction', breaks = c(21, 22),
                       labels = c('Decrease', 'Increase'), guide = 'legend') +
  scale_size_identity('Magnitude', guide = 'legend', breaks = c(2.5, 5, 7.5, 10),
                      labels = c(2.5, 5, 7.5, 10)/5)
             
### slope
slope.df <-
  data.frame ('taxa' = c(Tree [[1]] $ tip.label, 110:217),
              'post.dev' = allo.Data $ phylo.slope.df $ post.mean,
              'pMCMC' = as.numeric (allo.Data $ phylo.slope.df $ pMCMC),
              'abs.post.dev' = abs (allo.Data $ phylo.slope.df $ post.mean),
              'signal.post.dev' = allo.Data $ phylo.slope.df $ post.mean /
              abs (allo.Data $ phylo.slope.df $ post.mean * 2) + 21.5,
              'sig' = ifelse(as.numeric (allo.Data $ phylo.slope.df $ pMCMC) < 0.05,
                'S', ''))

Tree.Plots $ slope <-
  inter.tree %<+% slope.df +
  geom_point(aes(color = NULL, shape = signal.post.dev, size = abs.post.dev * 5),
             alpha = 0.6) +
  geom_text(aes (label = sig, color = NULL), size = 2.5, alpha = 0.7) +
  scale_shape_identity('Direction', breaks = c(21, 22),
                       labels = c('Decrease', 'Increase'), guide = 'legend') +
  scale_size_identity('Magnitude', guide = 'legend', breaks = c(2.5, 5, 7.5, 10),
                      labels = c(2.5, 5, 7.5, 10)/5)

Tree.Plots $ interslope <-
  plot_grid(
    Tree.Plots $ intercept +
    guides(shape = FALSE, size = FALSE,
           color = guide_legend(ncol = 2, order = 3)) +
    theme(legend.position = 'bottom', legend.direction = 'vertical',
          legend.box = 'horizontal'),
    Tree.Plots $ slope +
    guides(shape = guide_legend(order = 1),
           size = guide_legend(order = 2),
           color = FALSE) +
    theme(legend.position = 'bottom', legend.direction = 'vertical',
          legend.box = 'horizontal'),
    nrow = 1, labels = c('a', 'b'), label_size = 32)
    

save(Tree.Plots, file = 'Data/tree.plots.RData')
