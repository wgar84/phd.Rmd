anthro.normalized <- t (solve (sqrtm (allo.Data $ W.node.Def [['Anthropoidea']])) %*%
                        t(laply (OneDef, function (L) L $ mean)))

anthro.pca <- prcomp (anthro.normalized)

anthro.pc.df <- data.frame ('OTU' = names (OneDef), anthro.pca $ x)

ggplot (anthro.pc.df) +
  geom_text (aes(x = PC1, y = PC2, label = OTU)) +
  theme_bw()

pan.normalized <- t (solve (sqrtm (allo.Data $ W.all.Def [['Homo_sapiens']])) %*%
                        t(laply (OneDef, function (L) L $ mean)))

pan.pca <- prcomp (pan.normalized)

pan.pc.df <- data.frame ('OTU' = names (OneDef), pan.pca $ x)

pan.pc.df $ PC1 <- - pan.pc.df $ PC1

plot_grid (
  ggplot (pan.pc.df) +
  geom_text (aes(x = PC1, y = PC2, label = OTU), size = 3) +
  theme_bw(),
  ggplot (anthro.pc.df) +
  geom_text (aes(x = PC1, y = PC2, label = OTU), size = 3) +
  theme_bw(), nrow = 1)

dimnames (anthro.normalized) <- dimnames (pan.normalized) <-
  list (names (OneDef), rownames (Aux $ def.hyp))

pan.df <-
  melt (as.matrix (dist (pan.normalized))) [which (lower.tri (diag (109))), ]

anthro.df <-
  melt (as.matrix (dist (anthro.normalized))) [which (lower.tri (diag (109))), ]

compare.df <- data.frame (anthro.df, 'pan' = pan.df $ value)

compare.df $ Homo_ <- 
  ifelse(grepl('Homo', compare.df $ Var1) |
         grepl('Homo', compare.df $ Var2) |
         grepl('Pan', compare.df $ Var1) |
         grepl('Pan', compare.df $ Var2) |
         grepl('Gorilla', compare.df $ Var1) |
         grepl('Gorilla', compare.df $ Var2),
         TRUE, FALSE)

colnames (compare.df)

mean.evol.ratio <-
  MeanMatrixStatistics(allo.Data $ W.node.Def [['Homo+Pan+Gorilla']])['evolvability'] /
  MeanMatrixStatistics(allo.Data $ W.node.Def [['Anthropoidea']])['evolvability']
                     

ggplot (compare.df) +
  geom_point(aes(x = value, y = pan, alpha = Homo_)) +
  theme_bw() +
  geom_abline(intercept = 0, slope = sqrt (mean.evol.ratio))


random.walk <- sim.char(Tree [[1]],
                        par = allo.Data $ W.node.Def [['Anthropoidea']],
                        model = 'BM',
                        root = 0)

random.walk.norm <- t (solve (sqrtm (allo.Data $ W.node.Def [['Anthropoidea']])) %*%
                       t (random.walk [, , 1]))

random.walk.pca <- prcomp (random.walk.norm) $ x

ggplot (data.frame('OTU' = names (OneDef),
                   random.walk.pca)) +
  geom_text (aes(x = PC1, y = PC2, label = OTU), size = 3) + coord_fixed() +
  theme_bw()
