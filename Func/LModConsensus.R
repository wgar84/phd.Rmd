LModConsensus <- function (tree, node, W, parallel = FALSE)
  {
    subtree <- extract.clade (tree, node)
    otus <- dimnames (W) [[3]]
    otus.to.estimate <- otus %in% subtree $ tip.label
    corW.2go <- aaply (W [, , otus.to.estimate], cov2cor)
    lmod.2go <- alply (corW.2go, 1, LModularity, .parallel = parallel)
    lmod.values <- laply (lmod.2go, function (L) L [[1]])
    names (lmod.values) <- subtree $ tip.label
    function (LMod.out)
      outer (LMod.out [[2]], t(LMod.out [[2]]))
    
      
    
  }

PlotNetwork <- function (lmod.out, traits)
  {
    graph <- lmod.out [[2]]
    n.node <- length (traits)
    node.pos <- ((1:n.node) - 1) * 2 * pi / n.node
    plot(x = cos(node.pos), y = sin (node.pos), pch = 20, cex = 2)
    coleurs <- rainbow (ncol (graph))
    for (i in 1:ncol (graph))
      {
        hyp.mat <- graph [, i] %*% graph [, i]
        for (j in 1:(n.node-1))
          for (k in j:n.node)
            {
              if (hyp.mat [j,k] == 1)
                arrows(x0 = cos(node.pos [j]), y0 = sin (node.pos [j]),
                       x1 = cos(node.pos [k]), y1 = sin (node.pos [k]),
                       code = 3, length = 0, color = coleurs [i])
            }
      }
  }

