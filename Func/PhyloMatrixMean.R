PhyloMatrixMean <-
  function (terminal.matrix, tree) 
  {
    twoMatrixMean <- function (A, B)
      A %*% sqrtm (solve (A) %*% B)

    n.taxa <- length (tree $ tip.label)

    plot.phylo(tree, type = 'fan', show.tip.label = FALSE)

    tips.now <- 1:n.taxa

    out <- array (0, dim(terminal.matrix) + c(0, 0, n.taxa - 1))

    out [, , tips.now] <- terminal.matrix
    
    repeat
      {
        terminal.branch <- tree $ edge [, 2] %in% tips.now

        sub.nodes <- tree $ edge [terminal.branch, ]
    
        nodes.now <- unique (sub.nodes [, 1])

        to.compare <-
          alply (nodes.now, 1, function (i)
                 sub.nodes [sub.nodes [, 1] == i, 2])

        we.can.compare <- which (laply (to.compare, function (L) length (L) > 1))

        if (length (we.can.compare) <= 1)
          {
            out [, , nodes.now] <-
              twoMatrixMean(out [, , to.compare [[we.can.compare]] [1]],
                            out [, , to.compare [[we.can.compare]] [2]])
          }

        else
          {
            compare.now <-
              laply (to.compare [we.can.compare],
                     function (x) x)
            
            tips.now.out <- 
              aaply (compare.now, 1, function (ij) twoMatrixMean (out [, , ij [1]],
                                                                  out [, , ij [2]]))
          
            tips.now.out <- aperm (tips.now.out, c(2, 3, 1), resize = TRUE)
        
            out [, , nodes.now[we.can.compare]] <- tips.now.out

          }
        
        compare.later <-
              laply (to.compare [which (laply (to.compare,
                                               function (L) length (L) == 1))],
                     function (x) x)

        nodelabels('ok', nodes.now[we.can.compare], frame = 'none', col = 'blue')
        
        tips.now <- c(compare.later, nodes.now [we.can.compare])

        if (length (tips.now) == 1)
          break
      }
    out
  }
