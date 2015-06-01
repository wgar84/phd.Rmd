Bslope <- function (post.sample, cs, tree, node = NA)
  {
    post.sample <- allo.Data $ allo.mcmc.randomreg $ Sol [2, ]
    post.table <- cbind (post.sample [grepl ('Intercept', names (post.sample))], 
                     post.sample [grepl ('logCS', names (post.sample))])

    post.names <-
      laply(strsplit(names (post.sample) [grep('logCS', names (post.sample))], 'animal.'),
            function (L) L[length (L)])

    rownames (post.table) <- post.names
    rownames (post.table) [1] <- 'Mean'
    colnames (post.table) <- c('intercept', 'slope')

    post.nodes <-
      as.numeric (laply (strsplit(post.names [grep ('Node', post.names)], 'Node'),
                         function (L) L [2])) + 109

    node.is <- post.table[grep ('Node', post.names), ]
    node.is <- cbind(post.nodes, node.is)

    term.is <- post.table[!grepl ('Node', post.names), ] [-1, ]

    cs.order <- cs [match (rownames (term.is), rownames (cs)), 'mCS']

    mean.is <- post.table ['Mean', ]
    term.is <- cbind (cs.order, term.is)

    if (!is.na(node))
      {
        tree <- extract.clade(tree, node)
        mean.is <- mean.is + node.is [node.is[, 'post.nodes'] == node, -1]
        term.is <- term.is [rownames (term.is) %in% tree $ tip.label, ]
      }

    term.is <- data.frame (term.is)

    term.is $ intercept <- term.is $ intercept + mean.is ['intercept']
    term.is $ slope <- term.is $ slope + mean.is ['slope']
    
    term.is $ static.term <-
      term.is $ intercept +
        term.is $ slope *
          term.is $ cs.order
    
    b.slope <-
      var(term.is $ static.term, term.is $ cs.order) / var (term.is $ cs.order)
    
    out <- c(mean.is ['slope'], b.slope)
    names (out) <- c('W', 'B')
    return (out)
  }
