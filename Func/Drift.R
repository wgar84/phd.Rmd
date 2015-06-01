singleDrift <- function (W.mat, terminal)
  {
    options(contrasts = c('contr.sum', 'contr.poly'))
    m <- nrow (terminal)
    k <- ncol (terminal)
    W.eig.dec <- eigen (W.mat)
    if (k >= m)
      {
        W.pc <- W.eig.dec $ vectors [, 1:(m-1)]
        var.W <- W.eig.dec $ values [1:(m-1)]
      }
    else
      {
        W.pc <- W.eig.dec $ vectors
        var.W <- W.eig.dec $ values
      }
    B.on.W <- terminal %*% W.pc ### B.on.W será m x m - 1 ou m x k
    ### variance
    var.B.on.W <- aaply (B.on.W, 2, var)
    reg.test <- lm (log (var.B.on.W) ~ log (var.W))
    var.df <- data.frame (1:length (var.W), log (var.B.on.W), log (var.W))
    names (var.df) <- c('PC', 'B', 'W')
    ### correlation
    cor.B.on.W <- cor (B.on.W)
    cor.test <- cortest.bartlett (cor.B.on.W, n = m)
    return (list ('reg' = reg.test, 'cor' = cor.test, 'mat' = cor.B.on.W,
                  'var.df' = var.df))
  }
