ModIndex.Shape <- function (cov.ss, mod.hyp, remove.size = FALSE, ...)
  {
    icv <- ICV.Shape (cov.ss) [remove.size + 1]
    if (remove.size)
      cov.shape <- RemoveCAC(cov.ss)
    else
      {
        logCS.pos <- which (rownames (cov.ss) == 'logCS')
        cov.shape <- cov.ss[- logCS.pos, - logCS.pos]
      }
    cor.shape <- cov2cor (cov.shape)
    mod.test <- TestModularity (cor.shape, mod.hyp, ...)
    mod.index <- (mod.test [, 'AVG+'] - mod.test [, 'AVG-']) / icv
    out <- cbind (mod.test, mod.index)
    colnames (out) <- c (colnames (mod.test), 'ModIndex')
    return (out)
  }

ICV.Shape <- function (cov.ss)
  {
    logCS.pos <- which(rownames(cov.ss) == 'logCS')
    cov.shape <- cov.ss [- logCS.pos, - logCS.pos]
    eval.shape <- eigen (cov.shape) $ values
    icv.shape <- sd (eval.shape) / mean (eval.shape)
    cov.res <- RemoveCAC(cov.ss)
    eval.res <- eigen (cov.res) $ values
    icv.res <- sd (eval.res) / mean (eval.res)
    c(icv.shape, icv.res)
  }

RemoveCAC <- function (cov.ss)
  {
    logCS.pos <- which(rownames(cov.ss) == 'logCS')
    cov.shape <- cov.ss [- logCS.pos, - logCS.pos]
    CAC <- Normalize (cov.ss [logCS.pos, -logCS.pos] / cov.ss [logCS.pos, logCS.pos])
    Iaa <- diag (nrow (cov.shape)) - (CAC %*% t (CAC))
    cov.res <- t(Iaa) %*% cov.shape %*% Iaa
    cov.res
  }

ModIndex <- function (cov.matrix, mod.hyp, remove.size = FALSE, ...)
  {
    cor.matrix <- cov2cor (cov.matrix)
    if (remove.size)
      {
        cov.matrix <- RemoveSize (cov.matrix)
        cor.matrix <- RemoveSize (cor.matrix)
      }
    icv <- ICV (cov.matrix)
    mod.test <- TestModularity (cor.matrix, mod.hyp, ...)
    mod.index <- (mod.test [, 'AVG+'] - mod.test [, 'AVG-']) / icv
    out <- cbind (mod.test, mod.index)
    colnames (out) <- c (colnames (mod.test), 'ModIndex')
    return (out)
  }

ICV <- function (cov.matrix)
  {
    cov.res <- RemoveSize (cov.matrix)
    eval <- eigen (cov.matrix) $ values
    ICV.raw <- sd (eval) / mean (eval)
    eval.res <- eigen (cov.res) $ values
    ICV.res <- sd (eval.res) / mean (eval.res)
    c (ICV.raw, ICV.res)
  }
