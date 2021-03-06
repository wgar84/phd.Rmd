MeanMatrix <- function (matArray, tol = 1e-14, max.steps = 100, parallel = FALSE)
  {
      ### Computes (geometric) mean matrix using gradient optimization
      ### in the manifold of PD-Sym matrices (from Woods, 2003)
      logm.single <- function (Ai, inv.Mk) return (logm (Ai %*% inv.Mk))
      A <- matArray
      N <- dim (A) [3]
      Mk <- diag (nrow (A))
      i <- 1
      repeat
          {
              inv.Mk <- solve (Mk)
              centered.now <- aaply (A, 3, logm.single, inv.Mk = inv.Mk,
                                     .parallel = parallel)
              centered.now <- aperm(centered.now, c(2, 3, 1))
              o <- array (- rowMeans (centered.now), c(nrow (A), nrow (A)))
              frob.norm.o <- FrobNorm (o)
              print (frob.norm.o)
              if (frob.norm.o < tol)
                  break
              Mk <- expm (- o) %*% Mk
              if (i == max.steps)
                  stop ('Convergence has not been achieved in number of steps.')
              i <- i + 1
          }
      return (Mk)
  }



InductiveMean <- function (mat.array)
  {
    Lambda.t <- function (A, B, t = 0.5)
      {
        sqrt.A <- sqrtm (A)
        is.A <- solve (sqrt.A)
        sqrt.A %*% expm (t * logm (is.A %*% B %*% is.A)) %*% sqrt.A        
      }
    n.mat <- dim (mat.array) [3]
    S <- mat.array [, , 1]
    for (i in 2:n.mat)
      {
        print (i)
        S <- Lambda.t(S, mat.array [, , i], t = 1/i)
      }
    S
  }

VanishingGradientTest <- function (mat.array, mean.mat, parallel = FALSE)
  {
    is.X <- solve (sqrtm (mean.mat))
    sum (aaply(mat.array, 3,
               function (A)
               {
                 logm (is.X %*% A %*% is.X)
               }, .parallel = parallel))
  }
