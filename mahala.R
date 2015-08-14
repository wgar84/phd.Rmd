require(StatMatch)

mahala.shape <-
  cmdscale (
    as.dist (
      mahalanobis.dist(laply (OneDef, function (L) L $ mean [-1]),
                       vc = allo.Data $ W.node.Def [['Anthropoidea']] [-1, -1])))

mahala.shape <-
  data.frame ('OTU' = names (OneDef),
              'pco' = mahala.shape)

ggplot(mahala.shape) +
  geom_text (aes(x = pco.1, y = pco.2, label = OTU))

sqrt (Evolvability(solve (allo.Data $ W.node.Def [['Anthropoidea']]) %*%
                   (OneDef [[108]] $ mean - OneDef [[109]] $ mean),
                   allo.Data $ W.node.Def [['Anthropoidea']]))

as.dist (
  mahalanobis.dist(laply (OneDef, function (L) L $ mean),
                   vc = allo.Data $ W.node.Def [['Platyrrhini']]))

mahala.ed <-
  cmdscale (
    as.dist (
      mahalanobis.dist(laply (ED, function (L) L $ ed.mean) [, -20],
                       vc = allo.Data $ W.node.ED [['Anthropoidea']])))

mahala.ed <-
  data.frame ('OTU' = names (OneDef),
              'pco' = mahala.ed)

ggplot(mahala.ed) +
  geom_text (aes(x = pco.1, y = pco.2, label = OTU))

sqrt (Evolvability(solve (allo.Data $ W.node.Def [['Anthropoidea']]) %*%
                   (OneDef [[108]] $ mean - OneDef [[109]] $ mean),
                   allo.Data $ W.node.Def [['Anthropoidea']]))

as.dist (
  mahalanobis.dist(laply (OneDef, function (L) L $ mean),
                   vc = allo.Data $ W.node.Def [['Platyrrhini']]))


sym.mean <- laply (Sym, function (L) L $ sym.mean)
sym.mean <- aperm (sym.mean, c(2, 3, 1))
sym.gpa <- procGPA(sym.mean, pcaoutput = FALSE)

tan <- t (sym.gpa $ tan)

pco.to.shape.1 <- coef (lm (tan ~ mahala.shape [, 2])) [2, ]
pco.to.shape.2 <- coef (lm (tan ~ mahala.shape [, 3])) [2, ]

Norm(pco.to.shape.1)

dim (pco.to.shape.1) <- c(36, 3)
dim (pco.to.shape.2) <- c(36, 3)

pco.shape.1 <- array (0, c(36, 3, 5))
pco.value <- c(-2, -1, 0, 1, 2)
for (i in 1:5)
  pco.shape.1 [, , i] <-
  allo.Data $ sym.gpa $ mshape + pco.value [i] * pco.to.shape.1
  
dimnames (pco.shape.1) [1:2] <- dimnames (Sym [[1]] $ sym) [1:2]

for (i in 1:5)
  {
    coleurs <- colorRampPalette(c ('blue', 'red'))(5)
    points3d(pco.shape.1 [, , i], rgl.open = ifelse (i == 1, T, F),
             col = coleurs [i])
    for (j in 1:(dim (Aux $ wireframe) [1]))
      lines3d (allo.Data $ CAC.shape [Aux $ wireframe [j, ], , i], col = coleurs [i])
  }

pco.shape.2 <- array (0, c(36, 3, 5))
pco.value <- c(-2, -1, 0, 1, 2)
for (i in 1:5)
  pco.shape.2 [, , i] <-
  allo.Data $ sym.gpa $ mshape + pco.value [i] * pco.to.shape.2
  
dimnames (pco.shape.2) [1:2] <- dimnames (Sym [[1]] $ sym) [1:2]

for (i in 1:5)
  {
    coleurs <- colorRampPalette(c ('blue', 'red'))(5)
    points3d(pco.shape.2 [, , i], rgl.open = ifelse (i == 1, T, F),
             col = coleurs [i])
    for (j in 1:(dim (Aux $ wireframe) [1]))
      lines3d (allo.Data $ CAC.shape [Aux $ wireframe [j, ], , i], col = coleurs [i])
  }
