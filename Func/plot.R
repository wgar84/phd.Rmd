PrintSkullDef = function (skull, tesselation, double = TRUE, rgl.open = TRUE, mag = 10, deform)
  {
    require (shapes)
    x = 38
    lines = array (dim = c(2, x),
      data = c(
          'IS', 'PM',
          'IS', 'NSL',
          'IS', 'PNS',
          'PM', 'ZS',
          'PM', 'ZI',
          'PM', 'MT',
          'NSL', 'NA',
          'NSL', 'ZS',
          'NSL', 'ZI',
          'NA', 'BR',
          'NA', 'FM',
          'NA', 'PNS',
          'BR', 'PT',
          'BR', 'APET',
          'PT', 'FM',
          'PT', 'APET',
          'PT', 'BA',
          'PT', 'EAM',
          'PT', 'ZYGO',
          'FM', 'ZS',
          'FM', 'MT',
          'ZS', 'ZI',
          'ZI', 'MT',
          'ZI', 'ZYGO',
          'ZI', 'TSP',
          'MT', 'PNS',
          'PNS', 'APET',
          'APET', 'BA',
          'APET', 'TS',
          'BA', 'EAM',
          'EAM', 'ZYGO',
          'ZYGO', 'TSP',
          'LD', 'AS',
          'BR', 'LD',
          'OPI', 'LD',
          'PT', 'AS',
          'JP', 'AS',
          'BA', 'OPI'
        ))
    lines = t (lines)
    if (double)
      {
        lines.d = lines.e = lines
        lines.d = ifelse (lines.d == 'IS' | lines.d == 'NA' | lines.d == 'NSL' | lines.d == 'PNS' |
          lines.d == 'BR' | lines.d == 'BA' | lines.d == 'OPI' | lines.d == 'LD',
          lines.d, paste (lines.d, '-D', sep = ''))
        lines.e = ifelse (lines.e == 'IS' | lines.e == 'NA' | lines.e == 'NSL' | lines.e == 'PNS' |
          lines.e == 'BR' | lines.e == 'BA' | lines.e == 'OPI' | lines.e == 'LD',
          lines.e, paste (lines.e, '-E', sep = ''))
        lines = rbind (lines.d, lines.e)
      }
    on.skull = rownames (skull)
    links = array (match (lines, on.skull), dim (lines))
    print (links)
    skull.defpoints <- skull [tesselation, ]
    dim (skull.defpoints) <- c(dim (tesselation), ncol (skull))
    skull.defpoints <- apply (skull.defpoints, 1, colMeans)
    shapes3d (skull, rglopen = rgl.open)
    for (i in 1:dim (links)[1])
      {
        if (!any (is.na(links[i,])))
          shapes3d (skull[links[i,],], type = 'l', joinline = 1:2, rglopen = FALSE)
      }
    for (i in 1:dim (skull.defpoints) [2])
        spheres3d (skull.defpoints [1, i], skull.defpoints [2, i], skull.defpoints [3, i],
                   radius = deform [i] * mag, alpha = 0.75, 
                   color = ifelse (deform > 0,  'red', 'blue'))
    text3d (skull, text = dimnames (skull) [[1]], col = 'black')
  }


PlotShapeDeformation <- function (reference.shape, tesselation, magnify = 10,
                                  Balls = NULL, cor.mat = NULL, Colors = NULL,
                                  ColorsContinuous = NULL, 
                                  palette = colorRampPalette(c('red', 'green', 'blue'),
                                    space = 'Lab'), 
                                  Trans = NULL, view = c(1, 2), center = c(0, 0, 0),
                                  add = FALSE,
                                  scale = 1, landmark.names = FALSE,
                                  main = '', ylab = '', xaxt = 'n', yaxt = 'n',
                                  xlab = '', 
                                  graph.par = list(), ...)
  {
    require (scales)
    ## plot reference shape with deformation associated to tesselation
    Q <- reference.shape
    Lms <- rownames (Q)
    Right <- grep ('-D', Lms)
    Left <- grep ('-E', Lms)
    Midline <- !(Lms %in% c (Lms [Right], Lms [Left]))
    Q <- Q %*% Rotate2MidlineMatrix (Q, Lms [Midline])
    dimnames (Q) = dimnames (reference.shape)
    if (Q ['BR', 'Y'] < 0)
      Q [, 'Y'] <- -Q [, 'Y']
    if (all (view == c(1, 2)))
      Q <- Q %*% array (c(cos(pi/7), sin (pi/7), 0,
                          -sin (pi/7), cos(pi/7), 0, 0, 0, 1),
                        c(3, 3))
    Q <- t(t (Q * scale) + center)
    Q.tetra <- Q [tesselation, ]
    pts <- which (rownames (Q) %in% rownames (Q.tetra))
    dim (Q.tetra) <- c (dim (tesselation), ncol (Q))
    Q.centroids <- apply (Q.tetra, 1, colMeans)
    if (is.null (Trans))
      Trans <- rep (1, dim (Q.tetra) [1])
    if (!add)
      {
        do.call(par, graph.par)
        plot (Q [, view], col = 'white', asp = 1,
              main = main, ylab = ylab, xaxt = xaxt, yaxt = yaxt, xlab = xlab,...)
      }
    else
      points (Q [, view], col = 'white') 
    if (!is.null (cor.mat))
      for (i in 1:(dim (Q.centroids) [2] - 1))
        for (j in i:dim (Q.centroids) [2])
          if (Q.centroids [3, i] * Q.centroids [3, j] > 0)
            arrows (x0 = Q.centroids [view [1], i], y0 = Q.centroids [view [2], i],
                    x1 = Q.centroids [view [1], j], y1 = Q.centroids [view [2], j],
                    length = 0, lwd = abs (cor.mat) [i, j],
                    col = hsv (h = ifelse (cor.mat [i, j] > 0, 0, 0.75),
                      s = 1, v = abs (cor.mat [i, j]), alpha = abs (cor.mat [i, j])))
    for (i in 1:dim (Q.tetra) [1])
      lines (Q.tetra [i, c (1:dim (Q.tetra) [2], 1), view],
             col = rgb (0, 0, 0, Trans [i]))
    if (!is.null (Colors))
      {
        coleurs <- palette(length (unique (Colors)))
        for (i in 1:dim (Q.tetra) [1])
          {
            lines (Q.tetra [i, c (1:dim (Q.tetra) [2], 1), view],
                   col = alpha (coleurs [Colors] [i], Trans [i]), lwd = 3)
          }
      }
    if (!is.null (ColorsContinuous))
      {
        ab <- range(ColorsContinuous)
        ColorsContinuous <- (ColorsContinuous - ab [1]) / diff (ab)
        coleurs <- palette (ColorsContinuous)
        coleurs <- coleurs / 256
        colnames (coleurs) <- c('red', 'green', 'blue')
        coleurs <- aaply (coleurs, 1, function (V) do.call (rgb, as.list (V)))
        for (i in 1:dim (Q.tetra) [1])
          {
            lines (Q.tetra [i, c (1:dim (Q.tetra) [2], 1), view],
                   col = alpha (coleurs [i], Trans [i]), lwd = 3)
          }
      }
    if (!is.null (Balls))
      {
        for (i in 1:dim (Q.centroids) [2])
          points (x = Q.centroids [view [1], i], y = Q.centroids [view [2], i],
                  cex = magnify * abs (Balls [i]),
                  col = ifelse (Balls [i] > 0, rgb (1, 0, 0, 0.5), rgb (0, 0, 1, 0.5)),
                  pch = 20)
      }
    if(landmark.names)
    text (Q [pts, view], labels = gsub ('-D', '', rownames (Q) [pts]),
          pos = sample (1:4, length (pts), TRUE), cex = 0.6)
  }


Rotate2MidlineMatrix <- function (X, midline)
  {
    ## returns the rotation matrix that aligns a specimen saggital line
    ## to plane y = 0 (2D) or z = 0 (3D)
    ncl <- ncol (X) 
    Xm <- na.omit (X [midline, ])
    Mm <- matrix (apply (Xm, 2, mean), byrow = TRUE, nr = nrow (X), nc = ncl)
    Xc <- X - Mm 
    W <- na.omit (Xc [midline, ])
    RM <-svd (var (W))$v
    return (RM)
  }

ggshape <- function (shape, wireframe, colors, view = c(1, 2, 3),
                     rotation = c(1, 1, 1), culo = 0.015, thickness = 3,
                     palette = rev (brewer.pal (10, 'Spectral')))
  {
    Q <- shape
    Lms <- rownames (Q)
    Right <- grep ('-D', Lms)
    Left <- grep ('-E', Lms)
    Midline <- !(Lms %in% c (Lms [Right], Lms [Left]))
    Q <- Q %*% Rotate2MidlineMatrix (Q, Lms [Midline]) 
    Q <- Q %*% diag (rotation)
    if (all (view == c(1, 2, 3)))
      Q <- Q %*%
        array (c(cos(pi/7), sin (pi/7), 0,
                 -sin (pi/7), cos(pi/7), 0,
                 0, 0, 1), c(3, 3))
    colnames (Q) <- c('X', 'Y', 'Z')
    ## pts <- which (rownames (Q) %in% rownames (Q.tetra))
    Q <- Q [, view]
    colnames (Q) <- c('X', 'Y', 'Z')
    Q.tetra <- Q [wireframe, ]
    dim (Q.tetra) <- c (dim (wireframe), ncol (Q))
    Q.names <- array (rownames (Q) [wireframe], dim = dim (wireframe))
    Q.names <- apply (Q.names, 1, paste, collapse = '.')
    dimnames (Q.tetra) <- list('ild' = Q.names,
                               'pos' = c(1, 2),
                               'dim' = c('X', 'Y', 'Z'))

    Q.singular <- which (!duplicated (dimnames (Q.tetra) [[1]]))

    Q.colors <- rep (colors, times = 2) [Q.singular]

    Q.tetra <- Q.tetra [Q.singular, , ]

    Q.tetra.df <- dcast (melt (Q.tetra), ild ~ dim + pos)

    Q.tetra.df $ culo <- rep (culo, nrow (Q.tetra))

    #Q.tetra.df $ Z_1 <-
    #  (Q.tetra.df $ Z_1 - min (Q.tetra.df $ Z_1)) /
    #    (max (Q.tetra.df $ Z_1) - min (Q.tetra.df $ Z_1))

    #Q.tetra.df $ Z_2 <-
    #  (Q.tetra.df $ Z_2 - min (Q.tetra.df $ Z_2)) /
    #    (max (Q.tetra.df $ Z_2) - min (Q.tetra.df $ Z_2))
   
    Q.line.df <-
      plyr::ddply (Q.tetra.df, .(ild), plyr::summarise,
                   'X' = c(
                     X_1 - Z_1 * culo,
                     X_1 + Z_1 * culo,
                     X_2 + Z_2 * culo,
                     X_2 - Z_2 * culo),
                   'Y' = c(
                     Y_1,
                     Y_1,
                     Y_2,
                     Y_2))
    
    Q.line.df $ color <- rep (Q.colors, each = 4)
  
    shape.plot <-
      ggplot (data.frame(Q)) +
        geom_point (aes (x = X, y = Y), alpha = 0) +
          coord_fixed() +
            theme_minimal() +
              theme(plot.margin = unit(c(0, 0, 0, 0), 'cm')) +
                scale_x_continuous(breaks = NULL) +
                  scale_y_continuous(breaks = NULL) +
                    guides(size = FALSE) +
                      xlab('') + ylab('')

    shape.plot <-
      shape.plot +
        geom_polygon(aes (x = X, y = Y, group = ild, 
                          color = color, fill = color), Q.line.df, size = thickness)
    spec.pal <- colorRampPalette (palette, space = 'Lab')
    shape.plot +
      scale_color_gradientn('', colours = spec.pal(10)) +
        scale_fill_gradientn('', colours = spec.pal(10))
  }
