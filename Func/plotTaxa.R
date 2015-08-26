Plot.taxa <- function (names = TRUE)
  {
    plotcircle(r = 0.46, from = 0, to = 9 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 10 * (2 * pi / 109), to = 19 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 20 * (2 * pi / 109), to = 25 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 26 * (2 * pi / 109), to = 37 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 38 * (2 * pi / 109), to = 39 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 40 * (2 * pi / 109), to = 45 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 46 * (2 * pi / 109), to = 50 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 51 * (2 * pi / 109), to = 69 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 70 * (2 * pi / 109), to = 87 * (2 * pi / 109), lwd = 6)
    plotcircle(r = 0.46, from = 88 * (2 * pi / 109), to = 108 * (2 * pi / 109), lwd = 6)
    if (names)
      {
        text(x = 0.53 * mean (cos (c(0, 9 * (2 * pi / 109)))),
             y = 0.51 * mean (sin (c(0, 9 * (2 * pi / 109)))),
             labels = 'Pit', cex = 1.5)
        text(x = 0.57 * mean (cos (c(10, 19) * (2 * pi / 109))),
             y = 0.54 * mean (sin (c(10, 19) * (2 * pi / 109))) - 0.03,
             labels = 'Ate', cex = 1.5)
        text(x = 0.53 * mean (cos (c(20, 25) * (2 * pi / 109))) + 0.04,
             y = 0.51 * mean (sin (c(20, 25) * (2 * pi / 109))),
             labels = 'Ceb', cex = 1.5)
        text(x = 0.485 * mean (cos (c(26, 37) * (2 * pi / 109))) - 0.05,
             y = 0.485 * mean (sin (c(26, 37) * (2 * pi / 109))), pos = 3, 
             labels = 'Cal', cex = 1.5)
        text(x = 0.495 * mean (cos (c(38, 39) * (2 * pi / 109))) - 0.03,
             y = 0.52 * mean (sin (c(38, 39) * (2 * pi / 109))) - 0.01,
             labels = 'Aot', cex = 1.5)
        text(x = 0.485 * mean (cos (c(40, 45) * (2 * pi / 109))) + 0.02,
             y = 0.485 * mean (sin (c(40, 45) * (2 * pi / 109))), pos = 2,
             labels = 'Hom', cex = 1.5)
        text(x = 0.485 * mean (cos (c(46, 50) * (2 * pi / 109))) + 0.02,
             y = 0.485 * mean (sin (c(46, 50) * (2 * pi / 109))), pos = 2,
             labels = 'Hyl', cex = 1.5)
        text(x = 0.465 * mean (cos (c(61, 69) * (2 * pi / 109))) - 0.04,
             y = 0.475 * mean (sin (c(61, 69) * (2 * pi / 109))), pos = 2,
             labels = 'Pap', cex = 1.5)
        text(x = 0.485 * mean (cos (c(70, 87) * (2 * pi / 109))),
             y = 0.485 * mean (sin (c(70, 87) * (2 * pi / 109))) - 0.05, pos = 1,
             labels = 'Cer', cex = 1.5)
        text(x = 0.485 * mean (cos (c(88, 108) * (2 * pi / 109))) + 0.05,
             y = 0.485 * mean (sin (c(88, 108) * (2 * pi / 109))) - 0.035, pos = 4,
             labels = 'Col', cex = 1.5)
      }
  }

taxa.legend <- function ()
  'Platyrrhini: **Pit**hecidae, **Ate**lidae, **Ceb**inae, **Cal**lithrichinae, and **Aot**inae; Catarrhini: **Hom**inidae, **Hyl**obatidae, **Pap**ionini, **Cer**copithecini, and **Col**obinae'
