
sup.rs.riem <- post.ppca $ sup.rs.riem.mean
colnames (sup.rs.riem) [1] <- 'otus'

min.ss <- tbl_df (sup.rs.riem) %>%
  separate(otus, into = c("otu1", "otu2"), sep = "\\.") %$%
  aaply (cbind (otu1, otu2), 1, function (L) min (Aux $ sample.size [L]))


sup.rs.riem $ min.ss <- min.ss

head (sup.rs.riem)

gradient <- colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")

ppca.extra <- list ()

ppca.extra $ rs.riem <- 
ggplot (sup.rs.riem) +
  geom_point(aes (x = meanRiemDist, y = meanRS, color = min.ss)) +
  theme_bw() +
  scale_color_gradientn(name = 'Lower\nSample Size',
                        colours = gradient(100), trans = 'log',
                        breaks = c(25, 50, 100)) +
  xlab('Posterior Mean Riemannian Distance') +
  ylab('Posterior Mean Random Skewers')

ppca.extra $ ss.local1 <- 
  ggplot (data.frame (
    'otu' = rownames (etd.def.ppca $ li),
    'local1' = etd.def.ppca $ li [, 'PC108'],
    'ss' = Aux $ sample.size)) +
  geom_text(aes (x = ss, y = local1, label = otu), size = 2.5) +
  xlab(expression(paste(log[10], ' Sample Size'))) + ylab('Local 1') +
  scale_x_continuous(trans = 'log', breaks = c(20, 40, 80, 160)) +
  theme_bw()

ppca.shape <- list ()

ppca.Data $ post.arc %>%
  filter(Type == 'Local Shape Variables') %>%
  mutate('trait2' = factor (as.character(trait), levels = rownames (Aux $ def.hyp))) %>%
  group_by(axis, trait2) %>%
  summarise_each(funs(mean), value) %$%
  {
    for (i in 1:4)
      {
        current.value <- value [axis == levels (axis) [i]]
        ppca.shape [[i]] <<-
          ggshape(Reference [, , 'Macaca_mulatta'],
                  Aux $ single.tessel.38, current.value [-1],
                  palette = brewer.pal(10, 'Spectral'),
                  culo = 0.03, thickness = 1.5) +
                    xlab(gsub ('\\.', ' ', levels (axis) [i])) +
                      guides(color = guide_colorbar(title = 'SRD'),
                             fill = guide_colorbar(title = 'SRD'))
      }
  }

ppca.shape.comp <- plot_grid(plotlist = ppca.shape, ncol = 2)
