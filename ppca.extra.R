
sup.rs.riem <- post.ppca $ sup.rs.riem.mean
colnames (sup.rs.riem) [1] <- 'otus'

min.ss <- tbl_df (sup.rs.riem) %>%
  separate(otus, into = c("otu1", "otu2"), sep = "\\.") %$%
  aaply (cbind (otu1, otu2), 1, function (L) min (Aux $ sample.size [L]))


sup.rs.riem $ min.ss <- min.ss

head (sup.rs.riem)

gradient <- colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")

ppca.extra <- list ()

sup.rs.riem <-
    sup.rs.riem %>%
    separate(., otus, into = c('otu1', 'otu2'), sep = '\\.')

load('ed.ppca.RData')

ed.ppca $ sup.rs.riem.mean $ type <- rep('Euclidean', times = nrow(ed.ppca $ sup.rs.riem.mean))
sup.rs.riem $ type <- rep('Marquez', times = nrow(sup.rs.riem))

colnames (sup.rs.riem)
colnames (ed.ppca $ sup.rs.riem.mean)

sup.rs.riem <- rbind(sup.rs.riem, ed.ppca $ sup.rs.riem.mean)

ppca.extra $ rs.riem <- 
                 ggplot (subset(sup.rs.riem, type == 'Marquez')) +
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

gradient <- colorRampPalette(brewer.pal(10, 'Spectral'))

ppca.Data $ post.arc %>%
  filter(Type == 'Local Shape Variables') %>%
  mutate('trait2' = factor (as.character(trait),
           levels = rownames (Aux $ def.hyp))) %>%
  group_by(axis, trait2) %>%
  summarise_each(funs(mean), value) %$%
  {
    for (i in 1:4)
      {
        current.value <- value [axis == levels (axis) [i]]
        ppca.shape [[i]] <<-
          ggshape(
            Reference [, , 'Cercopithecus_diana'],
            Aux $ single.tessel.38, current.value [-1],
            palette = brewer.pal(10, 'Spectral'), rotation = c(1, 1, 1),
            culo = 0.03, thickness = 1.5) +
              ggtitle(gsub ('\\.', ' ', levels (axis) [i]))

        if (i < 3)
          ppca.shape [[i]] <<-
            ppca.shape [[i]] +
              scale_color_gradientn(
                limits = c(0.75, 1), colours = gradient(10),
                breaks = c(0.8, 0.9, 1)) +
                  scale_fill_gradientn(
                    limits = c(0.75, 1), colours = gradient(10),
                    breaks = c(0.8, 0.9, 1))
                      
        else
          ppca.shape [[i]] <<-
            ppca.shape [[i]] +
              scale_color_gradientn(
                limits = c(0.98, 1), colours = gradient(10),
                breaks = c(0.98, 0.99, 1)) +
                  scale_fill_gradientn(
                    limits = c(0.98, 1), colours = gradient(10),
                    breaks = c(0.98, 0.99, 1))
        
        if (i %% 2 == 0)
          ppca.shape [[i]] <<-
            ppca.shape [[i]] +
              guides(color = guide_colorbar(title = ifelse(i == 2, 'Global', 'Local')),
                     fill = guide_colorbar(title = ifelse(i == 2, 'Global', 'Local')))
        else
          ppca.shape [[i]] <<- ppca.shape [[i]] +
            guides(color = FALSE, fill = FALSE)

      }
  }

ppca.shape.comp <- plot_grid(plotlist = ppca.shape, ncol = 2, rel_widths = c(1, 1.275))

ds.srd.pr <- post.ppca $ mean.srd.df

levels(ds.srd.pr $ hyp) [1] <- 'Size'

alt.spline <- 
  ggplot(ds.srd.pr) +
  geom_point(aes (x = pm, y = mean.trait, color = hyp),
                  alpha = 0.6) +
  facet_wrap(~ hyp, nrow = 4) +
  theme_bw() +
  scale_color_brewer(palette = 'Dark2') +
  scale_x_continuous(breaks = c(1, 31, 61, 91)) +
  xlab('pPC') + ylab('Posterior SRD Mean') +
  guides(color = guide_legend (ncol = 4), 
         fill = guide_legend (ncol = 4)) +
  theme(legend.position = "bottom") +
  guides(color = FALSE)

ds.srd.pr $ var <- rep (post.ppca $ var.moran.mean [, 2], each = 39)


var.ppc.srd <-
  ggplot(ds.srd.pr) +
  geom_point(aes (x = var, y = mean.trait, color = hyp),
             alpha = 0.6) +
  geom_smooth(aes(x = var, y = mean.trait, color = hyp, fill = hyp), method = 'lm') +
#  facet_wrap(~ hyp, nrow = 4) +
  theme_bw() +
  scale_color_brewer('Region', palette = 'Dark2') +
  scale_fill_brewer('Region', palette = 'Dark2') +
  scale_x_continuous(breaks = 10^c(-5, -3, -1), trans = 'log') +
  xlab('log pPC Variance') + ylab('Posterior SRD Mean') +
  guides(color = guide_legend (ncol = 4), 
         fill = guide_legend (ncol = 4)) +
  theme(legend.position = "bottom")

load('ed.ppca.RData')

ed.ppca $ sup.rs.riem.mean $ type <- rep('Euclidean', times = nrow(ed.ppca $ sup.rs.riem.mean))
sup.rs.riem $ type <- rep('Marquez', times = nrow(sup.rs.riem))

colnames (sup.rs.riem)
colnames (ed.ppca $ sup.rs.riem.mean)

rs.riem.comp.homo <- 
    ggplot (sup.rs.riem) +
    geom_point(aes (x = meanRiemDist, y = meanRS, color = min.ss)) +
    geom_point(aes (x = meanRiemDist, y = meanRS),
               subset(sup.rs.riem, grepl('Homo', otu1)),
               color = 'black', shape = '+', size = 5) +
    geom_point(aes (x = meanRiemDist, y = meanRS),
               subset(sup.rs.riem, grepl('Homo', otu2)),
               color = 'black', shape = '+', size = 5) +
    facet_wrap(~ type) +
    theme_bw() +
    scale_color_gradientn(name = 'Lower\nSample Size',
                          colours = gradient(100), trans = 'log',
                          breaks = c(25, 50, 100)) +
    xlab('Posterior Mean Riemannian Distance') +
    ylab('Posterior Mean Random Skewers')


ed.vs.marquez.rs <- data.frame('Marquez' = subset(sup.rs.riem, type == 'Marquez') $ meanRS,
                               'Euclidean' = subset(sup.rs.riem, type == 'Euclidean') $ meanRS,
                               'otu1' = subset(sup.rs.riem, type == 'Marquez') $ otu1,
                               'otu2' = subset(sup.rs.riem, type == 'Marquez') $ otu2,
                               'min.ss' = subset(sup.rs.riem, type == 'Marquez') $ min.ss)


ggplot(ed.vs.marquez.rs) +
    geom_point(aes(x = Euclidean, y = Marquez, color = min.ss)) +
    theme_bw() +
    scale_color_gradientn(name = 'Lower\nSample Size',
                          colours = gradient(100), trans = 'log',
                          breaks = c(25, 50, 100)) +
    geom_point(aes(x = Euclidean, y = Marquez),
               subset (ed.vs.marquez.rs, grepl('Homo', otu1)),
               shape = '+', size = 5) +
    geom_point(aes(x = Euclidean, y = Marquez),
               subset (ed.vs.marquez.rs, grepl('Homo', otu2)),
               shape = '+', size = 5) +
    geom_smooth(data = subset (ed.vs.marquez.rs, grepl('Homo', otu1) | grepl('Homo', otu2)),
                mapping = aes(x = Euclidean, y = Marquez),
                #formula = Marquez ~ Euclidean - 1,
                method = 'lm') +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')

confint(lm(Marquez ~ Euclidean - 1,
           subset (ed.vs.marquez.rs, grepl('Homo', otu1) | grepl('Homo', otu2))))
