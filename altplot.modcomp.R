alt.dist.sim <- 
ggplot (modsim.Data $ values.df) +
  geom_boxplot(aes(x = interaction (variable, type, sep = ' - '), y = value,
                   fill = interaction (variable, type, sep = ' - '),
                   color = interaction (variable, type, sep = ' - ')),
               alpha = 0.5, position = 'identity',
               outlier.shape = '+', outlier.size = 1) +
  facet_grid(size ~ stat, scales = 'free') +
  scale_fill_brewer(name = 'Simulated Matrix Type', palette = 'Paired') +
  scale_color_brewer(name = 'Simulated Matrix Type', palette = 'Paired') +
  ylab('Value') + xlab ('') +
  scale_x_discrete(breaks = NULL) +
  theme_bw() 

myPalette.RV <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
myPalette.AVG <- colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")

modcomp.Plots $ RV.Func <- 
  ggplot (subset (modcomp.Data $ Summ, type == 'RV')) +
  geom_tile(aes(x = otu, y = hyp, fill = value)) +
  facet_grid(data ~ size) +
  theme_bw() +
  scale_fill_gradientn(name = 'RV', colours = myPalette.RV(100), 
                       limits = c(0, 1), 
                       breaks = c(0.1, 0.5, 0.9)) +
  geom_point (aes (x = otu, y = hyp,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'RV Coefficent') +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ hyp))) +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))

modcomp.Plots $ MI.Func <- 
  ggplot (subset (modcomp.Data $ Summ, type == 'MI')) +
  geom_tile(aes(y = hyp, x = otu, fill = value)) +
  facet_grid(data ~ size) +
  theme_bw() +
  scale_fill_gradientn(name = 'AVG Index',
                        colours = myPalette.AVG(100), 
                        breaks = c(-.3, 0, .3), limits = c(-.4, .4)) +
  geom_point (aes (y = hyp, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'AVG Index') +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  ##scale_x_discrete(limits = rev(levels(modcomp.Data $ Summ $ otu))) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ hyp))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0)) +
  guides (size = FALSE)

modcomp.Plots $ RV.NeuroFace <- 
  ggplot (subset (modcomp.Data $ Summ.Dev, type == 'RV')) +
  geom_tile(aes(y = data, x = otu, fill = value)) +
  facet_wrap(~ size, ncol = 2) +
  theme_bw() +
  scale_fill_gradientn (name = 'RV',
                        colours = myPalette.RV(100),
                        limits = c(0, 1), breaks = c(0.1, 0.5, 0.9)) +
  geom_point (aes (y = data, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
                  labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('') + xlab ('') + labs(title = 'Face/Neuro RV') +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ $ data))) +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0)) + guides(size = FALSE)

modcomp.Plots $ MI.Dev <- 
  ggplot (subset (modcomp.Data $ Summ.Dev, type == 'MI')) +
  geom_tile(aes(y = hyp, x = otu, fill = value)) +
  facet_grid(data ~ size) +
  theme_bw() +
  scale_fill_gradientn(name = 'AVG Index',
                       colours = myPalette.AVG(100),
                       breaks = c(-.3, 0, .3), limits = c(-.4, .4)) +
  geom_point (aes (y = hyp, x = otu,
                   size = (p < 0.05) + (p < 0.01) + (p < 0.001),
                   alpha = c(0, 1) [1 + (p < 0.05)]), shape = 21) +
  scale_size_area(name = expression(P(alpha)),
labels = c('< 0.05', '< 0.01', '< 0.001'),
                  breaks = c(1, 2, 3)) +
  ylab ('Hypothesis') + xlab ('') + labs(title = 'Face/Neuro AVG Index') +
  scale_alpha_continuous(limits = c(0, 1)) + guides(alpha = FALSE) +
  scale_y_discrete(limits = rev(levels(modcomp.Data $ Summ.Dev $ hyp))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0))

plot_grid(modcomp.Plots $ MI.Func +
          theme(text = element_text(size = 16),
                axis.text = element_text(size = 18)),
          modcomp.Plots $ RV.Func +
          theme(text = element_text(size = 16),
                axis.text = element_text(size = 18)),
          nrow = 2, labels = c('a', 'b'), label_size = 24)

modsim.Plots $ cor.dist <- 
  ggplot (modsim.Data $ cor.wb.df) +
  geom_boxplot (aes (y = value, x = wb), outlier.shape = '+') +
  facet_grid(size ~ type) + theme_bw() +
  xlab('Correlation Type') + ylab('Correlation Value')
