### Parcimony Allo

write.csv(allo.Data $ integra.df [1:109, c('node', 'W.inter', 'W.slope')],
          row.names = FALSE, col.names = TRUE, file = 'Data/allo.csv', quote = FALSE)

write.nexus(Tree [[1]], file = 'Data/tree.nex')

allo.Parc <- data.frame (cbind (111:217, which (!(Tree [[1]] $ edge [, 2] %in% 1:109)) + 2),
                         read.csv('Data/allo_parc.csv') [-1, 2:3])

head (allo.Parc)

head (allo.Data $ model.slopes)

allo.Data $ model.slopes %>%
  filter(pos == 'Ancestor') %>%
  mutate('parc' = allo.Parc $ slope) %>%
  ggplot (.) +
  geom_text(aes (x = parc, y = post.mean, label = node, color = pMCMC < 0.05),
            size = 3) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')
  
allo.Data $ model.intercepts %>%
  filter(pos == 'Ancestor') %>%
  mutate('parc' = allo.Parc $ inter) %>%
  ggplot (.) +
  geom_text(aes (x = parc, y = post.mean, label = node, color = pMCMC < 0.05),
            size = 3) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')
