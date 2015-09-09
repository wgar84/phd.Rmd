
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
  scale_color_gradientn(name = 'Sample\nSize',
                        colours = gradient(100), trans = 'log',
                        breaks = c(25, 50, 100)) +
  xlab('Posterior Mean Riemannian Distance') +
  ylab('Posterior Mean Random Skewers')

ppca.extra $ ss.local1 <- 
  ggplot (data.frame (
    'otu' = rownames (etd.def.ppca $ li),
    'local1' = etd.def.ppca $ li [, 'PC108'],
    'ss' = Aux $ sample.size)) +
  geom_text(aes (x = ss, y = local1, label = otu), size = 3) +
  xlab('Sample Size') + ylab('Local 1') +
  theme_bw()
