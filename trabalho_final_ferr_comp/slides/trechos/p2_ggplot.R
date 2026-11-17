p2_espalha <- position_jitterdodge(jitter.width = 0.18,
                                   dodge.width = 0.7, seed = 1)

p2_g1 <- ggplot(p2_prog, aes(x = nota, y = pct, color = area)) +
  geom_point(position = p2_espalha, size = 1.9, alpha = 0.55) +
  geom_errorbar(data = p2_conj, aes(ymin = pct, ymax = pct),
                position = position_dodge(width = 0.7),
                width = 0.55, linewidth = 1.1) +
  scale_color_manual(values = cores_area, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Docentes com bolsa PQ por nota do programa",
       subtitle = p2_subtitulo,
       x = "Nota do programa (Quadrienal 2017 a 2020)", y = NULL,
       caption = p2_rodape_g1) +
  p2_tema
