p2_evolucao <- p2_docentes %>%
  group_by(ano, area) %>%
  summarise(n = n(), bolsistas = sum(pq), .groups = "drop") %>%
  mutate(pct = 100 * bolsistas / n)

p2_g3 <- ggplot(p2_evolucao, aes(x = ano, y = pct, color = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = filter(p2_evolucao, ano == max(ano)),
            aes(label = area), hjust = 0, nudge_x = 0.08, size = 3.6,
            show.legend = FALSE) +
  scale_color_manual(values = cores_area, guide = "none")
