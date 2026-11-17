p2_comp <- p2_doc24 %>%
  count(area, nota, nivel_pq) %>%
  group_by(area, nota) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

p2_g2 <- ggplot(p2_comp, aes(x = nota, y = pct, fill = fct_rev(nivel_pq))) +
  geom_col(width = 0.72, color = "white", linewidth = 0.6) +
  scale_fill_manual(values = c("Nível 1" = "#3d3d3a", "Nível 2" = "#8f8e87",
                               "Sem bolsa" = "#dcdbd5"),
                    breaks = c("Nível 1", "Nível 2", "Sem bolsa"), name = NULL) +
  facet_wrap(~ area)
