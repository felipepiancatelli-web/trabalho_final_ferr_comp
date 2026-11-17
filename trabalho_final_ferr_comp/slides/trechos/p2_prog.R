# Uma linha por programa: docentes, bolsistas e percentual de bolsistas
p2_prog <- p2_doc24 %>%
  group_by(id_prog, area, nota) %>%
  summarise(docentes = n(), bolsistas = sum(pq),
            .groups = "drop") %>%
  mutate(pct = 100 * bolsistas / docentes)
