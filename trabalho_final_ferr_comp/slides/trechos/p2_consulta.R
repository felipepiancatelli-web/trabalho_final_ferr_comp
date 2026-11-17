p2_docentes <- dbGetQuery(con, "
  SELECT DISTINCT ON (d.ano, a.nm_area, d.id_pessoa)
         d.ano, a.nm_area, d.id_pessoa, p.id_prog, p.nota, d.bolsa_pq
  FROM sucupira.tab_docente d
  JOIN sucupira.ref_programa p ON d.programa_id = p.id_prog
  JOIN sucupira.ref_area a     ON p.area_id = a.id_area
  WHERE p.nota <> 'A'
  ORDER BY d.ano, a.nm_area, d.id_pessoa, p.nota DESC;") %>%
  mutate(area = rotulo_area(nm_area),
         nota = nota_categoria(nota),
         pq = !is.na(bolsa_pq),
         # nível 1 = 1A, 1B, 1C, 1D e SR (sênior)
         nivel_pq = factor(case_when(is.na(bolsa_pq) ~ "Sem bolsa",
                                     bolsa_pq == "2" ~ "Nível 2",
                                     TRUE ~ "Nível 1"),
                           levels = c("Nível 1", "Nível 2", "Sem bolsa")))
