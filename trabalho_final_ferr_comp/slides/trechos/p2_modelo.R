p2_dados_mod <- p2_prog %>%
  mutate(nota_num = as.integer(substr(as.character(nota), 1, 1)),
         nota = relevel(nota, ref = "4"), area = factor(area))

p2_modelo <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area + nota,
  family = quasibinomial, data = p2_dados_mod)
