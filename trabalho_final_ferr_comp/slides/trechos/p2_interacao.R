p2_inter_cat <- glm(
  cbind(bolsistas, docentes - bolsistas) ~ area * nota,
  family = quasibinomial, data = p2_dados_mod)
p2_teste_cat <- anova(p2_modelo, p2_inter_cat, test = "F")
