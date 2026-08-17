test_that("obtener_emmeans and obtener_posthoc work correctly", {
  modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  # 1. EMMeans
  emm <- obtener_emmeans(modelo, "Species")
  expect_s4_class(emm, "emmGrid")
  
  # 2. Post-hoc pairwise differences
  ph <- obtener_posthoc(modelo, "Species")
  expect_s3_class(ph, "data.frame")
  expect_true("contrast" %in% names(ph))
  expect_true("p.value" %in% names(ph))
  
  # 3. Post-hoc with CLD Letters
  ph_letras <- obtener_posthoc(modelo, "Species", letras = TRUE)
  expect_s3_class(ph_letras, "data.frame")
  expect_true("Grupo" %in% names(ph_letras) || "emmean" %in% names(ph_letras))
})

test_that("graficar_predichos and graficar_posthoc return ggplot objects", {
  modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  # Plot predictions with letters
  g1 <- graficar_predichos(modelo, "Species", mostrar_letras = TRUE)
  expect_s3_class(g1, "ggplot")
  
  # Plot post-hoc contrasts
  ph <- obtener_posthoc(modelo, "Species")
  g2 <- graficar_posthoc(ph)
  expect_s3_class(g2, "ggplot")
  
  # obtener_posthoc with graficar = TRUE
  g3 <- obtener_posthoc(modelo, "Species", graficar = TRUE)
  expect_s3_class(g3, "ggplot")
  
  g4 <- obtener_posthoc(modelo, "Species", letras = TRUE, graficar = TRUE)
  expect_s3_class(g4, "ggplot")
})

test_that("analizar_odds_ratio computes correct ORs", {
  df_bin <- iris
  df_bin$es_setosa <- as.integer(df_bin$Species == "setosa")
  m_bin <- analizar_glm(df_bin, es_setosa ~ Sepal.Length, familia = "binomial", diagnosticos = FALSE)
  
  ors <- analizar_odds_ratio(m_bin)
  expect_s3_class(ors, "data.frame")
  expect_true(all(c("termino", "log_odds", "SE", "OR", "IC_inf", "IC_sup") %in% names(ors)))
  expect_true(all(ors$OR > 0))
})
