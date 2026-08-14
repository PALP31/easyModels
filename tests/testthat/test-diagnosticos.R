test_that("Diagnostic tools and assumptions checking work correctly", {
  # 1. Test verificar_supuestos
  m_lm <- analizar_lm(iris, Sepal.Length ~ Sepal.Width + Petal.Length, diagnosticos = FALSE)
  sup <- verificar_supuestos(m_lm, silencioso = TRUE)
  
  expect_s3_class(sup, "easy_supuestos")
  expect_type(sup, "list")
  expect_true("normalidad" %in% names(sup))
  expect_true("homocedasticidad" %in% names(sup))
  expect_true("colinealidad" %in% names(sup))
  
  # 2. Test comparar_modelos
  m1 <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  m2 <- analizar_lm(iris, Sepal.Length ~ Species + Sepal.Width, diagnosticos = FALSE)
  
  comp <- comparar_modelos(m1, m2, nombres = c("M1", "M2"))
  expect_s3_class(comp, "data.frame")
  expect_true("AIC" %in% names(comp))
  expect_true("delta_AIC" %in% names(comp))
  expect_true("Peso_AIC" %in% names(comp))
  expect_equal(nrow(comp), 2)
  
  # 3. Test sugerir_modelo
  sug_norm <- sugerir_modelo(iris, "Sepal.Length")
  expect_s3_class(sug_norm, "easy_sugerencia")
  expect_true("familia_recomendada" %in% names(sug_norm))
  
  df_bin <- data.frame(mortalidad = c(0, 1, 0, 1, 1, 0, 0, 1), dosis = 1:8)
  sug_bin <- sugerir_modelo(df_bin, "mortalidad")
  expect_true(grepl("binomial", sug_bin$familia_recomendada))
})
