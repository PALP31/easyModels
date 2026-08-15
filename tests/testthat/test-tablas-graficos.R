test_that("Table exports and publication plotting work correctly", {
  m <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  # 1. Test exportar_tabla_anova
  tab_anova <- exportar_tabla_anova(m, formato = "data.frame")
  expect_s3_class(tab_anova, "data.frame")
  expect_true("p_valor" %in% names(tab_anova))
  
  # 2. Test exportar_tabla_posthoc
  ph <- obtener_posthoc(m, "Species", letras = TRUE)
  tab_ph <- exportar_tabla_posthoc(ph, formato = "data.frame")
  expect_s3_class(tab_ph, "data.frame")
  expect_true("Grupo" %in% names(tab_ph) || ".group" %in% names(tab_ph))
  
  # 3. Test graficar_predichos with barplots
  p_bar <- graficar_predichos(m, "Species", tipo_grafico = "barras", mostrar_letras = TRUE)
  expect_s3_class(p_bar, "ggplot")
  
  # 4. Test graficar_predichos with points
  p_point <- graficar_predichos(m, "Species", tipo_grafico = "puntos", paleta = "teal")
  expect_s3_class(p_point, "ggplot")
  
  # 5. Test graficar_interaccion
  m_int <- analizar_lm(iris, Sepal.Length ~ Species * Petal.Width, diagnosticos = FALSE)
  # Interaction plot
  p_int <- graficar_interaccion(m_int, factor_x = "Species", factor_traza = "Species", paleta = "viridis")
  expect_s3_class(p_int, "ggplot")
})

test_that("Posthoc fallback keeps Grupo column when CLD dependencies are unavailable", {
  m <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) FALSE,
    .env = environment(obtener_posthoc)
  )
  
  ph <- obtener_posthoc(m, "Species", letras = TRUE)
  expect_s3_class(ph, "data.frame")
  expect_true("Grupo" %in% names(ph))
  expect_true(all(is.na(ph$Grupo)))
})

test_that("graficar_predichos skips CLD gracefully when dependencies are unavailable", {
  m <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) FALSE,
    .env = environment(graficar_predichos)
  )
  
  expect_no_warning(
    p <- graficar_predichos(m, "Species", tipo_grafico = "barras", mostrar_letras = TRUE)
  )
  expect_s3_class(p, "ggplot")
})
