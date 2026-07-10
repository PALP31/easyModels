test_that("S3 easy_model class structure is consistent", {
  # 1. Test linear model unifed S3 object
  datos_lm <- iris
  modelo_lm <- analizar_lm(datos_lm, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  expect_s3_class(modelo_lm, "easy_model")
  expect_type(modelo_lm, "list")
  
  # Check exact list keys
  required_keys <- c("modelo", "anova", "diagnostico", "formula", "datos", 
                     "respuesta", "tipo_modelo", "familia", "link", "info")
  expect_true(all(required_keys %in% names(modelo_lm)))
  
  expect_equal(modelo_lm$tipo_modelo, "LM")
  expect_equal(modelo_lm$respuesta, "Sepal.Length")
  expect_equal(modelo_lm$familia, "gaussian")
  expect_equal(modelo_lm$link, "identity")
  
  # 2. Test model extractor utility
  expect_s3_class(extraer_modelo(modelo_lm), "lm")
  expect_s3_class(extraer_modelo(modelo_lm$modelo), "lm")
})

test_that("S3 easy_splitplot combined class is structured correctly", {
  set.seed(456)
  datos_split <- data.frame(
    Riego = factor(rep(c("Riego", "Secano"), each = 24)),
    Genotipo = factor(rep(rep(c("G1", "G2", "G3"), each = 8), times = 2)),
    Bloque = factor(rep(1:4, times = 12)),
    Rendimiento = rnorm(48, mean = 8, sd = 1.2)
  )
  
  modelo_sp <- analizar_parcelas_divididas(
    datos = datos_split,
    formula_fijos = Rendimiento ~ Riego * Genotipo,
    bloque = "Bloque",
    parcela_principal = "Riego",
    diagnosticos = FALSE
  )
  
  expect_s3_class(modelo_sp, "easy_splitplot")
  expect_s3_class(modelo_sp, "easy_model")
  expect_equal(modelo_sp$tipo_modelo, "Split-Plot")
})
