test_that("resumir_ajuste comunica el uso de observaciones", {
  datos <- iris
  datos$Sepal.Length[1] <- NA_real_

  modelo <- analizar_lm(datos, Sepal.Length ~ Species, diagnosticos = FALSE)
  resumen <- resumir_ajuste(modelo)

  expect_s3_class(resumen, "data.frame")
  expect_named(
    resumen,
    c(
      "modelo", "formula", "respuesta", "familia", "enlace",
      "observaciones", "observaciones_utilizadas", "observaciones_excluidas",
      "AIC", "BIC", "R2_marginal", "R2_condicional"
    )
  )
  expect_equal(resumen$observaciones, 150L)
  expect_equal(resumen$observaciones_utilizadas, 149L)
  expect_equal(resumen$observaciones_excluidas, 1L)
  expect_equal(modelo$info$observaciones_utilizadas, 149L)
})

test_that("los ajustadores validan datos y variables antes de ajustar", {
  expect_error(
    analizar_lm(iris, respuesta_inexistente ~ Species, diagnosticos = FALSE),
    "No se encontraron"
  )
  expect_error(
    analizar_glm(matrix(1:8, ncol = 2), V1 ~ V2, diagnosticos = FALSE),
    "data.frame"
  )
})
