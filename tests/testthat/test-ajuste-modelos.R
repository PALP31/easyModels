test_that("analizar_lm fits linear models properly", {
  m <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  expect_s3_class(m, "easy_model")
  expect_equal(m$tipo_modelo, "LM")
  expect_equal(m$respuesta, "Sepal.Length")
  expect_s3_class(m$modelo, "lm")
  expect_true(!is.null(m$anova))
})

test_that("analizar_lmm fits linear mixed models with lme4", {
  set.seed(123)
  df <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    bloque = factor(rep(1:6, each = 10))
  )
  m <- analizar_lmm(df, y ~ x, "(1 | bloque)", diagnosticos = FALSE)
  expect_s3_class(m, "easy_model")
  expect_equal(m$tipo_modelo, "LMM")
  expect_s4_class(m$modelo, "lmerMod")
})

test_that("analizar_glm fits various families", {
  # 1. Binomial
  df_bin <- iris
  df_bin$es_setosa <- as.integer(df_bin$Species == "setosa")
  m_bin <- analizar_glm(df_bin, es_setosa ~ Sepal.Length, familia = "binomial", diagnosticos = FALSE)
  expect_s3_class(m_bin, "easy_model")
  expect_equal(m_bin$familia, "binomial")
  
  # 2. Poisson
  set.seed(42)
  df_count <- data.frame(
    conteo = rpois(50, lambda = 4),
    grupo = factor(rep(c("A", "B"), each = 25))
  )
  m_pois <- analizar_glm(df_count, conteo ~ grupo, familia = "poisson", diagnosticos = FALSE)
  expect_s3_class(m_pois, "easy_model")
  expect_equal(m_pois$familia, "poisson")
  
  # 3. Gamma
  df_gamma <- data.frame(
    tiempo = rgamma(50, shape = 2, scale = 3),
    trat = factor(rep(c("Ctrl", "Trat"), each = 25))
  )
  m_gamma <- analizar_glm(df_gamma, tiempo ~ trat, familia = "gamma", diagnosticos = FALSE)
  expect_s3_class(m_gamma, "easy_model")
  expect_equal(m_gamma$familia, "Gamma")
})

test_that("analizar_bloques_azar fits RCBD correctly", {
  set.seed(101)
  df_rcbd <- data.frame(
    rendimiento = rnorm(30, mean = 10),
    tratamiento = factor(rep(c("T1", "T2", "T3"), each = 10)),
    Bloque = factor(rep(1:10, times = 3))
  )
  m <- analizar_bloques_azar(df_rcbd, rendimiento ~ tratamiento, "Bloque", diagnosticos = FALSE)
  expect_s3_class(m, "easy_model")
  expect_equal(m$tipo_modelo, "RCBD")
  expect_s4_class(m$modelo, "lmerMod")
})

test_that("analizar_glmm fits mixed generalized models", {
  set.seed(202)
  df_glmm <- data.frame(
    conteo = rpois(40, lambda = 5),
    trat = factor(rep(c("A", "B"), each = 20)),
    bloque = factor(rep(1:4, times = 10))
  )
  m <- analizar_glmm(df_glmm, conteo ~ trat, "(1 | bloque)", tipo = "poisson", diagnosticos = FALSE)
  expect_s3_class(m, "easy_model")
  expect_equal(m$tipo_modelo, "GLMM")
  expect_s4_class(m$modelo, "glmerMod")
})
