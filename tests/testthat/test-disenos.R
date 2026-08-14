test_that("Experimental designs work correctly in easyModels", {
  # 1. Test RCBD
  set.seed(123)
  df_rcbd <- data.frame(
    rendimiento = rnorm(30, mean = 20),
    Tratamiento = factor(rep(c("T1", "T2", "T3"), each = 10)),
    Bloque = factor(rep(1:10, times = 3))
  )
  m_rcbd <- analizar_bloques_azar(df_rcbd, rendimiento ~ Tratamiento, "Bloque", diagnosticos = FALSE)
  expect_s3_class(m_rcbd, "easy_model")
  expect_equal(m_rcbd$tipo_modelo, "RCBD")
  
  # 2. Test Cuadrado Latino
  df_lat <- data.frame(
    y = rnorm(16, mean = 15),
    Trat = factor(rep(LETTERS[1:4], each = 4)),
    Fila = factor(rep(1:4, times = 4)),
    Columna = factor(rep(1:4, each = 4))
  )
  m_lat <- analizar_latino(df_lat, y ~ Trat, "Fila", "Columna", diagnosticos = FALSE)
  expect_s3_class(m_lat, "easy_model")
  expect_equal(m_lat$tipo_modelo, "Cuadrado-Latino")
  
  # 3. Test Medidas Repetidas
  df_rep <- data.frame(
    spad = rnorm(40, mean = 45),
    tratamiento = factor(rep(c("Control", "Estres"), each = 20)),
    tiempo = factor(rep(rep(c("T1", "T2"), each = 10), times = 2)),
    sujeto = factor(rep(1:10, times = 4))
  )
  m_rep <- analizar_medidas_repetidas(df_rep, spad ~ tratamiento * tiempo, sujeto = "sujeto", diagnosticos = FALSE)
  expect_s3_class(m_rep, "easy_model")
  expect_equal(m_rep$tipo_modelo, "Medidas-Repetidas")
  
  # 4. Test Strip-Plot
  df_strip <- data.frame(
    y = rnorm(36),
    A = factor(rep(c("A1", "A2"), each = 18)),
    B = factor(rep(rep(c("B1", "B2", "B3"), each = 6), times = 2)),
    Bloque = factor(rep(1:6, times = 6))
  )
  m_strip <- analizar_strip_plot(df_strip, y ~ A * B, bloque = "Bloque", factor_a = "A", factor_b = "B", diagnosticos = FALSE)
  expect_s3_class(m_strip, "easy_model")
  expect_equal(m_strip$tipo_modelo, "Strip-Plot")
})
