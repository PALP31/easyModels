test_that("evaluar_modelo and S3 methods work seamlessly", {
  modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
  
  # 1. evaluar_modelo
  res_ev <- evaluar_modelo(modelo)
  expect_s3_class(res_ev, "lm")
  
  # 2. print method
  out_print <- capture.output(print(modelo))
  expect_true(length(out_print) > 0)
  
  # 3. summary method
  s <- summary(modelo)
  expect_s3_class(s, "summary_easy_model")
  out_sum <- capture.output(print(s))
  expect_true(any(grepl("TABLA DE ANOVA", out_sum)))
  
  # 4. plot method
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(modelo))
})
