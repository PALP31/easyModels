#' Resumir un ajuste de easyModels en una fila
#'
#' Crea una tabla compacta y reproducible con la información esencial de un
#' ajuste: fórmula, familia, observaciones usadas y métricas de desempeño. Es
#' útil para informes, cuadernos de análisis y para comparar resultados sin
#' depender de la salida impresa de cada motor estadístico.
#'
#' @param modelo Un objeto \code{easy_model} o un modelo nativo compatible.
#'
#' @return Un \code{data.frame} de una fila con el resumen del ajuste.
#' @export
#'
#' @examples
#' modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#' resumir_ajuste(modelo)
resumir_ajuste <- function(modelo) {
  modelo_nativo <- extraer_modelo(modelo)
  es_easy_model <- inherits(modelo, "easy_model")

  formula_modelo <- tryCatch(
    Reduce(paste, deparse(stats::formula(modelo_nativo))),
    error = function(e) NA_character_
  )
  info_muestra <- if (es_easy_model && is.list(modelo$info)) {
    modelo$info
  } else {
    .resumir_observaciones(modelo_nativo)
  }

  r2_valor <- tryCatch(
    suppressWarnings(performance::r2(modelo_nativo)),
    error = function(e) NULL
  )
  r2_condicional <- .safe_get(r2_valor, "R2_conditional")
  r2_marginal <- .safe_get(r2_valor, "R2_marginal")
  r2_simple <- .safe_get(r2_valor, "R2")
  if (is.na(r2_marginal) && !is.na(r2_simple)) r2_marginal <- r2_simple

  observaciones <- .safe_get(info_muestra, "observaciones")
  observaciones_utilizadas <- .safe_get(info_muestra, "observaciones_utilizadas")
  observaciones_excluidas <- .safe_get(info_muestra, "observaciones_excluidas")

  data.frame(
    modelo = if (es_easy_model) modelo$tipo_modelo else class(modelo_nativo)[1],
    formula = formula_modelo,
    respuesta = if (es_easy_model) modelo$respuesta else NA_character_,
    familia = if (es_easy_model) modelo$familia else NA_character_,
    enlace = if (es_easy_model) modelo$link else NA_character_,
    observaciones = as.integer(observaciones),
    observaciones_utilizadas = as.integer(observaciones_utilizadas),
    observaciones_excluidas = as.integer(observaciones_excluidas),
    AIC = tryCatch(round(stats::AIC(modelo_nativo), 2), error = function(e) NA_real_),
    BIC = tryCatch(round(stats::BIC(modelo_nativo), 2), error = function(e) NA_real_),
    R2_marginal = round(r2_marginal, 3),
    R2_condicional = round(r2_condicional, 3),
    check.names = FALSE
  )
}
