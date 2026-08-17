#' Extraer el modelo estadistico subyacente
#'
#' Esta funcion detecta si el objeto de entrada es de clase \code{easy_model}
#' y extrae el modelo nativo subyacente. Si ya es un modelo nativo, lo devuelve tal cual.
#'
#' @param x Un objeto de clase \code{easy_model} o un modelo nativo (lm, glm, merMod, glmmTMB, etc.).
#'
#' @return El modelo nativo subyacente.
#' @export
extraer_modelo <- function(x) {
  if (inherits(x, "easy_model")) {
    return(x$modelo)
  }
  return(x)
}

#' Helper interno para extraccion segura de elementos numericos
#'
#' Extrae de forma segura un elemento de un objeto (lista o vector nombrado).
#' Devuelve NA si el objeto no es un vector/lista, si la clave no existe,
#' o si el valor es NA (incluyendo el NA logico que devuelve performance::icc()
#' cuando el modelo es singular).
#'
#' @param obj Lista o vector nombrado.
#' @param key Clave a extraer.
#'
#' @return Valor numerico o NA.
#' @keywords internal
.safe_get <- function(obj, key) {
  if (is.null(obj)) return(NA)
  if (is.logical(obj) && length(obj) == 1 && is.na(obj)) return(NA)
  if (is.list(obj)) {
    val <- obj[[key]]
    if (is.null(val)) return(NA)
    result <- as.numeric(val[1])
    if (is.na(result)) return(NA)
    result
  } else if (is.numeric(obj) && !is.null(names(obj)) && key %in% names(obj)) {
    as.numeric(obj[key])
  } else {
    NA
  }
}

#' Crear un objeto unificado de clase easy_model
#'
#' Esta funcion interna construye un objeto S3 de clase \code{easy_model} a partir
#' de un modelo nativo ajustado. Extrae de forma automatica metodos, formulas,
#' familias y links utilizando el paquete \code{insight}.
#'
#' @param modelo Modelo nativo ajustado (ej. lm, glm, merMod, glmmTMB).
#' @param tipo_modelo Cadena de caracteres que define el tipo de modelo (ej. "LM", "GLM", "LMM", "GLMM", "RCBD", "Split-Plot").
#' @param datos El \code{data.frame} de datos original utilizado en el ajuste.
#' @param custom_class Clase S3 adicional para anteponer a "easy_model" (ej. "easy_splitplot").
#'
#' @return Un objeto de clase S3 \code{easy_model} (y opcionalmente la subclase especificada).
#' @keywords internal
#' @importFrom insight find_formula find_response model_info
#' @importFrom stats formula anova residuals fitted
#' @importFrom car Anova
crear_easy_model <- function(modelo, tipo_modelo, datos, custom_class = NULL) {
  # Obtener formula
  f <- tryCatch({
    form_res <- insight::find_formula(modelo)
    if (is.list(form_res) && !is.null(form_res$conditional)) {
      form_res$conditional
    } else {
      stats::formula(modelo)
    }
  }, error = function(e) {
    tryCatch(stats::formula(modelo), error = function(e2) NULL)
  })
  
  # Obtener respuesta
  resp <- tryCatch({
    insight::find_response(modelo)
  }, error = function(e) {
    if (!is.null(f) && length(f) >= 2) as.character(f[[2]]) else "desconocida"
  })
  
  # Obtener familia y link
  fam_info <- tryCatch({
    insight::model_info(modelo)
  }, error = function(e) {
    NULL
  })
  
  fam <- if (!is.null(fam_info$family)) fam_info$family else "gaussian"
  lnk <- if (!is.null(fam_info$link_function)) fam_info$link_function else "identity"
  
  # Calcular ANOVA
  is_mixed_or_split <- tipo_modelo %in% c("LMM", "GLMM", "RCBD", "Split-Plot")
  anova_type <- if (is_mixed_or_split) 3 else 2
  
  tab_anova <- tryCatch({
    car::Anova(modelo, type = anova_type)
  }, error = function(e) {
    tryCatch({
      stats::anova(modelo)
    }, error = function(e2) {
      NULL
    })
  })
  
  # Diagnostico
  diag_obj <- list()
  diag_obj$residuos_pearson <- tryCatch({
    stats::residuals(modelo, type = "pearson")
  }, error = function(e) {
    tryCatch(stats::residuals(modelo), error = function(e2) NULL)
  })
  
  diag_obj$valores_ajustados <- tryCatch({
    stats::fitted(modelo)
  }, error = function(e) {
    NULL
  })
  
  # Construir objeto easy_model
  em <- list(
    modelo = modelo,
    anova = tab_anova,
    diagnostico = diag_obj,
    formula = f,
    datos = datos,
    respuesta = resp,
    tipo_modelo = tipo_modelo,
    familia = fam,
    link = lnk,
    info = list()
  )
  
  class(em) <- if (!is.null(custom_class)) c(custom_class, "easy_model") else "easy_model"
  return(em)
}
