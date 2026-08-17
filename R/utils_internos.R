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

#' Validar datos y fórmula antes de ajustar un modelo
#'
#' Comprueba los errores de entrada más frecuentes antes de delegar el ajuste al
#' motor estadístico. Es una utilidad interna compartida por los ajustadores del
#' paquete para que los mensajes de error sean claros y consistentes.
#'
#' @param datos Un data.frame con las variables del modelo.
#' @param formula Una fórmula de R, o una cadena que pueda convertirse en fórmula.
#' @param funcion Nombre de la función que solicita la validación.
#'
#' @return La fórmula validada.
#' @keywords internal
.validar_entrada_modelo <- function(datos, formula, funcion) {
  if (!is.data.frame(datos)) {
    cli::cli_abort("{funcion}() requiere que 'datos' sea un data.frame.")
  }

  if (is.character(formula) && length(formula) == 1L && !is.na(formula)) {
    formula <- tryCatch(
      stats::as.formula(formula),
      error = function(e) cli::cli_abort("'formula' no es válida: {conditionMessage(e)}")
    )
  }

  if (!inherits(formula, "formula")) {
    cli::cli_abort("{funcion}() requiere una fórmula de R en 'formula'.")
  }

  variables <- unique(all.vars(formula))
  faltantes <- setdiff(variables, names(datos))
  if (length(faltantes) > 0L) {
    cli::cli_abort(
      "No se encontraron estas variables en 'datos': {paste(faltantes, collapse = ', ')}. Variables disponibles: {paste(names(datos), collapse = ', ')}."
    )
  }

  formula
}

#' Resumir las observaciones utilizadas en un ajuste
#'
#' @param modelo Un modelo estadístico ajustado.
#' @param datos El data.frame original, si está disponible.
#'
#' @return Una lista con el número total, utilizado y excluido de observaciones.
#' @keywords internal
.resumir_observaciones <- function(modelo, datos = NULL) {
  n_total <- if (is.data.frame(datos)) nrow(datos) else NA_integer_
  n_utilizadas <- tryCatch(
    as.integer(nrow(stats::model.frame(modelo))),
    error = function(e) tryCatch(as.integer(stats::nobs(modelo)), error = function(e2) NA_integer_)
  )

  n_excluidas <- if (!is.na(n_total) && !is.na(n_utilizadas)) {
    as.integer(n_total - n_utilizadas)
  } else {
    NA_integer_
  }

  list(
    observaciones = as.integer(n_total),
    observaciones_utilizadas = as.integer(n_utilizadas),
    observaciones_excluidas = n_excluidas
  )
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

  info_muestra <- .resumir_observaciones(modelo, datos)
  
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
    info = info_muestra
  )
  
  class(em) <- if (!is.null(custom_class)) c(custom_class, "easy_model") else "easy_model"
  return(em)
}
