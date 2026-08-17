#' Construir formula mixta
#'
#' Concatena la formula de efectos fijos con la estructura de efectos aleatorios.
#'
#' @param formula_fijos Formula de efectos fijos o caracter.
#' @param aleatorios Caracter con la definicion de efectos aleatorios (ej. "(1 | bloque)").
#'
#' @return Un objeto de clase \code{formula}.
#' @keywords internal
construir_formula_mixta <- function(formula_fijos, aleatorios) {
  if (inherits(formula_fijos, "formula")) {
    formula_str <- paste(deparse(formula_fijos), collapse = " ")
  } else {
    formula_str <- as.character(formula_fijos)
  }

  stats::as.formula(paste(formula_str, "+", aleatorios))
}

#' Obtener familia de distribucion del modelo
#'
#' @param modelo Modelo ajustado.
#' @return Objeto de familia o NULL.
#' @keywords internal
obtener_familia_modelo <- function(modelo) {
  m_nat <- extraer_modelo(modelo)
  fam <- tryCatch(stats::family(m_nat), error = function(e) NULL)
  if (is.null(fam)) {
    fam_info <- tryCatch(insight::model_info(m_nat), error = function(e) NULL)
    if (is.list(fam_info)) {
      fam <- list(
        family = fam_info$family,
        link = fam_info$link_function
      )
    }
  }
  fam
}

#' Obtener coeficientes fijos del modelo
#'
#' @param modelo Modelo ajustado (lm, glm, merMod, glmmTMB, zeroinfl, etc.).
#' @return Vector nombrado de coeficientes fijos.
#' @keywords internal
obtener_coeficientes_fijos <- function(modelo) {
  m_nat <- extraer_modelo(modelo)
  if (inherits(m_nat, "merMod")) {
    return(lme4::fixef(m_nat))
  }
  if (inherits(m_nat, "glmmTMB")) {
    if (requireNamespace("glmmTMB", quietly = TRUE)) {
      fe <- glmmTMB::fixef(m_nat)
      if (is.list(fe) && !is.null(fe$cond)) {
        return(fe$cond)
      }
      return(fe)
    }
  }
  if (inherits(m_nat, "zeroinfl")) {
    return(stats::coef(m_nat, model = "count"))
  }
  stats::coef(m_nat)
}

#' Calcular sobredispersion de Pearson
#'
#' @param modelo Modelo ajustado.
#' @return data.frame con chi2, gl y ratio.
#' @keywords internal
calcular_sobredispersion <- function(modelo) {
  m_nat <- extraer_modelo(modelo)
  residuos <- stats::residuals(m_nat, type = "pearson")
  chi2 <- sum(residuos^2, na.rm = TRUE)
  gl <- stats::df.residual(m_nat)

  if (is.null(gl) || is.na(gl)) {
    coefs <- tryCatch(obtener_coeficientes_fijos(m_nat), error = function(e) numeric(0))
    n_obs <- tryCatch(stats::nobs(m_nat), error = function(e) length(residuos))
    gl <- n_obs - length(coefs)
  }

  ratio_val <- if (!is.null(gl) && !is.na(gl) && gl > 0) chi2 / gl else NA_real_

  data.frame(
    chi2 = chi2,
    gl = gl,
    ratio = ratio_val,
    row.names = NULL
  )
}

#' Validar existencia de un predictor en el modelo
#'
#' @param modelo Modelo ajustado o easy_model.
#' @param predictor Nombre del predictor.
#' @return TRUE invisible o error.
#' @keywords internal
validar_predictor_modelo <- function(modelo, predictor) {
  m_nat <- extraer_modelo(modelo)
  
  variables_modelo <- tryCatch(names(stats::model.frame(m_nat)), error = function(e) NULL)
  
  if (inherits(modelo, "easy_model") && !is.null(modelo$datos)) {
    variables_modelo <- union(variables_modelo, names(modelo$datos))
  }
  
  if (is.null(variables_modelo)) {
    var_info <- tryCatch(insight::find_variables(m_nat), error = function(e) NULL)
    if (is.list(var_info)) {
      variables_modelo <- unlist(var_info, use.names = FALSE)
    }
  }

  if (!is.null(variables_modelo) && !(predictor %in% variables_modelo)) {
    respuesta <- tryCatch(deparse(stats::formula(m_nat)[[2]]), error = function(e) "")
    predictores_disponibles <- setdiff(variables_modelo, respuesta)
    cli::cli_abort(
      paste0(
        "El predictor '{predictor}' no existe en el modelo ajustado.\n",
        "Predictores detectados: ",
        paste(paste0("'", predictores_disponibles, "'"), collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' Detectar columna de prediccion en salida de emmeans
#'
#' @param datos data.frame devuelto por summary(emm).
#' @return Nombre de la columna encontrada.
#' @keywords internal
detectar_columna_prediccion <- function(datos) {
  candidatas <- c("response", "prob", "rate", "emmean", "prediction", "estimate")
  encontrada <- candidatas[candidatas %in% names(datos)][1]

  if (is.na(encontrada)) {
    cli::cli_abort("No se encontro una columna de prediccion reconocible en el resultado de emmeans.")
  }

  encontrada
}

#' Detectar columna de contraste en salida de emmeans
#'
#' @param datos data.frame devuelto por summary(contrast).
#' @return Nombre de la columna encontrada.
#' @keywords internal
detectar_columna_contraste <- function(datos) {
  candidatas <- c("odds.ratio", "ratio", "rate.ratio", "response", "estimate", "difference")
  encontrada <- candidatas[candidatas %in% names(datos)][1]

  if (is.na(encontrada)) {
    cli::cli_abort("No se encontro una columna de contraste reconocible.")
  }

  encontrada
}

#' Detectar columnas de intervalos de confianza
#'
#' @param datos data.frame.
#' @return Lista con 'inferior' y 'superior' o NULL.
#' @keywords internal
detectar_intervalos <- function(datos) {
  pares <- list(
    list(inferior = "lower.CL", superior = "upper.CL"),
    list(inferior = "asymp.LCL", superior = "asymp.UCL"),
    list(inferior = "lower.HPD", superior = "upper.HPD"),
    list(inferior = "asymp.lcl", superior = "asymp.ucl"),
    list(inferior = "2.5 %", superior = "97.5 %")
  )

  for (par in pares) {
    if (all(c(par$inferior, par$superior) %in% names(datos))) {
      return(par)
    }
  }

  NULL
}
