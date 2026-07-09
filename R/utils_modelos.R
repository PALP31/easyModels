construir_formula_mixta <- function(formula_fijos, aleatorios) {
  if (inherits(formula_fijos, "formula")) {
    formula_str <- paste(deparse(formula_fijos), collapse = " ")
  } else {
    formula_str <- as.character(formula_fijos)
  }

  stats::as.formula(paste(formula_str, "+", aleatorios))
}

obtener_familia_modelo <- function(modelo) {
  tryCatch(stats::family(modelo), error = function(e) NULL)
}

obtener_coeficientes_fijos <- function(modelo) {
  if (inherits(modelo, "merMod")) {
    return(lme4::fixef(modelo))
  }

  stats::coef(modelo)
}

calcular_sobredispersion <- function(modelo) {
  residuos <- stats::residuals(modelo, type = "pearson")
  chi2 <- sum(residuos^2, na.rm = TRUE)
  gl <- stats::df.residual(modelo)

  if (is.null(gl) || is.na(gl)) {
    gl <- stats::nobs(modelo) - length(obtener_coeficientes_fijos(modelo))
  }

  data.frame(
    chi2 = chi2,
    gl = gl,
    ratio = chi2 / gl,
    row.names = NULL
  )
}

validar_predictor_modelo <- function(modelo, predictor) {
  variables_modelo <- tryCatch(names(stats::model.frame(modelo)), error = function(e) NULL)

  if (!is.null(variables_modelo) && !(predictor %in% variables_modelo)) {
    respuesta <- tryCatch(deparse(stats::formula(modelo)[[2]]), error = function(e) "")
    predictores_disponibles <- setdiff(variables_modelo, respuesta)
    stop(
      paste0(
        "Error: el predictor '", predictor, "' no existe en el modelo ajustado.\n",
        "Predictores detectados: ",
        paste(paste0("'", predictores_disponibles, "'"), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

detectar_columna_prediccion <- function(datos) {
  candidatas <- c("response", "prob", "rate", "emmean", "prediction")
  encontrada <- candidatas[candidatas %in% names(datos)][1]

  if (is.na(encontrada)) {
    stop("No se encontro una columna de prediccion reconocible en el resultado de emmeans.", call. = FALSE)
  }

  encontrada
}

detectar_columna_contraste <- function(datos) {
  candidatas <- c("odds.ratio", "ratio", "rate.ratio", "response", "estimate")
  encontrada <- candidatas[candidatas %in% names(datos)][1]

  if (is.na(encontrada)) {
    stop("No se encontro una columna de contraste reconocible.", call. = FALSE)
  }

  encontrada
}

detectar_intervalos <- function(datos) {
  pares <- list(
    list(inferior = "lower.CL", superior = "upper.CL"),
    list(inferior = "asymp.LCL", superior = "asymp.UCL"),
    list(inferior = "lower.HPD", superior = "upper.HPD")
  )

  for (par in pares) {
    if (all(c(par$inferior, par$superior) %in% names(datos))) {
      return(par)
    }
  }

  NULL
}
