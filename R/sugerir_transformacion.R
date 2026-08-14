#' Sugerir Familia de GLM o Transformación de Datos
#'
#' Analiza la variable de respuesta biológica (distribución, presencia de ceros,
#' sesgo, soporte numérico y sobredispersión) y recomienda la familia óptima de GLM/GLMM
#' en \code{easyModels} o la transformación matemática adecuada (Box-Cox, log, raíz cuadrada, arcoseno).
#'
#' @param datos Un \code{data.frame} con los datos experimentales.
#' @param respuesta Nombre de la columna de respuesta (cadena de caracteres).
#' @param predictores Vector opcional con los nombres de predictores para evaluar Box-Cox en regresión.
#'
#' @return Una lista de clase \code{easy_sugerencia} con el diagnóstico y recomendaciones.
#' @export
#'
#' @importFrom stats na.omit lm formula var sd quantile
#' @importFrom MASS boxcox
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success cli_alert_warning boxx
#'
#' @examples
#' \dontrun{
#'   sugerir_modelo(iris, "Sepal.Length")
#' }
sugerir_modelo <- function(datos, respuesta, predictores = NULL) {
  if (!is.character(respuesta) || !(respuesta %in% names(datos))) {
    stop(paste0("La variable de respuesta '", respuesta, "' no existe en el data.frame."), call. = FALSE)
  }
  
  y <- stats::na.omit(datos[[respuesta]])
  n <- length(y)
  n_ceros <- sum(y == 0)
  prop_ceros <- n_ceros / n
  es_entero <- all(y == floor(y)) && all(y >= 0)
  min_val <- min(y)
  max_val <- max(y)
  
  sugerencia <- list(
    variable = respuesta,
    n = n,
    prop_ceros = prop_ceros,
    tipo_datos = "",
    familia_recomendada = "",
    transformacion_recomendada = "Ninguna",
    boxcox_lambda = NA,
    justificacion = ""
  )
  
  cli::cli_h1("Asistente Bioestadístico de Selección de Modelos (easyModels)")
  
  # 1. Caso Binario (0/1)
  if (all(y %in% c(0, 1))) {
    sugerencia$tipo_datos <- "Binario (Presencia/Ausencia, Mortalidad, Germinación 0/1)"
    sugerencia$familia_recomendada <- "binomial (o binomial_cloglog para tasas de incidencia)"
    sugerencia$justificacion <- "Variable dicotómica 0/1. No requiere transformación; use GLM/GLMM con enlace logit o cloglog."
    
  # 2. Caso Proporciones Continuas / Coberturas en (0, 1)
  } else if (min_val > 0 && max_val < 1 && !es_entero) {
    sugerencia$tipo_datos <- "Proporción Continua en intervalo (0, 1) (Cobertura foliar, severidad)"
    sugerencia$familia_recomendada <- "beta (vía betareg o glmmTMB)"
    sugerencia$transformacion_recomendada <- "arcsin(sqrt(y)) o logit(y) para modelos lineales clásicos"
    sugerencia$justificacion <- "Los datos están acotados entre 0 y 1. La regresión Beta modela directamente heterocedasticidad y asimetría."

  # 3. Caso Conteos Enteros (Poisson / Negativa Binomial / ZIP / ZINB)
  } else if (es_entero) {
    media_y <- mean(y)
    var_y <- stats::var(y)
    ratio_disp <- if (media_y > 0) var_y / media_y else 1
    
    if (prop_ceros > 0.30) {
      sugerencia$tipo_datos <- paste0("Conteos con Exceso de Ceros (", round(prop_ceros * 100, 1), "% de ceros)")
      sugerencia$familia_recomendada <- if (ratio_disp > 1.5) "zinb (Zero-Inflated Negative Binomial)" else "zip (Zero-Inflated Poisson)"
      sugerencia$justificacion <- "Alta frecuencia de ceros (>30%). Se recomienda un modelo en dos partes (Zero-Inflation) con 'pscl' o 'glmmTMB'."
    } else if (ratio_disp > 1.5) {
      sugerencia$tipo_datos <- paste0("Conteos con Sobredispersión (Varianza/Media = ", round(ratio_disp, 2), " > 1.5)")
      sugerencia$familia_recomendada <- "negativa_binomial (MASS::glm.nb / glmmTMB)"
      sugerencia$transformacion_recomendada <- "log(y + 1) o sqrt(y) para LM clásico"
      sugerencia$justificacion <- "La varianza supera ampliamente la media, violando el supuesto de Poisson. La distribución Binomial Negativa ajusta el parámetro de dispersión theta."
    } else {
      sugerencia$tipo_datos <- "Conteos estándar (Equidispersión)"
      sugerencia$familia_recomendada <- "poisson"
      sugerencia$transformacion_recomendada <- "sqrt(y) para LM clásico"
      sugerencia$justificacion <- "La varianza es aproximadamente igual a la media. Use GLM Poisson con enlace log."
    }

  # 4. Caso Continuo Positivo con Ceros Mixtos (Precipitación, Biomasa, Rendimiento con fallos)
  } else if (min_val == 0 && max_val > 0) {
    sugerencia$tipo_datos <- paste0("Continuo Semicontinuo con Ceros Exactos (", round(prop_ceros * 100, 1), "% de ceros)")
    sugerencia$familia_recomendada <- "tweedie (statmod / glmmTMB con tweedie_p = 1.5)"
    sugerencia$transformacion_recomendada <- "log(y + 1)"
    sugerencia$justificacion <- "Masa de probabilidad en cero combinada con cola continua positiva. La familia Tweedie modela conjuntamente ceros continuos sin sesgo por adición de constantes."

  # 5. Caso Continuo Estrictamente Positivo
  } else if (min_val > 0) {
    # Calcular asimetría (skewness)
    media_y <- mean(y)
    mediana_y <- stats::median(y)
    sd_y <- stats::sd(y)
    sesgo <- if (sd_y > 0) sum((y - media_y)^3) / ((n - 1) * sd_y^3) else 0
    
    # Calcular Box-Cox
    f_boxcox <- if (!is.null(predictores)) {
      stats::as.formula(paste(respuesta, "~", paste(predictores, collapse = " + ")))
    } else {
      stats::as.formula(paste(respuesta, "~ 1"))
    }
    
    bc <- tryCatch(suppressWarnings(MASS::boxcox(f_boxcox, data = datos, plotit = FALSE)), error = function(e) NULL)
    if (!is.null(bc)) {
      sugerencia$boxcox_lambda <- round(bc$x[which.max(bc$y)], 2)
    }
    
    if (sesgo > 1.0) {
      sugerencia$tipo_datos <- paste0("Continuo Positivo Asimétrico a la Derecha (Sesgo = ", round(sesgo, 2), ")")
      sugerencia$familia_recomendada <- "gamma (enlace log) o gaussian_inversa"
      sugerencia$transformacion_recomendada <- if (!is.na(sugerencia$boxcox_lambda) && abs(sugerencia$boxcox_lambda) < 0.2) "log(y)" else paste0("y^(", sugerencia$boxcox_lambda, ")")
      sugerencia$justificacion <- "Distribución asimétrica con varianza proporcional a la media al cuadrado. GLM Gamma es preferible a transformar logarítmicamente."
    } else {
      sugerencia$tipo_datos <- "Continuo Simétrico / Normal aproximado"
      sugerencia$familia_recomendada <- "gaussian (LM / LMM estándar)"
      sugerencia$transformacion_recomendada <- "Ninguna requerida"
      sugerencia$justificacion <- "Los datos cumplen razonablemente la simetría gaussiana."
    }
  } else {
    sugerencia$tipo_datos <- "Continuo con valores negativos y positivos"
    sugerencia$familia_recomendada <- "gaussian (LM / LMM)"
    sugerencia$justificacion <- "Valores reales continuos con soporte en ambos signos."
  }
  
  # Imprimir recomendación en CLI
  cli::cli_alert_info("Variable analizada: {.strong {respuesta}} (N = {n}, Ceros = {round(prop_ceros*100, 1)}%)")
  cli::cli_alert_info("Naturaleza de los datos: {.val {sugerencia$tipo_datos}}")
  cli::cli_alert_success("Familia GLM Recomendada: {.strong {sugerencia$familia_recomendada}}")
  if (sugerencia$transformacion_recomendada != "Ninguna requerida" && sugerencia$transformacion_recomendada != "Ninguna") {
    cli::cli_alert_info("Transformación Clásica sugerida (si usa LM): {.code {sugerencia$transformacion_recomendada}}")
  }
  if (!is.na(sugerencia$boxcox_lambda)) {
    cli::cli_alert_info("Parámetro óptimo Box-Cox (\u03bb): {.val {sugerencia$boxcox_lambda}}")
  }
  cli::cli_alert_info("Fundamento biológico: {sugerencia$justificacion}")
  cat("\n")
  
  class(sugerencia) <- "easy_sugerencia"
  invisible(sugerencia)
}
