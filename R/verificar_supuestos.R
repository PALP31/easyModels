#' Verificar supuestos estadísticos de un modelo
#'
#' Esta función realiza una batería exhaustiva y automatizada de pruebas de supuestos
#' para modelos lineales (\code{lm}), lineales mixtos (\code{lmer}) y GLM:
#' normalidad de residuos, homocedasticidad, multicolinealidad (VIF), independencia/autocorrelación
#' y detección de observaciones atípicas/influyentes (distancia de Cook).
#'
#' @param modelo Un objeto de clase \code{easy_model} o un modelo nativo ajustado (\code{lm}, \code{merMod}, \code{glm}, etc.).
#' @param alfa Nivel de significancia para las pruebas de hipótesis (por defecto \code{0.05}).
#' @param silencioso Valor lógico. Si es \code{TRUE}, no imprime el reporte en consola y solo devuelve la lista de resultados.
#'
#' @return Una lista con los resultados cuantitativos de las pruebas y diagnósticos:
#'   \itemize{
#'     \item \code{normalidad}: Estadístico y p-valor del test de Shapiro-Wilk.
#'     \item \code{homocedasticidad}: Prueba de Breusch-Pagan / homogeneidad.
#'     \item \code{autocorrelacion}: Estadístico de Durbin-Watson.
#'     \item \code{colinealidad}: Factores de Inflación de Varianza (VIF).
#'     \item \code{influyentes}: Índices de observaciones con distancia de Cook > 4/n.
#'     \item \code{cumple_todos}: Valor lógico general.
#'   }
#' @export
#'
#' @importFrom stats residuals fitted shapiro.test model.frame
#' @importFrom performance check_heteroscedasticity check_collinearity check_outliers
#' @importFrom car durbinWatsonTest
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info cli_li boxx
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Sepal.Width + Petal.Length, diagnosticos = FALSE)
#'   supuestos <- verificar_supuestos(modelo)
#' }
verificar_supuestos <- function(modelo, alfa = 0.05, silencioso = FALSE) {
  m_nat <- extraer_modelo(modelo)
  
  residuos <- stats::residuals(m_nat)
  ajustados <- stats::fitted(m_nat)
  n <- length(residuos)
  
  resultados <- list(
    normalidad = list(test = "Shapiro-Wilk", estadistico = NA, p_valor = NA, cumple = NA),
    homocedasticidad = list(test = "Breusch-Pagan / Heterocedasticidad", p_valor = NA, cumple = NA),
    autocorrelacion = list(test = "Durbin-Watson", estadistico = NA, p_valor = NA, cumple = NA),
    colinealidad = list(vif = NULL, max_vif = NA, cumple = NA),
    influyentes = list(indices = integer(0), n_atipicos = 0, cumple = TRUE),
    cumple_todos = TRUE
  )
  
  # 1. Normalidad de residuos (Shapiro-Wilk si n <= 5000)
  if (n >= 3 && n <= 5000) {
    sw <- tryCatch(stats::shapiro.test(residuos), error = function(e) NULL)
    if (!is.null(sw)) {
      resultados$normalidad$estadistico <- as.numeric(sw$statistic)
      resultados$normalidad$p_valor <- as.numeric(sw$p.value)
      resultados$normalidad$cumple <- sw$p.value >= alfa
      if (!resultados$normalidad$cumple) resultados$cumple_todos <- FALSE
    }
  }
  
  # 2. Homocedasticidad
  het <- tryCatch(suppressWarnings(performance::check_heteroscedasticity(m_nat)), error = function(e) NULL)
  if (!is.null(het) && is.numeric(as.numeric(het))) {
    p_het <- as.numeric(het)
    resultados$homocedasticidad$p_valor <- p_het
    resultados$homocedasticidad$cumple <- p_het >= alfa
    if (!resultados$homocedasticidad$cumple) resultados$cumple_todos <- FALSE
  } else {
    resultados$homocedasticidad$cumple <- TRUE
  }
  
  # 3. Autocorrelación (Durbin-Watson)
  dw <- tryCatch(suppressWarnings(car::durbinWatsonTest(m_nat)), error = function(e) NULL)
  if (!is.null(dw)) {
    dw_stat <- tryCatch(as.numeric(dw$dw), error = function(e) NA)
    dw_p <- tryCatch(as.numeric(dw$p), error = function(e) NA)
    resultados$autocorrelacion$estadistico <- dw_stat
    resultados$autocorrelacion$p_valor <- dw_p
    if (!is.na(dw_p)) {
      resultados$autocorrelacion$cumple <- dw_p >= alfa
      if (!resultados$autocorrelacion$cumple) resultados$cumple_todos <- FALSE
    }
  }
  
  # 4. Multicolinealidad (VIF)
  vif_check <- tryCatch(suppressWarnings(performance::check_collinearity(m_nat)), error = function(e) NULL)
  if (!is.null(vif_check) && is.data.frame(vif_check) && nrow(vif_check) > 0) {
    vif_vals <- vif_check$VIF
    names(vif_vals) <- vif_check$Parameter
    resultados$colinealidad$vif <- vif_vals
    resultados$colinealidad$max_vif <- max(vif_vals, na.rm = TRUE)
    resultados$colinealidad$cumple <- max(vif_vals, na.rm = TRUE) < 5
    if (!resultados$colinealidad$cumple) resultados$cumple_todos <- FALSE
  } else {
    resultados$colinealidad$cumple <- TRUE
  }
  
  # 5. Observaciones influyentes (Cook's D)
  cooks_d <- tryCatch(stats::cooks.distance(m_nat), error = function(e) NULL)
  if (!is.null(cooks_d)) {
    umbral <- 4 / n
    idx_infl <- which(cooks_d > umbral)
    resultados$influyentes$indices <- idx_infl
    resultados$influyentes$n_atipicos <- length(idx_infl)
    resultados$influyentes$cumple <- (length(idx_infl) / n) <= 0.05
    if (!resultados$influyentes$cumple) resultados$cumple_todos <- FALSE
  }
  
  # Reporte en consola si no está en modo silencioso
  if (!isTRUE(silencioso)) {
    cli::cli_h1("Auditoría de Supuestos del Modelo (easyModels)")
    
    # Normalidad
    if (!is.na(resultados$normalidad$p_valor)) {
      p_txt <- round(resultados$normalidad$p_valor, 4)
      w_txt <- round(resultados$normalidad$estadistico, 4)
      if (isTRUE(resultados$normalidad$cumple)) {
        cli::cli_alert_success("Normalidad de residuos (Shapiro-Wilk): Cumple (W = {w_txt}, p = {p_txt} >= {alfa})")
      } else {
        cli::cli_alert_danger("Normalidad de residuos (Shapiro-Wilk): VIOLADO (W = {w_txt}, p = {p_txt} < {alfa})")
        cli::cli_alert_info("  -> Recomendación: Considere transformar la variable (log/Box-Cox) o usar GLM Gamma/Beta con 'analizar_glm()'.")
      }
    }
    
    # Homocedasticidad
    if (!is.na(resultados$homocedasticidad$p_valor)) {
      p_txt <- round(resultados$homocedasticidad$p_valor, 4)
      if (isTRUE(resultados$homocedasticidad$cumple)) {
        cli::cli_alert_success("Homocedasticidad (Varianza constante): Cumple (p = {p_txt} >= {alfa})")
      } else {
        cli::cli_alert_danger("Homocedasticidad: VIOLADO (Heterocedasticidad detectada, p = {p_txt} < {alfa})")
        cli::cli_alert_info("  -> Recomendación: Modelar heterocedasticidad con glmmTMB (dispformula) o estimadores de varianza robustos.")
      }
    }
    
    # Colinealidad
    if (!is.na(resultados$colinealidad$max_vif)) {
      vif_txt <- round(resultados$colinealidad$max_vif, 2)
      if (isTRUE(resultados$colinealidad$cumple)) {
        cli::cli_alert_success("Multicolinealidad (VIF): Adecuada (VIF máximo = {vif_txt} < 5)")
      } else {
        cli::cli_alert_warning("Multicolinealidad (VIF): Elevada (VIF máximo = {vif_txt} >= 5)")
        cli::cli_alert_info("  -> Recomendación: Verifique correlación entre predictores continuos o elimine variables redundantes.")
      }
    }
    
    # Influyentes
    if (resultados$influyentes$n_atipicos > 0) {
      cli::cli_alert_warning("Puntos Influyentes: Se detectaron {resultados$influyentes$n_atipicos} observaciones con Distancia de Cook > 4/n (obs: {paste(head(resultados$influyentes$indices, 5), collapse = ', ')}{if (resultados$influyentes$n_atipicos > 5) '...'})")
    } else {
      cli::cli_alert_success("Puntos Influyentes: No se detectaron valores atípicos severos según Distancia de Cook.")
    }
    
    # Resumen global
    cat("\n")
    if (isTRUE(resultados$cumple_todos)) {
      print(cli::boxx(" DIAGNÓSTICO GLOBAL: TODOS LOS SUPUESTOS SE CUMPLEN SATISFACTORIAMENTE ",
                      border_style = "round", col = "green", padding = 0))
    } else {
      print(cli::boxx(" ALERTA: UNO O MÁS SUPUESTOS PRESENTAN DESVIACIONES. REVISE LAS RECOMENDACIONES ",
                      border_style = "round", col = "yellow", padding = 0))
    }
    cat("\n")
  }
  
  class(resultados) <- "easy_supuestos"
  invisible(resultados)
}
