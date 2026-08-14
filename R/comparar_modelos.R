#' Comparar múltiples modelos estadísticos (Selección de Modelos)
#'
#' Esta función compara múltiples modelos estadísticos (\code{easy_model}, \code{lm}, \code{glm}, \code{lmer}, \code{glmmTMB})
#' calculando criterios de información de Akaike (AIC, BIC), log-verosimilitud (LogLik),
#' diferencias delta-AIC (\eqn{\Delta AIC}), pesos de Akaike (\eqn{w_i}) y coeficientes de determinación (\eqn{R^2}).
#'
#' @param ... Uno o más modelos ajustados (de clase \code{easy_model} o nativos), o una lista de modelos.
#' @param nombres Vector opcional de caracteres con los nombres descriptivos para cada modelo.
#' @param criterio Criterio principal de ordenamiento. Opciones: \code{"AIC"} (por defecto), \code{"BIC"} o \code{"R2"}.
#'
#' @return Un \code{data.frame} de clase \code{c("easy_comparacion", "data.frame")} ordenado según el mejor ajuste.
#' @export
#'
#' @importFrom stats AIC BIC logLik
#' @importFrom performance r2
#' @importFrom cli cli_h1 cli_alert_success cli_alert_info boxx
#'
#' @examples
#' \dontrun{
#'   m1 <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   m2 <- analizar_lm(iris, Sepal.Length ~ Species + Sepal.Width, diagnosticos = FALSE)
#'   m3 <- analizar_lm(iris, Sepal.Length ~ Species * Sepal.Width, diagnosticos = FALSE)
#'   comparar_modelos(m1, m2, m3, nombres = c("Simple", "Aditivo", "Interaccion"))
#' }
comparar_modelos <- function(..., nombres = NULL, criterio = c("AIC", "BIC", "R2")) {
  criterio <- match.arg(criterio)
  
  # Recoger modelos
  lista_mod <- list(...)
  if (length(lista_mod) == 1 && is.list(lista_mod[[1]]) && !inherits(lista_mod[[1]], c("easy_model", "lm", "merMod", "glmmTMB"))) {
    lista_mod <- lista_mod[[1]]
  }
  
  k <- length(lista_mod)
  if (k < 2) {
    stop("Debe proporcionar al menos 2 modelos para realizar la comparacion.", call. = FALSE)
  }
  
  if (is.null(nombres)) {
    llamadas <- as.character(match.call(expand.dots = FALSE)$...)
    if (length(llamadas) == k) {
      nombres <- llamadas
    } else {
      nombres <- paste0("Modelo_", seq_len(k))
    }
  }
  
  tab_res <- data.frame(
    Modelo = nombres,
    Tipo = character(k),
    K = integer(k),
    LogLik = numeric(k),
    AIC = numeric(k),
    delta_AIC = numeric(k),
    Peso_AIC = numeric(k),
    BIC = numeric(k),
    R2_Marginal = numeric(k),
    R2_Condicional = numeric(k),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(k)) {
    obj <- lista_mod[[i]]
    m_nat <- extraer_modelo(obj)
    
    # Tipo de modelo
    tipo <- if (inherits(obj, "easy_model")) obj$tipo_modelo else class(m_nat)[1]
    tab_res$Tipo[i] <- tipo
    
    # Parametros y LogLik
    ll <- tryCatch(as.numeric(stats::logLik(m_nat)), error = function(e) NA)
    df_p <- tryCatch(attr(stats::logLik(m_nat), "df"), error = function(e) NA)
    tab_res$LogLik[i] <- if (!is.na(ll)) round(ll, 2) else NA
    tab_res$K[i] <- if (!is.na(df_p)) as.integer(df_p) else NA
    
    # AIC y BIC
    aic_val <- tryCatch(as.numeric(stats::AIC(m_nat)), error = function(e) NA)
    bic_val <- tryCatch(as.numeric(stats::BIC(m_nat)), error = function(e) NA)
    tab_res$AIC[i] <- if (!is.na(aic_val)) round(aic_val, 2) else NA
    tab_res$BIC[i] <- if (!is.na(bic_val)) round(bic_val, 2) else NA
    
    # R2
    r2_val <- tryCatch(suppressWarnings(performance::r2(m_nat)), error = function(e) NULL)
    r2_cond <- .safe_get(r2_val, "R2_conditional")
    r2_marg <- .safe_get(r2_val, "R2_marginal")
    r2_plain <- .safe_get(r2_val, "R2")
    
    if (!is.na(r2_cond)) {
      tab_res$R2_Condicional[i] <- round(r2_cond, 3)
      tab_res$R2_Marginal[i] <- round(r2_marg, 3)
    } else if (!is.na(r2_plain)) {
      tab_res$R2_Marginal[i] <- round(r2_plain, 3)
      tab_res$R2_Condicional[i] <- NA
    } else {
      tab_res$R2_Marginal[i] <- NA
      tab_res$R2_Condicional[i] <- NA
    }
  }
  
  # Calcular Delta AIC y Pesos de Akaike
  min_aic <- min(tab_res$AIC, na.rm = TRUE)
  tab_res$delta_AIC <- round(tab_res$AIC - min_aic, 2)
  
  raw_weights <- exp(-0.5 * tab_res$delta_AIC)
  tab_res$Peso_AIC <- round(raw_weights / sum(raw_weights, na.rm = TRUE), 3)
  
  # Ordenar según criterio
  if (criterio == "AIC") {
    tab_res <- tab_res[order(tab_res$AIC), ]
  } else if (criterio == "BIC") {
    tab_res <- tab_res[order(tab_res$BIC), ]
  } else if (criterio == "R2") {
    tab_res <- tab_res[order(tab_res$R2_Marginal, decreasing = TRUE), ]
  }
  rownames(tab_res) <- NULL
  
  # Reporte en CLI
  cli::cli_h1("Tabla de Comparación y Selección de Modelos (easyModels)")
  mejor_modelo <- tab_res$Modelo[1]
  cli::cli_alert_success("Mejor modelo según {criterio}: {.strong {mejor_modelo}} (AIC = {tab_res$AIC[1]}, \u0394AIC = 0.00, Peso = {tab_res$Peso_AIC[1]})")
  cat("\n")
  print(tab_res)
  cat("\n")
  
  class(tab_res) <- c("easy_comparacion", "data.frame")
  invisible(tab_res)
}
