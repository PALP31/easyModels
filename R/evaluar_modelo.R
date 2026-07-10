#' Evaluación de Diagnóstico de Modelo Unificada
#'
#' Esta funcion combina herramientas de \code{performance}, \code{DHARMa} y
#' \code{car} para extraer e imprimir de manera estilizada métricas clave de
#' ajuste de modelos (AIC, BIC, R2, ICC, sobredispersión, singularidad) y
#' lanzar los gráficos de diagnóstico correspondientes.
#'
#' @param modelo Un objeto ajustado (de clase \code{easy_model} o un modelo nativo).
#'
#' @return Devuelve de forma invisible el modelo nativo subyacente.
#' @export
#'
#' @importFrom stats AIC BIC residuals fitted formula
#' @importFrom graphics par plot
#' @importFrom performance r2 icc check_singularity check_overdispersion
#' @importFrom DHARMa simulateResiduals
#' @importFrom insight model_info
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_warning cli_alert_success cli_li
#'
#' @examples
#' \dontrun{
#'   model <- lm(Sepal.Length ~ Species, data = iris)
#'   evaluar_modelo(model)
#' }
evaluar_modelo <- function(modelo) {
  m_nat <- extraer_modelo(modelo)
  
  cli::cli_h1("Evaluación de Diagnóstico de Modelo")
  
  # 1. Informacion Basica
  formula_str <- tryCatch({
    Reduce(paste, deparse(stats::formula(m_nat)))
  }, error = function(e) {
    "No disponible"
  })
  
  fam_info <- tryCatch({
    insight::model_info(m_nat)
  }, error = function(e) {
    NULL
  })
  
  fam <- if (!is.null(fam_info$family)) fam_info$family else "desconocida"
  lnk <- if (!is.null(fam_info$link_function)) fam_info$link_function else "desconocido"
  
  cli::cli_h2("Información del Modelo")
  cli::cli_alert_info("Clase nativa: {.val {class(m_nat)[1]}}")
  cli::cli_alert_info("Fórmula: {.code {formula_str}}")
  cli::cli_alert_info("Familia: {.val {fam}} | Link: {.val {lnk}}")
  
  # 2. Criterios de Ajuste
  cli::cli_h2("Métricas de Rendimiento")
  
  aic_val <- tryCatch(stats::AIC(m_nat), error = function(e) NA)
  bic_val <- tryCatch(stats::BIC(m_nat), error = function(e) NA)
  cli::cli_li("AIC: {.val {round(aic_val, 2)}}")
  cli::cli_li("BIC: {.val {round(bic_val, 2)}}")
  
  # R2
  r2_val <- tryCatch(performance::r2(m_nat), error = function(e) NULL)
  if (!is.null(r2_val)) {
    if (!is.null(r2_val$R2_conditional)) {
      cli::cli_li("R2 Condicional (efectos fijos + aleatorios): {.val {round(r2_val$R2_conditional, 4)}}")
      cli::cli_li("R2 Marginal (solo efectos fijos): {.val {round(r2_val$R2_marginal, 4)}}")
    } else if (!is.null(r2_val$R2)) {
      cli::cli_li("R2: {.val {round(r2_val$R2, 4)}}")
      if (!is.null(r2_val$R2_adjusted)) {
        cli::cli_li("R2 Ajustado: {.val {round(r2_val$R2_adjusted, 4)}}")
      }
    }
  }
  
  # ICC si es mixto
  if (inherits(m_nat, c("merMod", "glmmTMB"))) {
    icc_val <- tryCatch(performance::icc(m_nat), error = function(e) NULL)
    if (!is.null(icc_val)) {
      # Extract ICC safely
      val_icc <- if (!is.null(icc_val$ICC_adjusted)) icc_val$ICC_adjusted else icc_val[1]
      cli::cli_li("ICC (Coeficiente de Correlación Intraclase): {.val {round(val_icc, 4)}}")
    }
    
    # Singularidad
    sing_val <- tryCatch(performance::check_singularity(m_nat), error = function(e) FALSE)
    if (isTRUE(sing_val)) {
      cli::cli_alert_warning("El modelo presenta SINGULARIDAD (posible sobreajuste en efectos aleatorios).")
    } else {
      cli::cli_alert_success("El ajuste del modelo no es singular.")
    }
  }
  
  # 3. Sobredispersión para conteos
  is_count_model <- FALSE
  if (!is.null(fam_info)) {
    is_count_model <- isTRUE(fam_info$is_count) || fam %in% c("poisson", "quasipoisson", "negativa_binomial")
  }
  
  if (is_count_model) {
    cli::cli_h2("Prueba de Sobredispersión")
    od <- tryCatch(performance::check_overdispersion(m_nat), error = function(e) NULL)
    if (!is.null(od)) {
      cli::cli_li("Ratio de dispersión: {.val {round(od$dispersion_ratio, 3)}}")
      cli::cli_li("p-valor (Chi-cuadrado): {.val {round(od$p_value, 4)}}")
      if (od$p_value < 0.05) {
        cli::cli_alert_warning("¡Se detectó SOBREDISPERSIÓN significativa en el modelo! Considere usar una familia binomial negativa o cuasi-Poisson.")
      } else {
        cli::cli_alert_success("No se detectó sobredispersión significativa.")
      }
    }
  }
  
  # 4. Gráficos de diagnóstico
  cli::cli_h2("Generando Gráficos de Diagnóstico...")
  
  is_gaussian <- !is.null(fam_info) && isTRUE(fam_info$is_linear)
  if (is_gaussian && !inherits(m_nat, c("glmerMod", "glmmTMB"))) {
    old_par <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::plot(m_nat)
  } else {
    cli::cli_alert_info("Calculando residuos simulados con DHARMa...")
    residuos_dharma <- DHARMa::simulateResiduals(m_nat)
    graphics::plot(residuos_dharma)
  }
  
  invisible(m_nat)
}
