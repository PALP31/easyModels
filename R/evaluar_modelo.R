# ── Helper interno ────────────────────────────────────────────────────────────
# Extrae de forma segura un elemento de un objeto (lista o vector nombrado).
# Devuelve NA si el objeto no es un vector/lista, si la clave no existe,
# o si el valor es NA (incluyendo el NA lógico que devuelve performance::icc()
# cuando el modelo es singular).
.safe_get <- function(obj, key) {
  if (is.null(obj)) return(NA)
  # icc()/r2() devuelven logical NA en modelos singulares
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

# ── Función principal ─────────────────────────────────────────────────────────

#' Evaluacion de Diagnostico de Modelo Unificada
#'
#' Esta funcion combina herramientas de \code{performance}, \code{DHARMa} y
#' \code{car} para extraer e imprimir de manera estilizada metricas clave de
#' ajuste de modelos (AIC, BIC, R2, ICC, sobredispersion, singularidad) y
#' lanzar los graficos de diagnostico correspondientes.
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
  
  cli::cli_h1("Evaluacion de Diagnostico de Modelo")
  
  # ── 1. Informacion basica ──────────────────────────────────────────────────
  formula_str <- tryCatch(
    Reduce(paste, deparse(stats::formula(m_nat))),
    error = function(e) "No disponible"
  )
  
  fam_info <- tryCatch(insight::model_info(m_nat), error = function(e) NULL)
  fam      <- if (is.list(fam_info) && !is.null(fam_info$family))        fam_info$family        else "desconocida"
  lnk      <- if (is.list(fam_info) && !is.null(fam_info$link_function)) fam_info$link_function else "desconocido"
  
  cli::cli_h2("Informacion del Modelo")
  cli::cli_alert_info("Clase nativa: {.val {class(m_nat)[1]}}")
  cli::cli_alert_info("Formula: {.code {formula_str}}")
  cli::cli_alert_info("Familia: {.val {fam}} | Link: {.val {lnk}}")
  
  # ── 2. Criterios de ajuste ─────────────────────────────────────────────────
  cli::cli_h2("Metricas de Rendimiento")
  
  aic_val <- tryCatch(round(stats::AIC(m_nat), 2), error = function(e) NA)
  bic_val <- tryCatch(round(stats::BIC(m_nat), 2), error = function(e) NA)
  cli::cli_li("AIC: {.val {aic_val}}")
  cli::cli_li("BIC: {.val {bic_val}}")
  
  # ── R2 (robusto: icc/r2 pueden devolver logical NA en modelos singulares) ──
  r2_val  <- tryCatch(suppressWarnings(performance::r2(m_nat)), error = function(e) NULL)
  r2_cond  <- .safe_get(r2_val, "R2_conditional")
  r2_marg  <- .safe_get(r2_val, "R2_marginal")
  r2_plain <- .safe_get(r2_val, "R2")
  r2_adj   <- .safe_get(r2_val, "R2_adjusted")
  
  if (!is.na(r2_cond)) {
    cli::cli_li("R2 Condicional (fx fijos + aleatorios): {.val {round(r2_cond, 4)}}")
    cli::cli_li("R2 Marginal (solo efectos fijos): {.val {round(r2_marg, 4)}}")
  } else if (!is.na(r2_marg)) {
    cli::cli_li("R2 Marginal (solo efectos fijos): {.val {round(r2_marg, 4)}}")
    cli::cli_li("R2 Condicional: {.val NA (varianza de efectos aleatorios = 0, modelo singular)}")
  } else if (!is.na(r2_plain)) {
    cli::cli_li("R2: {.val {round(r2_plain, 4)}}")
    if (!is.na(r2_adj)) cli::cli_li("R2 Ajustado: {.val {round(r2_adj, 4)}}")
  } else {
    cli::cli_li("R2: {.val No disponible (modelo singular o distribucion no compatible)}")
  }
  
  # ── ICC (robusto ante NA logico devuelto por icc() en modelos singulares) ──
  if (inherits(m_nat, c("merMod", "glmmTMB"))) {
    icc_val <- tryCatch(suppressWarnings(performance::icc(m_nat)), error = function(e) NULL)
    
    icc_adj   <- .safe_get(icc_val, "ICC_adjusted")
    icc_plain <- if (is.na(icc_adj)) .safe_get(icc_val, "ICC") else NA
    final_icc <- if (!is.na(icc_adj)) icc_adj else if (!is.na(icc_plain)) icc_plain else NA
    
    if (!is.na(final_icc)) {
      cli::cli_li("ICC (Correlacion Intraclase): {.val {round(final_icc, 4)}}")
    } else {
      cli::cli_li("ICC (Correlacion Intraclase): {.val No disponible (varianza de efectos aleatorios = 0, modelo singular)}")
    }
    
    # Singularidad
    sing_val <- tryCatch(performance::check_singularity(m_nat), error = function(e) FALSE)
    if (isTRUE(sing_val)) {
      cli::cli_alert_warning(
        "El modelo presenta SINGULARIDAD. Los efectos aleatorios tienen varianza cero. Considere simplificar la estructura aleatoria o usar glmmTMB."
      )
    } else {
      cli::cli_alert_success("El ajuste del modelo no es singular.")
    }
  }
  
  # ── 3. Sobredispersion ─────────────────────────────────────────────────────
  is_count_model <- FALSE
  if (is.list(fam_info)) {
    is_count_model <- isTRUE(fam_info$is_count) ||
      fam %in% c("poisson", "quasipoisson", "negbinom", "nbinom1", "nbinom2")
  }
  
  if (is_count_model) {
    cli::cli_h2("Prueba de Sobredispersion")
    od <- tryCatch(performance::check_overdispersion(m_nat), error = function(e) NULL)
    if (!is.null(od) && is.list(od)) {
      disp_r <- tryCatch(round(od$dispersion_ratio, 3), error = function(e) NA)
      pval   <- tryCatch(round(od$p_value, 4),          error = function(e) NA)
      cli::cli_li("Ratio de dispersion: {.val {disp_r}}")
      cli::cli_li("p-valor (Chi-cuadrado): {.val {pval}}")
      if (!is.na(pval) && pval < 0.05) {
        cli::cli_alert_warning(
          "Sobredispersion significativa detectada! Considere: (1) familia binomial negativa con tipo = 'binomial_negativa', (2) cuasi-Poisson, o (3) un efecto aleatorio por observacion (OLRE)."
        )
      } else {
        cli::cli_alert_success("No se detecto sobredispersion significativa.")
      }
    }
  }
  
  # ── 4. Graficos de diagnostico ─────────────────────────────────────────────
  cli::cli_h2("Generando Graficos de Diagnostico...")
  
  is_gaussian <- is.list(fam_info) && isTRUE(fam_info$is_linear)
  
  if (is_gaussian && !inherits(m_nat, c("glmerMod", "glmmTMB"))) {
    old_par <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::plot(m_nat)
  } else {
    cli::cli_alert_info("Calculando residuos simulados con DHARMa...")
    residuos_dharma <- tryCatch(
      DHARMa::simulateResiduals(m_nat, plot = FALSE),
      error = function(e) {
        cli::cli_alert_warning("DHARMa no pudo calcular residuos: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(residuos_dharma)) {
      graphics::plot(residuos_dharma)
    }
  }
  
  invisible(m_nat)
}
