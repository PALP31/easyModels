#' Ajustar y analizar un Modelo Lineal Mixto (LMM)
#'
#' Esta funcion ajusta un modelo lineal mixto (LMM) utilizando \code{lme4::lmer}.
#' Adicionalmente, genera gráficos de diagnóstico de residuos (Homocedasticidad y Q-Q plot)
#' en una sola ventana gráfica y devuelve un objeto unificado de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una fórmula de R o una cadena de caracteres para la parte de efectos fijos (ej. \code{y ~ x1}).
#' @param aleatorios Una cadena de caracteres que define la estructura de efectos aleatorios (ej. \code{"(1 | bloque)"}).
#' @param REML Valor lógico. Use \code{FALSE} para comparar modelos con
#'   distintos efectos fijos y \code{TRUE} para el modelo final gaussiano.
#' @param diagnosticos Valor lógico. Si es \code{TRUE}, muestra gráficos de
#'   diagnóstico de residuos.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#' @importFrom lme4 lmer
#' @importFrom stats as.formula residuals fitted qqnorm qqline
#' @importFrom graphics par plot abline
#' @importFrom cli cli_alert_info cli_alert_warning cli_abort
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     y = rnorm(100),
#'     x = rnorm(100),
#'     bloque = factor(rep(1:10, each = 10))
#'   )
#'   modelo <- analizar_lmm(datos, y ~ x, "(1 | bloque)")
#'   summary(modelo)
#' }
analizar_lmm <- function(datos, formula_fijos, aleatorios, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("Iniciando Ajuste de LMM...")
  formula_completa <- construir_formula_mixta(formula_fijos, aleatorios)
  
  cli::cli_alert_info("Fórmula construida: {.code {Reduce(paste, deparse(formula_completa))}}")
  cli::cli_alert_info("Ajustando modelo con lme4::lmer... REML = {REML}")
  
  # Ajuste del modelo
  modelo_nat <- tryCatch({
    lme4::lmer(formula_completa, data = datos, REML = REML)
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo LMM: {e$message}")
  })
  
  em <- crear_easy_model(modelo_nat, tipo_modelo = "LMM", datos = datos)

  # Configuración y generación de gráficos de diagnóstico
  if (isTRUE(diagnosticos)) {
    cli::cli_alert_info("Generando gráficos de diagnóstico (Homocedasticidad y Q-Q Plot)...")
    old_par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::plot(
      stats::fitted(modelo_nat),
      stats::residuals(modelo_nat),
      xlab = "Valores Ajustados",
      ylab = "Residuos",
      main = "Homocedasticidad",
      pch = 20,
      col = "darkcyan"
    )
    graphics::abline(h = 0, col = "firebrick", lty = 2, lwd = 2)

    stats::qqnorm(
      stats::residuals(modelo_nat),
      main = "Normal Q-Q Plot de Residuos",
      pch = 20,
      col = "darkcyan"
    )
    stats::qqline(stats::residuals(modelo_nat), col = "firebrick", lwd = 2)
  }
  
  print(em)
  return(em)
}
