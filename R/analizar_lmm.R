#' Ajustar y analizar un Modelo Lineal Mixto (LMM)
#'
#' Esta funcion ajusta un modelo lineal mixto (LMM) utilizando \code{lme4::lmer}.
#' Adicionalmente, genera graficos de diagnostico de residuos (Homocedasticidad y Q-Q plot)
#' en una sola ventana grafica, imprime el resumen (\code{summary}) en la consola y
#' devuelve el objeto del modelo ajustado para su uso en analisis posteriores (ej. post-hoc).
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una formula de R o una cadena de caracteres para la parte de efectos fijos (ej. \code{y ~ x1}).
#' @param aleatorios Una cadena de caracteres que define la estructura de efectos aleatorios (ej. \code{"(1 | bloque)"}).
#' @param REML Valor logico. Use \code{FALSE} para comparar modelos con
#'   distintos efectos fijos y \code{TRUE} para el modelo final gaussiano.
#' @param diagnosticos Valor logico. Si es \code{TRUE}, muestra graficos de
#'   diagnostico de residuos.
#'
#' @return Un objeto de la clase \code{merMod} correspondiente al modelo ajustado.
#' @export
#'
#' @importFrom lme4 lmer
#' @importFrom stats as.formula residuals fitted qqnorm qqline
#' @importFrom graphics par plot abline
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     y = rnorm(100),
#'     x = rnorm(100),
#'     bloque = factor(rep(1:10, each = 10))
#'   )
#'   analizar_lmm(datos, y ~ x, "(1 | bloque)")
#' }
analizar_lmm <- function(datos, formula_fijos, aleatorios, REML = TRUE, diagnosticos = TRUE) {
  message("=== Iniciando Ajuste de LMM ===")
  formula_completa <- construir_formula_mixta(formula_fijos, aleatorios)
  
  message("Formula construida: ", Reduce(paste, deparse(formula_completa)))
  message("Ajustando modelo con lme4::lmer... REML = ", REML)
  
  # Ajuste del modelo
  modelo <- lme4::lmer(formula_completa, data = datos, REML = REML)
  
  # Configuracion y generacion de graficos de diagnostico
  if (isTRUE(diagnosticos)) {
    message("Generando graficos de diagnostico (Homocedasticidad y Q-Q Plot)...")
    old_par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::plot(
      stats::fitted(modelo),
      stats::residuals(modelo),
      xlab = "Valores Ajustados",
      ylab = "Residuos",
      main = "Homocedasticidad",
      pch = 20,
      col = "darkcyan"
    )
    graphics::abline(h = 0, col = "firebrick", lty = 2, lwd = 2)

    stats::qqnorm(
      stats::residuals(modelo),
      main = "Normal Q-Q Plot de Residuos",
      pch = 20,
      col = "darkcyan"
    )
    stats::qqline(stats::residuals(modelo), col = "firebrick", lwd = 2)
  }
  
  message("Modelo LMM ajustado correctamente. Resumen del modelo:")
  message("====================================================")
  print(summary(modelo))
  message("====================================================")
  
  return(modelo)
}
