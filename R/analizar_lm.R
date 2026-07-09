#' Ajustar y analizar un modelo lineal
#'
#' Ajusta un modelo lineal con \code{stats::lm}, imprime el resumen y, de forma
#' opcional, genera diagnosticos clasicos para modelos gaussianos.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula Una formula de R para el modelo lineal.
#' @param diagnosticos Valor logico. Si es \code{TRUE}, muestra graficos de
#'   residuos, Q-Q plot, escala-localizacion y distancia de Cook.
#'
#' @return Un objeto de clase \code{lm}.
#' @export
analizar_lm <- function(datos, formula, diagnosticos = TRUE) {
  message("=== Iniciando ajuste de LM ===")
  modelo <- stats::lm(formula = formula, data = datos)

  if (isTRUE(diagnosticos)) {
    message("Generando diagnosticos clasicos para LM...")
    old_par <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::plot(modelo)
  }

  message("Modelo LM ajustado correctamente. Resumen:")
  message("==========================================")
  print(summary(modelo))
  message("==========================================")

  return(modelo)
}
