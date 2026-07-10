#' Ajustar y analizar un modelo lineal (LM)
#'
#' Ajusta un modelo lineal con \code{stats::lm}, imprime el resumen y, de forma
#' opcional, genera diagnósticos clásicos para modelos gaussianos. Retorna un objeto unificado de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula Una fórmula de R para el modelo lineal.
#' @param diagnosticos Valor lógico. Si es \code{TRUE}, muestra gráficos de
#'   residuos, Q-Q plot, escala-localización y distancia de Cook.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#' @importFrom stats lm
#' @importFrom graphics par plot
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Species)
#'   summary(modelo)
#' }
analizar_lm <- function(datos, formula, diagnosticos = TRUE) {
  cli::cli_alert_info("Iniciando ajuste de LM...")
  
  modelo_nat <- tryCatch({
    stats::lm(formula = formula, data = datos)
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo lineal: {e$message}")
  })

  em <- crear_easy_model(modelo_nat, tipo_modelo = "LM", datos = datos)

  if (isTRUE(diagnosticos)) {
    cli::cli_alert_info("Generando gráficos de diagnóstico clásicos para LM...")
    old_par <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::plot(modelo_nat)
  }

  print(em)
  return(em)
}
