#' Obtener comparaciones multiples post-hoc con ajuste de Tukey
#'
#' Esta funcion realiza comparaciones multiples de medias post-hoc a partir de
#' modelos compatibles con \code{emmeans}: \code{lm}, \code{aov}, \code{glm},
#' \code{lmer}, \code{glmer} y \code{glmer.nb}.
#' Valida que el predictor exista en el modelo y devuelve una tabla limpia con los contrastes pareados
#' y el ajuste de p-valores por el metodo de Tukey.
#'
#' @param modelo Un objeto de modelo ajustado compatible con el paquete \code{emmeans}.
#' @param predictor Una cadena de caracteres que representa el nombre de la variable predictora (factor) sobre la cual se desean calcular las medias marginales estimadas.
#' @param ajuste Metodo de ajuste de comparaciones multiples. Por defecto,
#'   \code{"tukey"}.
#' @param tipo_respuesta Escala del resultado. Use \code{"response"} para
#'   probabilidades, tasas, medias u odds ratios en GLM/GLMM; use \code{"link"}
#'   para la escala lineal.
#' @param infer Valor logico de largo 2 para intervalos y pruebas en
#'   \code{emmeans::summary()}.
#'
#' @return Un \code{data.frame} limpio que contiene las comparaciones por pares, estimaciones, errores estandar, grados de libertad (si aplica), estadisticos de prueba y p-valores ajustados por Tukey.
#' @export
#'
#' @importFrom emmeans emmeans contrast
#' @importFrom stats model.frame formula
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     y = rnorm(100),
#'     grupo = factor(rep(c("A", "B", "C", "D"), each = 25))
#'   )
#'   modelo <- lm(y ~ grupo, data = datos)
#'   obtener_posthoc(modelo, "grupo")
#' }
obtener_posthoc <- function(modelo,
                            predictor,
                            ajuste = "tukey",
                            tipo_respuesta = "response",
                            infer = c(TRUE, TRUE)) {
  message("=== Iniciando Analisis Post-Hoc ===")
  message("Predictor seleccionado: ", predictor)

  validar_predictor_modelo(modelo, predictor)
  
  message("Calculando Medias Marginales Estimadas (EMMeans)...")
  em <- emmeans::emmeans(modelo, specs = predictor)
  
  message("Calculando contrastes pareados con ajuste: ", ajuste)
  contrastes <- emmeans::contrast(em, method = "pairwise", adjust = ajuste)
  
  tabla_limpia <- as.data.frame(
    summary(contrastes, infer = infer, type = tipo_respuesta)
  )
  
  message("Comparaciones multiples calculadas con exito.")
  message("==========================================")
  
  return(tabla_limpia)
}
