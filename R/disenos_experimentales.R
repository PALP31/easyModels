#' Analizar un diseño de Bloques Completos al Azar (RCBD)
#'
#' Esta funcion es un wrapper especializado de \code{analizar_lmm} para ajustar y
#' analizar un diseño de Bloques Completos al Azar (RCBD - Randomized Complete Block Design).
#' Agrega automaticamente la estructura de efectos aleatorios para el factor de bloqueo
#' (ej. \code{(1 | bloque)}).
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una formula de R o una cadena de caracteres para los efectos fijos (ej. \code{y ~ tratamiento}).
#' @param bloque Nombre de la columna en el \code{data.frame} que identifica los bloques (como factor o cadena de texto).
#' @param REML Valor logico. Use \code{FALSE} para comparar modelos y \code{TRUE} para estimaciones precisas de varianza.
#' @param diagnosticos Valor logico. Si es \code{TRUE}, genera graficos de diagnostico de residuos.
#'
#' @return Un objeto de la clase \code{merMod} correspondiente al modelo ajustado.
#' @export
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     rendimiento = rnorm(30, mean = 10),
#'     tratamiento = factor(rep(c("Control", "Trat1", "Trat2"), each = 10)),
#'     Bloque = factor(rep(1:10, times = 3))
#'   )
#'   analizar_bloques_azar(datos, rendimiento ~ tratamiento, "Bloque")
#' }
analizar_bloques_azar <- function(datos, formula_fijos, bloque, REML = TRUE, diagnosticos = TRUE) {
  message("=== Iniciando Ajuste de Diseño de Bloques Completos al Azar (RCBD) ===")
  
  if (!is.character(bloque) || length(bloque) != 1) {
    stop("El argumento 'bloque' debe ser el nombre de una unica columna en 'datos'.", call. = FALSE)
  }
  
  if (!(bloque %in% names(datos))) {
    stop(paste0("El bloque '", bloque, "' no existe en los datos."), call. = FALSE)
  }
  
  aleatorios <- paste0("(1 | ", bloque, ")")
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  return(modelo)
}

#' Analizar un diseño de Parcelas Divididas (Split-Plot)
#'
#' Esta funcion ajusta y analiza un modelo lineal mixto para un diseño de
#' parcelas divididas (Split-Plot), comun en experimentos biologicos y agricolas
#' (por ejemplo, donde un factor de estres como temperatura se aplica a la parcela
#' principal y otro factor como genotipo a la subparcela).
#' Automatiza la especificacion de efectos aleatorios agregando el error de la
#' parcela principal: \code{(1 | bloque) + (1 | bloque:parcela_principal)}.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una formula de R o una cadena de caracteres para la parte de efectos fijos (ej. \code{y ~ principal * subparcela}).
#' @param bloque Nombre de la columna que identifica el factor de bloque (ej. \code{"Bloque"}).
#' @param parcela_principal Nombre de la columna que identifica el factor asignado a la parcela principal (ej. \code{"Riego"}).
#' @param REML Valor logico. Use \code{TRUE} para estimaciones finales de componentes de varianza.
#' @param diagnosticos Valor logico. Si es \code{TRUE}, genera graficos de diagnostico de residuos.
#'
#' @return Un objeto de la clase \code{merMod} correspondiente al modelo ajustado.
#' @export
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     rendimiento = rnorm(48),
#'     Riego = factor(rep(c("Riego", "Secano"), each = 24)),
#'     Genotipo = factor(rep(rep(c("G1", "G2", "G3"), each = 8), times = 2)),
#'     Bloque = factor(rep(1:4, times = 12))
#'   )
#'   # El riego se aplica a nivel de parcela principal y el genotipo a nivel de subparcela
#'   analizar_parcelas_divididas(
#'     datos = datos,
#'     formula_fijos = rendimiento ~ Riego * Genotipo,
#'     bloque = "Bloque",
#'     parcela_principal = "Riego"
#'   )
#' }
analizar_parcelas_divididas <- function(datos, formula_fijos, bloque, parcela_principal, REML = TRUE, diagnosticos = TRUE) {
  message("=== Iniciando Ajuste de Diseño de Parcelas Divididas (Split-Plot) ===")
  
  if (!is.character(bloque) || length(bloque) != 1) {
    stop("El argumento 'bloque' debe ser el nombre de una unica columna en 'datos'.", call. = FALSE)
  }
  
  if (!(bloque %in% names(datos))) {
    stop(paste0("El bloque '", bloque, "' no existe en los datos."), call. = FALSE)
  }
  
  if (!is.character(parcela_principal) || length(parcela_principal) != 1) {
    stop("El argumento 'parcela_principal' debe ser el nombre de una unica columna en 'datos'.", call. = FALSE)
  }
  
  if (!(parcela_principal %in% names(datos))) {
    stop(paste0("La parcela principal '", parcela_principal, "' no existe en los datos."), call. = FALSE)
  }
  
  # Error de parcela principal: bloque:parcela_principal
  # Representa la restriccion de aleatorizacion de la parcela principal dentro de cada bloque.
  aleatorios <- paste0("(1 | ", bloque, ") + (1 | ", bloque, ":", parcela_principal, ")")
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  return(modelo)
}
