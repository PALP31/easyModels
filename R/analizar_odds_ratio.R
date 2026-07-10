#' Calcular odds ratios para modelos binomiales
#'
#' Calcula odds ratios e intervalos de confianza aproximados para modelos
#' binomiales unificados de clase \code{easy_model} o ajustados nativamente con \code{glm} o \code{glmer}.
#'
#' @param modelo Modelo binomial ajustado (clase \code{easy_model} o nativa).
#' @param nivel_confianza Nivel de confianza para los intervalos.
#' @param incluir_intercepto Valor logico. Si es \code{FALSE}, elimina el
#'   intercepto de la tabla final.
#'
#' @return Un \code{data.frame} con terminos, log-odds, odds ratios e
#'   intervalos de confianza.
#' @export
#' @importFrom stats vcov qnorm
#' @importFrom cli cli_abort cli_warn
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_glm(iris, Species == "setosa" ~ Sepal.Length, familia = "binomial")
#'   analizar_odds_ratio(modelo)
#' }
analizar_odds_ratio <- function(modelo,
                                nivel_confianza = 0.95,
                                incluir_intercepto = FALSE) {
  m_nat <- extraer_modelo(modelo)
  
  fam <- obtener_familia_modelo(m_nat)
  if (is.null(fam) || fam$family != "binomial") {
    cli::cli_abort("analizar_odds_ratio() requiere un modelo binomial con link logit.")
  }

  if (!identical(fam$link, "logit")) {
    cli::cli_warn("El modelo binomial no usa link logit; los resultados no son odds ratios clásicos.")
  }

  beta <- obtener_coeficientes_fijos(m_nat)
  matriz_vcov <- as.matrix(stats::vcov(m_nat))
  se <- sqrt(diag(matriz_vcov))[names(beta)]
  z <- stats::qnorm(1 - (1 - nivel_confianza) / 2)

  tabla <- data.frame(
    termino = names(beta),
    log_odds = as.numeric(beta),
    SE = as.numeric(se),
    OR = exp(as.numeric(beta)),
    IC_inf = exp(as.numeric(beta) - z * as.numeric(se)),
    IC_sup = exp(as.numeric(beta) + z * as.numeric(se)),
    row.names = NULL
  )

  if (!isTRUE(incluir_intercepto)) {
    tabla <- tabla[tabla$termino != "(Intercept)", , drop = FALSE]
  }

  return(tabla)
}
