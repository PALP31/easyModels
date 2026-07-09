#' Calcular odds ratios para modelos binomiales
#'
#' Calcula odds ratios e intervalos de confianza aproximados para modelos
#' binomiales ajustados con \code{glm} o \code{glmer}.
#'
#' @param modelo Modelo binomial ajustado.
#' @param nivel_confianza Nivel de confianza para los intervalos.
#' @param incluir_intercepto Valor logico. Si es \code{FALSE}, elimina el
#'   intercepto de la tabla final.
#'
#' @return Un \code{data.frame} con terminos, log-odds, odds ratios e
#'   intervalos de confianza.
#' @export
analizar_odds_ratio <- function(modelo,
                                nivel_confianza = 0.95,
                                incluir_intercepto = FALSE) {
  fam <- obtener_familia_modelo(modelo)
  if (is.null(fam) || fam$family != "binomial") {
    stop("analizar_odds_ratio() requiere un modelo binomial con link logit.", call. = FALSE)
  }

  if (!identical(fam$link, "logit")) {
    warning("El modelo binomial no usa link logit; los resultados no son odds ratios clasicos.", call. = FALSE)
  }

  beta <- obtener_coeficientes_fijos(modelo)
  matriz_vcov <- as.matrix(stats::vcov(modelo))
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
