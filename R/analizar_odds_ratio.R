#' Calcular odds ratios para modelos binomiales
#'
#' Calcula odds ratios e intervalos de confianza para modelos
#' binomiales unificados de clase \code{easy_model} o ajustados nativamente con \code{glm}, \code{glmer} o \code{glmmTMB}.
#'
#' @param modelo Modelo binomial ajustado (clase \code{easy_model} o nativa).
#' @param nivel_confianza Nivel de confianza para los intervalos (por defecto \code{0.95}).
#' @param incluir_intercepto Valor logico. Si es \code{FALSE} (predeterminado), excluye el
#'   intercepto de la tabla final.
#'
#' @return Un \code{data.frame} con terminos, log-odds, errores estandar (SE), odds ratios (OR) e
#'   intervalos de confianza (\code{IC_inf}, \code{IC_sup}).
#' @export
#' @importFrom stats vcov qnorm
#' @importFrom cli cli_abort cli_warn
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_glm(iris, I(Species == "setosa") ~ Sepal.Length, familia = "binomial")
#'   analizar_odds_ratio(modelo)
#' }
analizar_odds_ratio <- function(modelo,
                                nivel_confianza = 0.95,
                                incluir_intercepto = FALSE) {
  m_nat <- extraer_modelo(modelo)
  
  fam <- obtener_familia_modelo(m_nat)
  fam_name <- if (is.list(fam)) fam$family else if (is.character(fam)) fam else NULL
  fam_link <- if (is.list(fam)) fam$link else NULL
  
  if (is.null(fam_name) || !fam_name %in% c("binomial", "quasibinomial")) {
    fam_info <- tryCatch(insight::model_info(m_nat), error = function(e) NULL)
    if (is.list(fam_info) && isTRUE(fam_info$is_binomial)) {
      fam_name <- "binomial"
      fam_link <- fam_info$link_function
    }
  }

  if (is.null(fam_name) || !fam_name %in% c("binomial", "quasibinomial")) {
    cli::cli_abort("analizar_odds_ratio() requiere un modelo binomial o quasibinomial.")
  }

  if (!identical(fam_link, "logit")) {
    cli::cli_warn("El modelo binomial no usa link logit (link: '{fam_link}'); los coeficientes exponenciados no representan odds ratios clasicos.")
  }

  beta <- obtener_coeficientes_fijos(m_nat)
  
  matriz_vcov <- tryCatch({
    if (inherits(m_nat, "glmmTMB")) {
      as.matrix(stats::vcov(m_nat)$cond)
    } else {
      as.matrix(stats::vcov(m_nat))
    }
  }, error = function(e) {
    as.matrix(stats::vcov(m_nat))
  })
  
  se <- sqrt(diag(matriz_vcov))[names(beta)]
  alfa <- 1 - nivel_confianza
  z <- stats::qnorm(1 - alfa / 2)

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
