#' Ajustar y analizar un modelo lineal generalizado
#'
#' Ajusta modelos GLM gaussianos, binomiales, Poisson, cuasi-Poisson,
#' cuasi-binomiales o binomial negativa. Para modelos de conteo calcula
#' sobredispersion; para binomiales imprime odds ratios; y puede ejecutar
#' diagnosticos con \code{DHARMa}.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula Una formula de R para el GLM.
#' @param familia Familia del modelo. Puede ser \code{"gaussian"},
#'   \code{"binomial"}, \code{"poisson"}, \code{"quasibinomial"},
#'   \code{"quasipoisson"} o \code{"negativa_binomial"}.
#' @param diagnostico_dharma Valor logico. Si es \code{TRUE}, genera
#'   diagnosticos de residuos simulados con \code{DHARMa}.
#'
#' @return Un objeto de clase \code{glm} o \code{negbin}.
#' @export
analizar_glm <- function(datos,
                         formula,
                         familia = c(
                           "gaussian",
                           "binomial",
                           "poisson",
                           "quasibinomial",
                           "quasipoisson",
                           "negativa_binomial"
                         ),
                         diagnostico_dharma = TRUE) {
  message("=== Iniciando ajuste de GLM ===")
  familia <- match.arg(familia)

  if (familia == "negativa_binomial") {
    message("Familia seleccionada: Binomial negativa (MASS::glm.nb)")
    modelo <- MASS::glm.nb(formula = formula, data = datos)
  } else {
    fam <- switch(
      familia,
      gaussian = stats::gaussian(),
      binomial = stats::binomial(),
      poisson = stats::poisson(),
      quasibinomial = stats::quasibinomial(),
      quasipoisson = stats::quasipoisson()
    )
    message("Familia seleccionada: ", familia)
    modelo <- stats::glm(formula = formula, data = datos, family = fam)
  }

  if (familia %in% c("poisson", "quasipoisson", "negativa_binomial")) {
    dispersion <- calcular_sobredispersion(modelo)
    message(sprintf("Ratio de sobredispersion (Pearson Chi2/GL): %.3f", dispersion$ratio))
    if (dispersion$ratio > 1.5 && familia == "poisson") {
      message("Sugerencia: hay sobredispersion. Considere familia = 'quasipoisson' o 'negativa_binomial'.")
    }
  }

  if (familia %in% c("binomial", "quasibinomial")) {
    message("Odds ratios de efectos fijos:")
    print(analizar_odds_ratio(modelo))
  }

  if (isTRUE(diagnostico_dharma)) {
    message("Generando diagnosticos DHARMa de residuos simulados...")
    residuos_dharma <- DHARMa::simulateResiduals(modelo)
    plot(residuos_dharma)
    print(DHARMa::testDispersion(residuos_dharma))
    print(DHARMa::testZeroInflation(residuos_dharma))
  }

  message("Modelo GLM ajustado correctamente. Resumen:")
  message("==========================================")
  print(summary(modelo))
  message("==========================================")

  return(modelo)
}
