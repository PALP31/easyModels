#' Ajustar y analizar un modelo lineal generalizado (GLM)
#'
#' Ajusta modelos GLM gaussianos, binomiales, Poisson, cuasi-Poisson,
#' cuasi-binomiales o binomiales negativas. Para modelos de conteo calcula
#' sobredispersión; para binomiales imprime odds ratios; y puede ejecutar
#' diagnósticos unificados. Retorna un objeto unificado de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula Una fórmula de R para el GLM.
#' @param familia Familia del modelo. Puede ser \code{"gaussian"},
#'   \code{"binomial"}, \code{"poisson"}, \code{"quasibinomial"},
#'   \code{"quasipoisson"} o \code{"negativa_binomial"}.
#' @param diagnostico_dharma Valor lógico. Si es \code{TRUE}, genera
#'   diagnósticos de residuos simulados con \code{DHARMa}.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#' @importFrom stats glm gaussian binomial poisson quasibinomial quasipoisson
#' @importFrom MASS glm.nb
#' @importFrom DHARMa simulateResiduals testDispersion testZeroInflation
#' @importFrom cli cli_alert_info cli_alert_warning cli_abort
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_glm(iris, Species == "setosa" ~ Sepal.Length, familia = "binomial")
#'   summary(modelo)
#' }
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
  cli::cli_alert_info("Iniciando ajuste de GLM...")
  familia <- match.arg(familia)

  modelo_nat <- tryCatch({
    if (familia == "negativa_binomial") {
      cli::cli_alert_info("Familia seleccionada: Binomial negativa (MASS::glm.nb)")
      MASS::glm.nb(formula = formula, data = datos)
    } else {
      fam <- switch(
        familia,
        gaussian = stats::gaussian(),
        binomial = stats::binomial(),
        poisson = stats::poisson(),
        quasibinomial = stats::quasibinomial(),
        quasipoisson = stats::quasipoisson()
      )
      cli::cli_alert_info("Familia seleccionada: {familia}")
      stats::glm(formula = formula, data = datos, family = fam)
    }
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo GLM: {e$message}")
  })

  em <- crear_easy_model(modelo_nat, tipo_modelo = "GLM", datos = datos)

  if (familia %in% c("poisson", "quasipoisson", "negativa_binomial")) {
    dispersion <- calcular_sobredispersion(modelo_nat)
    cli::cli_alert_info("Ratio de sobredispersión (Pearson Chi2/GL): {.val {round(dispersion$ratio, 3)}}")
    if (dispersion$ratio > 1.5 && familia == "poisson") {
      cli::cli_alert_warning("Sugerencia: hay sobredispersión. Considere familia = 'quasipoisson' o 'negativa_binomial'.")
    }
  }

  if (familia %in% c("binomial", "quasibinomial")) {
    cli::cli_alert_info("Odds ratios de efectos fijos:")
    print(analizar_odds_ratio(modelo_nat))
  }

  if (isTRUE(diagnostico_dharma)) {
    cli::cli_alert_info("Generando diagnósticos DHARMa de residuos simulados...")
    residuos_dharma <- DHARMa::simulateResiduals(modelo_nat)
    plot(residuos_dharma)
    print(DHARMa::testDispersion(residuos_dharma))
    print(DHARMa::testZeroInflation(residuos_dharma))
  }

  print(em)
  return(em)
}
