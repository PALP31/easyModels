#' Ajustar y analizar un Modelo Lineal Mixto Generalizado (GLMM)
#'
#' Esta funcion ajusta un modelo lineal mixto generalizado (GLMM) utilizando \code{lme4::glmer}
#' con la familia correspondiente segun el tipo de respuesta (Poisson,
#' Binomial o Binomial Negativa).
#' En el caso de conteos, calcula e informa el ratio de sobredispersion de Pearson.
#' Devuelve un objeto unificado de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una formula de R o una cadena de caracteres para la parte de efectos fijos (ej. \code{y ~ x1}).
#' @param aleatorios Una cadena de caracteres que define la estructura de efectos aleatorios (ej. \code{"(1 | bloque)"}).
#' @param tipo Tipo de respuesta. Puede ser \code{"conteos"},
#'   \code{"presencia_ausencia"}, \code{"poisson"}, \code{"binomial"},
#'   \code{"negativa_binomial"} o \code{"binomial_negativa"}.
#' @param diagnostico_dharma Valor logico. Si es \code{TRUE}, genera diagnosticos de residuos simulados con \code{DHARMa}.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#'
#' @importFrom lme4 glmer fixef glmer.nb
#' @importFrom stats as.formula residuals df.residual binomial poisson
#' @importFrom DHARMa simulateResiduals testDispersion testZeroInflation
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_abort
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     exito = rbinom(100, 1, 0.5),
#'     conteos = rpois(100, lambda = 3),
#'     x = rnorm(100),
#'     bloque = factor(rep(1:10, each = 10))
#'   )
#'   # Para presencia/ausencia (Binomial)
#'   modelo <- analizar_glmm(datos, exito ~ x, "(1 | bloque)", tipo = "presencia_ausencia")
#'   summary(modelo)
#' }
analizar_glmm <- function(datos,
                          formula_fijos,
                          aleatorios,
                          tipo = c("conteos", "presencia_ausencia", "poisson", "binomial", "negativa_binomial", "binomial_negativa"),
                          diagnostico_dharma = TRUE) {
  cli::cli_alert_info("Iniciando Ajuste de GLMM...")
  
  tipo <- match.arg(tipo)
  if (tipo == "conteos") tipo <- "poisson"
  if (tipo == "presencia_ausencia") tipo <- "binomial"
  if (tipo == "binomial_negativa") tipo <- "negativa_binomial"

  formula_completa <- construir_formula_mixta(formula_fijos, aleatorios)
  
  cli::cli_alert_info("Fórmula construida: {.code {Reduce(paste, deparse(formula_completa))}}")
  
  # Seleccion de familia segun el tipo de datos
  if (tipo == "poisson") {
    fam <- stats::poisson(link = "log")
    cli::cli_alert_info("Tipo de análisis: Conteos (Familia: Poisson, Link: log)")
  } else if (tipo == "binomial") {
    fam <- stats::binomial(link = "logit")
    cli::cli_alert_info("Tipo de análisis: Presencia/Ausencia (Familia: Binomial, Link: logit)")
  } else {
    fam <- NULL
    cli::cli_alert_info("Tipo de análisis: Conteos sobredispersos (Familia: Binomial Negativa)")
  }
  
  cli::cli_alert_info("Ajustando modelo GLMM...")
  modelo_nat <- tryCatch({
    if (tipo == "negativa_binomial") {
      lme4::glmer.nb(formula_completa, data = datos)
    } else {
      lme4::glmer(formula_completa, data = datos, family = fam)
    }
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo GLMM: {e$message}")
  })
  
  em <- crear_easy_model(modelo_nat, tipo_modelo = "GLMM", datos = datos)
  
  # Evaluacion de sobredispersion para conteos
  if (tipo %in% c("poisson", "negativa_binomial")) {
    cli::cli_alert_info("Calculando ratio de sobredispersión...")
    dispersion <- calcular_sobredispersion(modelo_nat)
    ratio <- dispersion$ratio

    cli::cli_alert_info("Ratio de sobredispersión (Chi2/GL): {.val {round(ratio, 3)}}")
    cli::cli_alert_info("  - Chi2: {.val {round(dispersion$chi2, 2)}}")
    cli::cli_alert_info("  - Grados de Libertad (GL): {.val {dispersion$gl}}")
    
    if (ratio > 1.5 && tipo == "poisson") {
      cli::cli_alert_warning("El ratio de sobredispersión es mayor a 1.5 ({.val {round(ratio, 2)}}).")
      cli::cli_alert_warning("Esto sugiere sobredispersión en los datos de conteo. Considere:")
      cli::cli_alert_warning("1. Incluir un efecto aleatorio a nivel de observacion (OLRE).")
      cli::cli_alert_warning("2. Reajustar con tipo = 'binomial_negativa'.")
    } else {
      cli::cli_alert_success("El ratio de sobredispersión está dentro del rango aceptable (<= 1.5).")
    }
  }

  if (tipo == "binomial") {
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
