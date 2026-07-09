#' Ajustar y analizar un Modelo Lineal Mixto Generalizado (GLMM)
#'
#' Esta funcion ajusta un modelo lineal mixto generalizado (GLMM) utilizando \code{lme4::glmer}
#' con la familia correspondiente segun el tipo de respuesta (Poisson,
#' Binomial o Binomial Negativa).
#' En el caso de conteos, calcula e informa el ratio de sobredispersion de Pearson. Imprime el resumen
#' (\code{summary}) del modelo en la consola y devuelve el objeto del modelo ajustado.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una formula de R o una cadena de caracteres para la parte de efectos fijos (ej. \code{y ~ x1}).
#' @param aleatorios Una cadena de caracteres que define la estructura de efectos aleatorios (ej. \code{"(1 | bloque)"}).
#' @param tipo Tipo de respuesta. Puede ser \code{"conteos"},
#'   \code{"presencia_ausencia"}, \code{"poisson"}, \code{"binomial"} o
#'   \code{"negativa_binomial"}.
#' @param diagnostico_dharma Valor logico. Si es \code{TRUE}, genera diagnosticos de residuos simulados con \code{DHARMa}.
#'
#' @return Un objeto de la clase \code{merMod} (ajuste de glmer) con el modelo ajustado.
#' @export
#'
#' @importFrom lme4 glmer fixef
#' @importFrom stats as.formula residuals df.residual binomial poisson
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
#'   analizar_glmm(datos, exito ~ x, "(1 | bloque)", tipo = "presencia_ausencia")
#'   # Para conteos (Poisson)
#'   analizar_glmm(datos, conteos ~ x, "(1 | bloque)", tipo = "conteos")
#' }
analizar_glmm <- function(datos,
                          formula_fijos,
                          aleatorios,
                          tipo = c("conteos", "presencia_ausencia", "poisson", "binomial", "negativa_binomial"),
                          diagnostico_dharma = TRUE) {
  message("=== Iniciando Ajuste de GLMM ===")
  
  tipo <- match.arg(tipo)
  if (tipo == "conteos") tipo <- "poisson"
  if (tipo == "presencia_ausencia") tipo <- "binomial"

  formula_completa <- construir_formula_mixta(formula_fijos, aleatorios)
  
  message("Formula construida: ", Reduce(paste, deparse(formula_completa)))
  
  # Seleccion de familia segun el tipo de datos
  if (tipo == "poisson") {
    fam <- stats::poisson(link = "log")
    message("Tipo de analisis: Conteos (Familia: Poisson, Link: log)")
  } else if (tipo == "binomial") {
    fam <- stats::binomial(link = "logit")
    message("Tipo de analisis: Presencia/Ausencia (Familia: Binomial, Link: logit)")
  } else {
    fam <- NULL
    message("Tipo de analisis: Conteos sobredispersos (Familia: Binomial Negativa)")
  }
  
  message("Ajustando modelo GLMM...")
  modelo <- if (tipo == "negativa_binomial") {
    lme4::glmer.nb(formula_completa, data = datos)
  } else {
    lme4::glmer(formula_completa, data = datos, family = fam)
  }
  
  # Evaluacion de sobredispersion para conteos
  if (tipo %in% c("poisson", "negativa_binomial")) {
    message("Calculando ratio de sobredispersion...")
    dispersion <- calcular_sobredispersion(modelo)
    ratio <- dispersion$ratio

    message(sprintf("Ratio de sobredispersion (Chi2/GL): %.3f", ratio))
    message(sprintf("  - Chi2: %.2f", dispersion$chi2))
    message(sprintf("  - Grados de Libertad (GL): %d", dispersion$gl))
    
    if (ratio > 1.5 && tipo == "poisson") {
      message("-----------------------------------------------------------------")
      message("ADVERTENCIA: El ratio de sobredispersion es mayor a 1.5 (", round(ratio, 2), ").")
      message("Esto sugiere sobredispersion en los datos de conteo. Considere:")
      message("1. Incluir un efecto aleatorio a nivel de observacion (OLRE).")
      message("2. Reajustar con tipo = 'negativa_binomial'.")
      message("-----------------------------------------------------------------")
    } else {
      message("El ratio de sobredispersion esta dentro del rango aceptable (<= 1.5).")
    }
  }

  if (tipo == "binomial") {
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
  
  message("Modelo GLMM ajustado correctamente. Resumen del modelo:")
  message("=====================================================")
  print(summary(modelo))
  message("=====================================================")
  
  return(modelo)
}
