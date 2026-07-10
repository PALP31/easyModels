#' Ajustar y analizar un Modelo Lineal Mixto Generalizado (GLMM)
#'
#' Ajusta modelos GLMM con una amplia gama de distribuciones para datos biologicos
#' con estructura jerarquica o de medidas repetidas. Usa \code{lme4::glmer} para
#' familias basicas y \code{glmmTMB} para distribuciones avanzadas como Gamma,
#' Beta, Tweedie, zero-infladas y binomial negativa parametrizada.
#' Devuelve un objeto unificado S3 de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula_fijos Una formula de R o cadena para la parte de efectos fijos
#'   (ej. \code{y ~ x1 + x2}).
#' @param aleatorios Una cadena que define la estructura de efectos aleatorios
#'   (ej. \code{"(1 | bloque)"} o \code{"(1 | bloque/parcela)"}).
#' @param tipo Familia de distribucion. Opciones:
#'   \itemize{
#'     \item \code{"poisson"} / \code{"conteos"} — Conteos con efectos mixtos (lme4).
#'     \item \code{"binomial"} / \code{"presencia_ausencia"} — Binaria 0/1 (lme4).
#'     \item \code{"negativa_binomial"} / \code{"binomial_negativa"} — Conteos sobredispersos (lme4::glmer.nb).
#'     \item \code{"gamma"} — Respuesta continua positiva asimetrica (concentraciones,
#'       tiempos de respuesta, rendimiento continuo, masa foliar). Usa glmmTMB con link log.
#'     \item \code{"gamma_inverse"} — Gamma con link inverso. Usa glmmTMB.
#'     \item \code{"lognormal"} — Respuesta log-normal: datos positivos con distribucion
#'       altamente asimetrica donde log(y) es normal (tamanos coloniales, pesos).
#'       Usa glmmTMB.
#'     \item \code{"beta"} — Proporciones continuas en (0,1) (coberturas vegetales,
#'       indices de diversidad, tasas de germination). Usa glmmTMB.
#'     \item \code{"tweedie"} — Mezcla de ceros y valores positivos continuos
#'       (precipitacion, biomasa con ausencias, produccion con ceros reales).
#'       Usa glmmTMB.
#'     \item \code{"nbinom2"} — Binomial negativa parametrizacion NB2 (variance = mu + mu^2/phi),
#'       mas flexible que glmer.nb. Usa glmmTMB.
#'     \item \code{"nbinom1"} — Binomial negativa NB1 (variance lineal en mu). Usa glmmTMB.
#'     \item \code{"zip"} — Zero-Inflated Poisson (conteos con exceso de ceros). Usa glmmTMB.
#'     \item \code{"zinb"} — Zero-Inflated Binomial Negativa NB2. Usa glmmTMB.
#'     \item \code{"zifgamma"} — Zero-Inflated Gamma (respuesta continua positiva con ceros
#'       reales, ej. consumo de agua, excrecion). Usa glmmTMB.
#'     \item \code{"ordinal"} — Respuesta ordinal con efectos mixtos
#'       (escala de dano 1-5, grados de calidad). Requiere \code{ordinal::clmm}.
#'     \item \code{"binomial_cloglog"} — Binomial con link complementario log-log.
#'       Util para datos de incidencia o supervivencia agrupados. Usa lme4.
#'     \item \code{"binomial_probit"} — Binomial con link probit. Usa lme4.
#'   }
#' @param diagnosticos Valor logico. Si es \code{TRUE}, genera diagnosticos de
#'   residuos simulados con \code{DHARMa}.
#' @param zi_formula Formula para la parte zero-inflada en ZIP/ZINB/ZIGamma.
#'   Por defecto \code{~ 1} (intercepto).
#' @param link_gamma Link para familia Gamma en glmmTMB. \code{"log"} (predeterminado),
#'   \code{"inverse"}, o \code{"identity"}.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#'
#' @importFrom lme4 glmer glmer.nb
#' @importFrom stats as.formula residuals df.residual binomial poisson
#' @importFrom DHARMa simulateResiduals testDispersion testZeroInflation
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_abort cli_h2
#'
#' @examples
#' \dontrun{
#'   # Poisson GLMM — conteos de colonias por bloque
#'   datos <- data.frame(
#'     colonias = rpois(60, lambda = 8),
#'     genotipo = factor(rep(1:3, each = 20)),
#'     bloque   = factor(rep(1:5, times = 12))
#'   )
#'   m1 <- analizar_glmm(datos, colonias ~ genotipo, "(1 | bloque)", tipo = "poisson")
#'
#'   # Gamma GLMM — concentracion de clorofila
#'   datos_clorofila <- data.frame(
#'     clorofila = rgamma(80, shape = 3, scale = 2),
#'     tratamiento = factor(rep(c("Control","T1","T2","T3"), each = 20)),
#'     planta = factor(rep(1:20, times = 4))
#'   )
#'   m2 <- analizar_glmm(datos_clorofila, clorofila ~ tratamiento,
#'                        "(1 | planta)", tipo = "gamma")
#'
#'   # Beta GLMM — cobertura vegetal (0-1)
#'   datos_cob <- data.frame(
#'     cobertura = rbeta(80, 2, 3),
#'     manejo = factor(rep(c("Convencional","Organico"), each = 40)),
#'     parcela = factor(rep(1:10, times = 8))
#'   )
#'   m3 <- analizar_glmm(datos_cob, cobertura ~ manejo, "(1 | parcela)", tipo = "beta")
#' }
analizar_glmm <- function(datos,
                           formula_fijos,
                           aleatorios,
                           tipo = c(
                             "poisson", "conteos",
                             "binomial", "presencia_ausencia",
                             "negativa_binomial", "binomial_negativa",
                             "gamma",
                             "gamma_inverse",
                             "lognormal",
                             "beta",
                             "tweedie",
                             "nbinom2",
                             "nbinom1",
                             "zip",
                             "zinb",
                             "zifgamma",
                             "ordinal",
                             "binomial_cloglog",
                             "binomial_probit"
                           ),
                           diagnosticos = TRUE,
                           zi_formula   = ~ 1,
                           link_gamma   = "log") {
  cli::cli_alert_info("Iniciando Ajuste de GLMM...")

  tipo <- match.arg(tipo)
  # Aliases
  if (tipo == "conteos")          tipo <- "poisson"
  if (tipo == "presencia_ausencia") tipo <- "binomial"
  if (tipo == "binomial_negativa") tipo <- "negativa_binomial"

  formula_completa <- construir_formula_mixta(formula_fijos, aleatorios)
  cli::cli_alert_info("Formula construida: {.code {Reduce(paste, deparse(formula_completa))}}")

  # ── Mensajes guía bioestadística ─────────────────────────────────────────────
  msgs <- list(
    poisson          = "Conteos con efectos mixtos. Familia: Poisson, Link: log. Verificar sobredispersion.",
    binomial         = "Respuesta binaria 0/1 con efectos mixtos. Familia: Binomial, Link: logit.",
    negativa_binomial= "Conteos sobredispersos con efectos mixtos. Familia: Binomial Negativa (lme4::glmer.nb).",
    gamma            = paste0("Respuesta continua positiva con efectos mixtos. Familia: Gamma, Link: ", link_gamma, "."),
    gamma_inverse    = "Respuesta continua positiva. Familia: Gamma, Link: inverse. Usa glmmTMB.",
    lognormal        = "Respuesta log-normal: datos positivos muy sesgados donde log(y) es normal. Usa glmmTMB.",
    beta             = "Proporciones continuas (0,1): coberturas, tasas. Familia: Beta. Usa glmmTMB.",
    tweedie          = "Mezcla de ceros y valores positivos continuos. Familia: Tweedie. Usa glmmTMB.",
    nbinom2          = "Conteos sobredispersos (NB2: var = mu + mu^2/phi). Usa glmmTMB.",
    nbinom1          = "Conteos sobredispersos (NB1: var lineal en mu). Usa glmmTMB.",
    zip              = "Conteos con exceso de ceros (Zero-Inflated Poisson). Usa glmmTMB.",
    zinb             = "Conteos con exceso de ceros y sobredispersion (Zero-Inflated NB2). Usa glmmTMB.",
    zifgamma         = "Respuesta continua positiva con ceros reales (Zero-Inflated Gamma). Usa glmmTMB.",
    ordinal          = "Respuesta ordinal categorica con efectos mixtos. Usa ordinal::clmm.",
    binomial_cloglog = "Respuesta binaria. Link: cloglog (incidencia acumulada). Usa lme4.",
    binomial_probit  = "Respuesta binaria. Link: probit (normalidad latente). Usa lme4."
  )
  cli::cli_h2("Familia seleccionada: {tipo}")
  cli::cli_alert_info(msgs[[tipo]])

  # ── Familias que requieren glmmTMB ───────────────────────────────────────────
  tipos_glmmTMB <- c("gamma", "gamma_inverse", "lognormal", "beta", "tweedie",
                     "nbinom2", "nbinom1", "zip", "zinb", "zifgamma")

  if (tipo %in% tipos_glmmTMB && !requireNamespace("glmmTMB", quietly = TRUE)) {
    cli::cli_abort(
      "El paquete 'glmmTMB' es necesario para la familia '{tipo}'. Instalalo con install.packages('glmmTMB')."
    )
  }

  # ── Ajuste del modelo ────────────────────────────────────────────────────────
  cli::cli_alert_info("Ajustando modelo GLMM...")

  modelo_nat <- tryCatch({
    switch(tipo,

      # ── lme4: familias base ──────────────────────────────────────────────────
      "poisson" = lme4::glmer(
        formula_completa, data = datos, family = stats::poisson(link = "log")
      ),
      "binomial" = lme4::glmer(
        formula_completa, data = datos, family = stats::binomial(link = "logit")
      ),
      "binomial_probit" = lme4::glmer(
        formula_completa, data = datos, family = stats::binomial(link = "probit")
      ),
      "binomial_cloglog" = lme4::glmer(
        formula_completa, data = datos, family = stats::binomial(link = "cloglog")
      ),
      "negativa_binomial" = lme4::glmer.nb(formula_completa, data = datos),

      # ── glmmTMB: Gamma y Lognormal ─────────────────────────────────────────────
      "gamma" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = stats::Gamma(link = link_gamma)
      ),
      "gamma_inverse" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = stats::Gamma(link = "inverse")
      ),
      "lognormal" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::lognormal(link = "log")
      ),

      # ── glmmTMB: Beta ─────────────────────────────────────────────────────────
      "beta" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::beta_family(link = "logit")
      ),

      # ── glmmTMB: Tweedie ──────────────────────────────────────────────────────
      "tweedie" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::tweedie(link = "log")
      ),

      # ── glmmTMB: NB1, NB2 ────────────────────────────────────────────────────
      "nbinom2" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::nbinom2(link = "log")
      ),
      "nbinom1" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::nbinom1(link = "log")
      ),

      # ── glmmTMB: Zero-inflated ────────────────────────────────────────────────
      "zip" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = stats::poisson(link = "log"),
        ziformula = zi_formula
      ),
      "zinb" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::nbinom2(link = "log"),
        ziformula = zi_formula
      ),
      "zifgamma" = glmmTMB::glmmTMB(
        formula_completa, data = datos,
        family = glmmTMB::ziGamma(link = "log"),
        ziformula = zi_formula
      ),

      # ── ordinal::clmm ─────────────────────────────────────────────────────────
      "ordinal" = {
        if (!requireNamespace("ordinal", quietly = TRUE)) {
          cli::cli_abort("El paquete 'ordinal' es necesario para modelos ordinales mixtos. Instalalo con install.packages('ordinal').")
        }
        ordinal::clmm(formula_completa, data = datos)
      }
    )
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo GLMM ({tipo}): {e$message}")
  })

  em <- crear_easy_model(modelo_nat, tipo_modelo = "GLMM", datos = datos)

  # ── Post-ajuste: sobredispersión ──────────────────────────────────────────────
  tipos_sobredisp <- c("poisson", "negativa_binomial", "nbinom1", "nbinom2")
  if (tipo %in% tipos_sobredisp) {
    cli::cli_alert_info("Calculando ratio de sobredispersion (Pearson Chi2/GL)...")
    dispersion <- tryCatch(calcular_sobredispersion(modelo_nat), error = function(e) NULL)
    if (!is.null(dispersion) && !is.na(dispersion$ratio)) {
      cli::cli_alert_info(
        "Ratio de sobredispersion: {.val {round(dispersion$ratio, 3)}} (Chi2={round(dispersion$chi2, 2)}, GL={dispersion$gl})"
      )
      if (dispersion$ratio > 1.5 && tipo == "poisson") {
        cli::cli_alert_warning(
          "Sobredispersion detectada (ratio > 1.5). Considere tipo = 'negativa_binomial', 'nbinom2', 'zip' o 'zinb'."
        )
      } else {
        cli::cli_alert_success("Dispersion dentro del rango aceptable (<= 1.5).")
      }
    }
  }

  # ── Post-ajuste: odds ratios para binomiales ──────────────────────────────────
  tipos_bin <- c("binomial", "binomial_probit", "binomial_cloglog")
  if (tipo %in% tipos_bin) {
    cli::cli_alert_info("Odds ratios de efectos fijos:")
    print(analizar_odds_ratio(modelo_nat))
  }

  # ── Guias adicionales ─────────────────────────────────────────────────────────
  if (tipo == "beta") {
    cli::cli_alert_info("Beta GLMM: verificar que todos los valores esten en el intervalo abierto (0, 1).")
    cli::cli_alert_info("Si hay valores exactamente 0 o 1, aplica transformacion: (y*(n-1) + 0.5) / n.")
  }
  if (tipo %in% c("zip", "zinb", "zifgamma")) {
    cli::cli_alert_info("Modelo zero-inflado: verifica la proporcion de ceros con DHARMa testZeroInflation().")
  }
  if (tipo %in% c("gamma", "gamma_inverse")) {
    cli::cli_alert_info("Gamma GLMM: verificar que todos los valores de la respuesta sean estrictamente > 0.")
  }
  if (tipo == "lognormal") {
    cli::cli_alert_info("Lognormal GLMM: verificar que todos los valores de la respuesta sean estrictamente > 0.")
    cli::cli_alert_info("Los coeficientes estan en escala log; exponenciarlos para interpretar en escala original.")
  }

  # ── Diagnósticos DHARMa ────────────────────────────────────────────────────────
  if (isTRUE(diagnosticos)) {
    cli::cli_alert_info("Generando diagnosticos DHARMa de residuos simulados...")
    residuos_dharma <- tryCatch(
      DHARMa::simulateResiduals(modelo_nat, plot = FALSE),
      error = function(e) {
        cli::cli_alert_warning("DHARMa no pudo simular residuos: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(residuos_dharma)) {
      graphics::plot(residuos_dharma)
      print(DHARMa::testDispersion(residuos_dharma, plot = FALSE))
      if (tipo %in% c("poisson", "zip", "zinb", "zifgamma", "negativa_binomial", "nbinom1", "nbinom2")) {
        print(DHARMa::testZeroInflation(residuos_dharma, plot = FALSE))
      }
      if (tipo == "ordinal") {
        cli::cli_alert_info("Modelo ordinal: DHARMa no soporta residuos simulados para ordinal::clmm. Usa performance::check_model() para diagnosticos.")
      }
    }
  }

  print(em)
  return(em)
}
