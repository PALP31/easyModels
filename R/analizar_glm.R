#' Ajustar y analizar un Modelo Lineal Generalizado (GLM)
#'
#' Ajusta modelos GLM con una amplia gama de familias de distribucion adecuadas
#' para datos biológicos y agronomicos: gaussiana, binomial (logit, probit, cloglog),
#' Poisson, cuasi-Poisson, cuasi-binomial, binomial negativa (MASS), Gamma,
#' gaussiana inversa, beta (betareg), Tweedie, zero-inflado Poisson/NB (pscl),
#' y ordinal/multinomial (MASS::polr/multinom). Para modelos de conteo calcula
#' sobredispersion de Pearson; para binomiales imprime odds ratios; puede ejecutar
#' diagnosticos DHARMa. Devuelve un objeto unificado S3 de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} con las variables del modelo.
#' @param formula Una formula de R para el GLM.
#' @param familia Familia del modelo. Opciones:
#'   \itemize{
#'     \item \code{"gaussian"} — Respuesta continua simetrica, errores normales.
#'     \item \code{"binomial"} — Respuesta binaria 0/1 o proporcion. Link: logit.
#'     \item \code{"binomial_probit"} — Igual que binomial con link probit.
#'     \item \code{"binomial_cloglog"} — Binomial con link complementario log-log
#'       (util para datos de supervivencia o incidencia acumulada).
#'     \item \code{"poisson"} — Conteos (asume varianza = media).
#'     \item \code{"quasipoisson"} — Conteos con sobredispersion (factor de dispersion estimado).
#'     \item \code{"quasibinomial"} — Proporcion con sobredispersion.
#'     \item \code{"negativa_binomial"} — Conteos con sobredispersion severa (via MASS::glm.nb).
#'     \item \code{"gamma"} — Respuesta continua positiva y asimetrica (tiempos, concentraciones).
#'       Link: log (predeterminado) o inverse.
#'     \item \code{"gamma_inverse"} — Gamma con link inverso (clasico de actuaria y fisiologia).
#'     \item \code{"gaussian_inversa"} — Gaussian inversa: datos continuos positivos muy sesgados
#'       (tiempos de latencia, pesos extremos). Link: 1/mu^2.
#'     \item \code{"tweedie"} — Familia Tweedie compuesta (mix de ceros y valores positivos
#'       continuos, p.ej. precipitacion, biomasa con ausencias). Requiere paquete \code{statmod}.
#'       Usar junto con \code{tweedie_p} (tipicamente entre 1 y 2).
#'     \item \code{"beta"} — Datos en intervalo abierto (0,1): porcentajes, coberturas,
#'       proporciones continuas. Requiere paquete \code{betareg}.
#'     \item \code{"zip"} — Zero-Inflated Poisson (conteos con exceso de ceros).
#'       Requiere paquete \code{pscl}.
#'     \item \code{"zinb"} — Zero-Inflated Binomial Negativa. Requiere \code{pscl}.
#'     \item \code{"ordinal"} — Respuesta ordinal categorica (escalas Likert, grados de dano).
#'       Usa \code{MASS::polr} con link logit proporcional de probabilidades.
#'     \item \code{"multinomial"} — Respuesta categorica nominal (>2 categorias sin orden).
#'       Requiere paquete \code{nnet}.
#'   }
#' @param diagnosticos Valor logico. Si es \code{TRUE}, genera diagnosticos de residuos
#'   simulados con \code{DHARMa} (donde sea aplicable) o graficos diagnosticos base.
#' @param link_gamma Link alternativo para familia Gamma. \code{"log"} (predeterminado) o
#'   \code{"inverse"} o \code{"identity"}.
#' @param tweedie_p Parametro de potencia para Tweedie (\code{1 < p < 2}).
#'   Valor predeterminado: \code{1.5}.
#' @param zi_formula Formula para la parte zero-inflada en ZIP/ZINB.
#'   Por defecto \code{~ 1} (intercepto).
#'
#' @return Un objeto unificado S3 de clase \code{easy_model}.
#' @export
#'
#' @importFrom stats glm gaussian binomial poisson quasibinomial quasipoisson inverse.gaussian Gamma
#' @importFrom MASS glm.nb polr
#' @importFrom DHARMa simulateResiduals testDispersion testZeroInflation
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_abort cli_h2
#'
#' @examples
#' \dontrun{
#'   # Binomial (logit) — presencia/ausencia
#'   m1 <- analizar_glm(iris, I(Species == "setosa") ~ Sepal.Length, familia = "binomial")
#'
#'   # Gamma — tiempo de floracion o concentracion mineral
#'   datos_gamma <- data.frame(tiempo = rgamma(80, shape = 2, scale = 3),
#'                             tratamiento = factor(rep(c("A","B"), each = 40)))
#'   m2 <- analizar_glm(datos_gamma, tiempo ~ tratamiento, familia = "gamma")
#'
#'   # Binomial negativa — conteos con sobredispersion
#'   datos_nb <- data.frame(colonias = rnbinom(60, mu = 10, size = 2),
#'                          genotipo = factor(rep(1:3, each = 20)))
#'   m3 <- analizar_glm(datos_nb, colonias ~ genotipo, familia = "negativa_binomial")
#' }
analizar_glm <- function(datos,
                          formula,
                          familia = c(
                            "gaussian",
                            "binomial",
                            "binomial_probit",
                            "binomial_cloglog",
                            "poisson",
                            "quasipoisson",
                            "quasibinomial",
                            "negativa_binomial",
                            "gamma",
                            "gamma_inverse",
                            "gaussian_inversa",
                            "tweedie",
                            "beta",
                            "zip",
                            "zinb",
                            "ordinal",
                            "multinomial"
                          ),
                          diagnosticos = TRUE,
                          link_gamma  = "log",
                          tweedie_p   = 1.5,
                          zi_formula  = ~ 1) {
  cli::cli_alert_info("Iniciando ajuste de GLM...")
  familia <- match.arg(familia)

  # ── Mensajes bioestadísticos de guía ────────────────────────────────────────
  msgs <- list(
    gaussian         = "Respuesta continua simetrica (errores normales). Link: identity.",
    binomial         = "Respuesta binaria 0/1 o proporcion. Link: logit.",
    binomial_probit  = "Respuesta binaria. Link: probit (asume normalidad latente).",
    binomial_cloglog = "Respuesta binaria. Link: cloglog (util en supervivencia o incidencia acumulada).",
    poisson          = "Conteos (varianza = media). Link: log. Verificar sobredispersion.",
    quasipoisson     = "Conteos con sobredispersion estimada. Link: log.",
    quasibinomial    = "Proporcion con sobredispersion estimada. Link: logit.",
    negativa_binomial= "Conteos con sobredispersion severa (parametro de dispersion theta). Link: log.",
    gamma            = paste0("Respuesta continua positiva y asimetrica (concentraciones, tiempos). Link: ", link_gamma, "."),
    gamma_inverse    = "Respuesta continua positiva. Link: inverse (gamma clasico de actuaria).",
    gaussian_inversa = "Respuesta continua positiva muy sesgada (tiempos de latencia). Link: 1/mu^2.",
    tweedie          = paste0("Datos con mezcla de ceros y valores positivos (p=", tweedie_p, "). Link: log."),
    beta             = "Proporciones continuas en (0,1): coberturas, porcentajes. Link: logit.",
    zip              = "Conteos con exceso de ceros (Zero-Inflated Poisson). Requiere pscl.",
    zinb             = "Conteos con exceso de ceros y sobredispersion (Zero-Inflated NB). Requiere pscl.",
    ordinal          = "Respuesta ordinal categorica (Proportional Odds Model). Requiere MASS::polr.",
    multinomial      = "Respuesta nominal con >2 categorias (Multinomial Logit). Requiere nnet."
  )
  cli::cli_h2("Familia seleccionada: {familia}")
  cli::cli_alert_info(msgs[[familia]])

  # ── Ajuste del modelo ────────────────────────────────────────────────────────
  modelo_nat <- tryCatch({
    switch(familia,

      # ── Familias estándar de stats ──────────────────────────────────────────
      "gaussian" = stats::glm(formula, data = datos, family = stats::gaussian()),

      "binomial" = stats::glm(formula, data = datos, family = stats::binomial(link = "logit")),

      "binomial_probit" = stats::glm(formula, data = datos, family = stats::binomial(link = "probit")),

      "binomial_cloglog" = stats::glm(formula, data = datos, family = stats::binomial(link = "cloglog")),

      "poisson" = stats::glm(formula, data = datos, family = stats::poisson(link = "log")),

      "quasipoisson" = stats::glm(formula, data = datos, family = stats::quasipoisson(link = "log")),

      "quasibinomial" = stats::glm(formula, data = datos, family = stats::quasibinomial(link = "logit")),

      # ── Binomial negativa (MASS) ────────────────────────────────────────────
      "negativa_binomial" = MASS::glm.nb(formula = formula, data = datos),

      # ── Gamma ───────────────────────────────────────────────────────────────
      "gamma" = stats::glm(formula, data = datos,
                           family = stats::Gamma(link = link_gamma)),

      "gamma_inverse" = stats::glm(formula, data = datos,
                                   family = stats::Gamma(link = "inverse")),

      # ── Gaussiana inversa ────────────────────────────────────────────────────
      "gaussian_inversa" = stats::glm(formula, data = datos,
                                      family = stats::inverse.gaussian(link = "1/mu^2")),

      # ── Tweedie (statmod) ────────────────────────────────────────────────────
      "tweedie" = {
        if (!requireNamespace("statmod", quietly = TRUE)) {
          cli::cli_abort("El paquete 'statmod' es necesario para Tweedie. Instalalo con install.packages('statmod').")
        }
        stats::glm(formula, data = datos,
                   family = statmod::tweedie(var.power = tweedie_p, link.power = 0))
      },

      # ── Beta regression (betareg) ─────────────────────────────────────────────
      "beta" = {
        if (!requireNamespace("betareg", quietly = TRUE)) {
          cli::cli_abort("El paquete 'betareg' es necesario. Instalalo con install.packages('betareg').")
        }
        betareg::betareg(formula, data = datos)
      },

      # ── Zero-inflated (pscl) ──────────────────────────────────────────────────
      "zip" = {
        if (!requireNamespace("pscl", quietly = TRUE)) {
          cli::cli_abort("El paquete 'pscl' es necesario para ZIP. Instalalo con install.packages('pscl').")
        }
        pscl::zeroinfl(formula, data = datos, dist = "poisson", zero.dist = "binomial",
                       formula.zero = zi_formula)
      },

      "zinb" = {
        if (!requireNamespace("pscl", quietly = TRUE)) {
          cli::cli_abort("El paquete 'pscl' es necesario para ZINB. Instalalo con install.packages('pscl').")
        }
        pscl::zeroinfl(formula, data = datos, dist = "negbin", zero.dist = "binomial",
                       formula.zero = zi_formula)
      },

      # ── Ordinal (MASS::polr) ──────────────────────────────────────────────────
      "ordinal" = {
        MASS::polr(formula, data = datos, Hess = TRUE)
      },

      # ── Multinomial (nnet) ────────────────────────────────────────────────────
      "multinomial" = {
        if (!requireNamespace("nnet", quietly = TRUE)) {
          cli::cli_abort("El paquete 'nnet' es necesario para multinomial. Instalalo con install.packages('nnet').")
        }
        nnet::multinom(formula, data = datos, trace = FALSE)
      }
    )
  }, error = function(e) {
    cli::cli_abort("Error al ajustar el modelo GLM ({familia}): {e$message}")
  })

  em <- crear_easy_model(modelo_nat, tipo_modelo = "GLM", datos = datos)

  # ── Post-ajuste: mensajes y diagnósticos según familia ─────────────────────

  # Sobredispersión
  familias_sobredisp <- c("poisson", "quasipoisson", "negativa_binomial")
  if (familia %in% familias_sobredisp && inherits(modelo_nat, c("glm", "negbin"))) {
    dispersion <- tryCatch(calcular_sobredispersion(modelo_nat), error = function(e) NULL)
    if (!is.null(dispersion)) {
      cli::cli_alert_info("Ratio de sobredispersion (Pearson Chi2/GL): {.val {round(dispersion$ratio, 3)}}")
      if (dispersion$ratio > 1.5 && familia == "poisson") {
        cli::cli_alert_warning(
          "Sobredispersion detectada (ratio > 1.5). Considere familia = 'quasipoisson' o 'negativa_binomial'."
        )
      } else if (dispersion$ratio <= 1.5) {
        cli::cli_alert_success("Dispersion dentro del rango aceptable (<= 1.5).")
      }
    }
  }

  # Odds ratios para familias binomiales
  familias_bin <- c("binomial", "binomial_probit", "binomial_cloglog", "quasibinomial")
  if (familia %in% familias_bin && inherits(modelo_nat, "glm")) {
    cli::cli_alert_info("Odds ratios de efectos fijos (exponenciado de coeficientes):")
    print(analizar_odds_ratio(modelo_nat))
  }

  # Guía adicional por familia
  if (familia == "gamma") {
    cli::cli_alert_info("Gamma: verificar que todos los valores de la respuesta sean > 0.")
    cli::cli_alert_info("Para comparaciones post-hoc usa obtener_posthoc(..., tipo_respuesta = 'response').")
  }
  if (familia == "beta") {
    cli::cli_alert_info("Beta: verificar que todos los valores esten en el intervalo abierto (0, 1).")
    cli::cli_alert_info("Si hay valores exactamente 0 o 1, aplica la transformacion de Smithson & Verkuilen (2006).")
  }
  if (familia %in% c("zip", "zinb")) {
    cli::cli_alert_info("Modelo zero-inflado: verificar la proporcion de ceros vs esperados con DHARMa testZeroInflation().")
  }
  if (familia == "tweedie") {
    cli::cli_alert_info("Tweedie: el parametro de potencia p={tweedie_p}. p cerca de 1 = Poisson compuesto; p cerca de 2 = Gamma compuesto.")
  }

  # Diagnósticos DHARMa (donde sea aplicable)
  familias_dharma <- c("gaussian", "binomial", "binomial_probit", "binomial_cloglog",
                       "poisson", "quasipoisson", "quasibinomial", "negativa_binomial",
                       "gamma", "gamma_inverse", "gaussian_inversa", "tweedie", "zip", "zinb")

  if (isTRUE(diagnosticos) && familia %in% familias_dharma &&
      inherits(modelo_nat, c("glm", "negbin", "zeroinfl"))) {
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
      if (familia %in% c("poisson", "zip", "zinb", "negativa_binomial")) {
        print(DHARMa::testZeroInflation(residuos_dharma, plot = FALSE))
      }
    }
  } else if (isTRUE(diagnosticos) && familia %in% c("beta", "ordinal", "multinomial")) {
    cli::cli_alert_info("Generando graficos de diagnostico base...")
    old_par <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old_par), add = TRUE)
    tryCatch(graphics::plot(modelo_nat), error = function(e) NULL)
  }

  print(em)
  return(em)
}
