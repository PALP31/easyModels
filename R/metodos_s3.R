#' Imprimir un objeto easy_model
#'
#' Muestra un resumen estetico y corto del modelo ajustado utilizando
#' cajas y colores del paquete \code{cli}.
#'
#' @param x Un objeto de clase \code{easy_model}.
#' @param ... Argumentos adicionales no utilizados.
#'
#' @export
#' @method print easy_model
#' @importFrom cli cli_div boxx cli_alert_info cli_alert_success
#' @importFrom performance r2
print.easy_model <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange", fontface = "bold")))
  
  print(cli::boxx(
    paste0(" easyModels: ", x$tipo_modelo),
    padding = 1,
    float = "center",
    border_style = "round",
    col = "cyan"
  ))
  
  formula_str <- Reduce(paste, deparse(x$formula))
  
  cli::cli_alert_info("Respuesta: {.emph {x$respuesta}}")
  cli::cli_alert_info("Fórmula: {.code {formula_str}}")
  cli::cli_alert_info("Distribución: {.val {x$familia}} (Enlace: {.val {x$link}})")
  
  # R2 robusto: usa .safe_get para no fallar ante NA lógicos o vectores atómicos
  r2_val  <- tryCatch(suppressWarnings(performance::r2(x$modelo)), error = function(e) NULL)
  r2_cond <- .safe_get(r2_val, "R2_conditional")
  r2_marg <- .safe_get(r2_val, "R2_marginal")
  r2_plain <- .safe_get(r2_val, "R2")
  
  if (!is.na(r2_cond)) {
    cli::cli_alert_info("R\u00b2 Marginal: {.val {round(r2_marg, 3)}} | R\u00b2 Condicional: {.val {round(r2_cond, 3)}}")
  } else if (!is.na(r2_marg)) {
    cli::cli_alert_info("R\u00b2 Marginal: {.val {round(r2_marg, 3)}} | R\u00b2 Condicional: {.val NA (modelo singular)}")
  } else if (!is.na(r2_plain)) {
    cli::cli_alert_info("R\u00b2: {.val {round(r2_plain, 3)}}")
  }
  
  cli::cli_alert_success("Modelo listo para análisis post-hoc con 'obtener_posthoc()'.")
  invisible(x)
}


#' Imprimir un objeto easy_splitplot
#'
#' Muestra un resumen estético específico para modelos de parcelas divididas (Split-Plot).
#'
#' @param x Un objeto de clase \code{easy_splitplot}.
#' @param ... Argumentos adicionales no utilizados.
#'
#' @export
#' @method print easy_splitplot
#' @importFrom cli boxx cli_alert_info cli_alert_success
print.easy_splitplot <- function(x, ...) {
  print(cli::boxx(
    " easyModels: Diseño de Parcelas Divididas (Split-Plot) ",
    padding = 1,
    float = "center",
    border_style = "double",
    col = "green"
  ))
  
  formula_str <- Reduce(paste, deparse(x$formula))
  cli::cli_alert_info("Respuesta: {.val {x$respuesta}}")
  cli::cli_alert_info("Fórmula: {.code {formula_str}}")
  cli::cli_alert_info("Distribución: {.val {x$familia}} (Enlace: {.val {x$link}})")
  
  if (!is.null(x$anova)) {
    cli::cli_alert_success("Efectos aleatorios de parcela principal estructurados correctamente.")
  }
  invisible(x)
}

#' Resumen de un objeto easy_model
#'
#' Devuelve el resumen estadistico detallado del modelo nativo
#' y la tabla de ANOVA (usando \code{car::Anova} tipo III por defecto para modelos mixtos/split-plot).
#'
#' @param object Un objeto de clase \code{easy_model}.
#' @param ... Argumentos adicionales.
#'
#' @return Una lista con el resumen del modelo y la tabla ANOVA.
#' @export
#' @method summary easy_model
summary.easy_model <- function(object, ...) {
  resumen_nat <- summary(object$modelo)
  
  cat("\n====================================================\n")
  cat("             RESUMEN DEL MODELO NATIVO\n")
  cat("====================================================\n")
  print(resumen_nat)
  cat("\n====================================================\n")
  cat("             TABLA DE ANOVA (easyModels)\n")
  cat("====================================================\n")
  if (!is.null(object$anova)) {
    print(object$anova)
  } else {
    cat("No hay tabla ANOVA disponible para este modelo.\n")
  }
  cat("====================================================\n\n")
  
  invisible(list(resumen_nativo = resumen_nat, anova = object$anova))
}

#' Graficar diagnosticos de un objeto easy_model
#'
#' Genera y muestra los graficos de diagnostico correspondientes
#' según la familia y clase del modelo ajustado.
#'
#' @param x Un objeto de clase \code{easy_model}.
#' @param ... Argumentos adicionales pasados a \code{evaluar_modelo}.
#'
#' @export
#' @method plot easy_model
plot.easy_model <- function(x, ...) {
  evaluar_modelo(x, ...)
}
