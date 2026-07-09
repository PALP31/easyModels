#' Graficar valores predichos con emmeans
#'
#' Genera graficos de medias marginales estimadas o predichos marginales usando
#' \code{emmeans}. Funciona con modelos compatibles con \code{emmeans}, incluidos
#' \code{lm}, \code{glm}, \code{lmer} y \code{glmer}.
#'
#' @param modelo Modelo ajustado compatible con \code{emmeans}.
#' @param predictor Nombre del predictor que se graficara en el eje X.
#' @param por Variable opcional para separar lineas o grupos de color.
#' @param tipo_respuesta Escala de prediccion: \code{"response"} para la escala
#'   biologica o \code{"link"} para la escala del predictor lineal.
#' @param at Lista opcional para definir valores especificos de prediccion en
#'   \code{emmeans}.
#' @param titulo Titulo del grafico.
#' @param eje_x Etiqueta del eje X.
#' @param eje_y Etiqueta del eje Y.
#'
#' @return Un objeto \code{ggplot}.
#' @export
#' @importFrom rlang .data
graficar_predichos <- function(modelo,
                               predictor,
                               por = NULL,
                               tipo_respuesta = "response",
                               at = NULL,
                               titulo = "Valores predichos",
                               eje_x = predictor,
                               eje_y = "Prediccion marginal") {
  validar_predictor_modelo(modelo, predictor)
  if (!is.null(por)) {
    validar_predictor_modelo(modelo, por)
  }

  specs <- if (is.null(por)) {
    stats::as.formula(paste("~", predictor))
  } else {
    stats::as.formula(paste("~", predictor, "|", por))
  }

  emm <- emmeans::emmeans(modelo, specs = specs, at = at)
  datos <- as.data.frame(summary(emm, type = tipo_respuesta))
  y_col <- detectar_columna_prediccion(datos)
  intervalo <- detectar_intervalos(datos)
  es_numerico <- is.numeric(datos[[predictor]])

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data[[predictor]], y = .data[[y_col]])
  )

  if (!is.null(por)) {
    grafico <- grafico +
      ggplot2::aes(color = .data[[por]], group = .data[[por]])
  } else {
    grafico <- grafico +
      ggplot2::aes(group = 1)
  }

  if (es_numerico) {
    grafico <- grafico + ggplot2::geom_line(linewidth = 0.75)
  }

  grafico <- grafico +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::labs(title = titulo, x = eje_x, y = eje_y, color = por) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = if (is.null(por)) "none" else "top"
    )

  if (!is.null(intervalo)) {
    if (es_numerico) {
      if (!is.null(por)) {
        grafico <- grafico +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              ymin = .data[[intervalo$inferior]],
              ymax = .data[[intervalo$superior]],
              fill = .data[[por]]
            ),
            alpha = 0.18,
            color = NA
          )
      } else {
        grafico <- grafico +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              ymin = .data[[intervalo$inferior]],
              ymax = .data[[intervalo$superior]]
            ),
            alpha = 0.18,
            color = NA,
            fill = "#2C7FB8"
          )
      }
    } else {
      grafico <- grafico +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[intervalo$inferior]],
            ymax = .data[[intervalo$superior]]
          ),
          width = 0.12,
          linewidth = 0.55
        )
    }
  }

  return(grafico)
}
