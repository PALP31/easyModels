#' Graficar Interacciones entre Factores (Efectos Simples)
#'
#' Genera gráficos de interacción de alta calidad para modelos estadísticos
#' (\code{easy_model}, \code{lm}, \code{lmer}, \code{glmmTMB}), mostrando
#' líneas de tendencia, intervalos de confianza del 95\% y medias estimadas por \code{emmeans}.
#'
#' @param modelo Un objeto de clase \code{easy_model} o modelo compatible.
#' @param factor_x Nombre del factor en el eje X (cadena de caracteres).
#' @param factor_traza Nombre del factor de agrupación/color para las líneas y leyendas.
#' @param tipo_respuesta Escala: \code{"response"} o \code{"link"}.
#' @param titulo Título del gráfico.
#' @param eje_x Etiqueta del eje X.
#' @param eje_y Etiqueta del eje Y.
#' @param leyenda Título de la leyenda.
#' @param paleta Paleta de colores: \code{"teal"}, \code{"viridis"}, \code{"cividis"}, \code{"set2"}, \code{"okabe_ito"}.
#'
#' @return Un objeto \code{ggplot} listo para publicación.
#' @export
#'
#' @importFrom stats as.formula
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar labs theme_classic theme element_text element_rect scale_color_viridis_d scale_color_brewer position_dodge
#' @importFrom emmeans emmeans
#'
#' @examples
#' \dontrun{
#'   m <- analizar_lm(iris, Sepal.Length ~ Species * Petal.Width, diagnosticos = FALSE)
#'   # Grafico de interaccion
#' }
graficar_interaccion <- function(modelo,
                                 factor_x,
                                 factor_traza,
                                 tipo_respuesta = "response",
                                 titulo = "Gráfico de Interacción (Medias Marginales)",
                                 eje_x = factor_x,
                                 eje_y = "Respuesta Estimada",
                                 leyenda = factor_traza,
                                 paleta = c("teal", "viridis", "cividis", "set2", "okabe_ito")) {
  paleta <- match.arg(paleta)
  m_nat <- extraer_modelo(modelo)
  
  validar_predictor_modelo(m_nat, factor_x)
  validar_predictor_modelo(m_nat, factor_traza)
  
  formula_specs <- stats::as.formula(paste("~", factor_x, "*", factor_traza))
  emm <- emmeans::emmeans(m_nat, specs = formula_specs, type = tipo_respuesta)
  datos <- as.data.frame(summary(emm, type = tipo_respuesta))
  
  y_col <- detectar_columna_prediccion(datos)
  intervalo <- detectar_intervalos(datos)
  
  # Esquema de esquivar posiciones para evitar solapamientos
  dodge <- ggplot2::position_dodge(width = 0.2)
  
  p <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = .data[[factor_x]],
      y = .data[[y_col]],
      color = .data[[factor_traza]],
      group = .data[[factor_traza]]
    )
  ) +
    ggplot2::geom_line(position = dodge, linewidth = 0.85) +
    ggplot2::geom_point(position = dodge, size = 3.2)
  
  if (!is.null(intervalo)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data[[intervalo$inferior]],
        ymax = .data[[intervalo$superior]]
      ),
      position = dodge,
      width = 0.18,
      linewidth = 0.65
    )
  }
  
  # Aplicar paleta
  if (paleta == "teal") {
    colores_teal <- c("#00A88F", "#E65100", "#1E88E5", "#8E24AA", "#43A047", "#D81B60")
    p <- p + ggplot2::scale_color_manual(values = colores_teal)
  } else if (paleta == "viridis") {
    p <- p + ggplot2::scale_color_viridis_d(option = "viridis")
  } else if (paleta == "cividis") {
    p <- p + ggplot2::scale_color_viridis_d(option = "cividis")
  } else if (paleta == "set2") {
    p <- p + ggplot2::scale_color_brewer(palette = "Set2")
  } else if (paleta == "okabe_ito") {
    okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    p <- p + ggplot2::scale_color_manual(values = okabe)
  }
  
  p <- p +
    ggplot2::labs(
      title = titulo,
      x = eje_x,
      y = eje_y,
      color = leyenda
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.position = "top",
      panel.grid.major.y = ggplot2::element_line(color = "grey92", linetype = "dashed")
    )
  
  return(p)
}
