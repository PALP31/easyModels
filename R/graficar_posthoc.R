#' Graficar comparaciones post-hoc
#'
#' Esta funcion recibe la tabla generada por \code{obtener_posthoc()} y devuelve
#' un grafico limpio para estimaciones, razones de tasas u odds ratios.
#'
#' @param tabla_posthoc Un \code{data.frame} generado por \code{obtener_posthoc()}.
#'   Debe incluir \code{contrast} y una columna de resultado reconocible:
#'   \code{estimate}, \code{odds.ratio}, \code{ratio}, \code{rate.ratio} o
#'   \code{response}.
#' @param eje_x Texto para el eje X. Por defecto, \code{"Comparaciones"}.
#' @param eje_y Texto para el eje Y. Por defecto, \code{"Diferencia estimada"}.
#' @param titulo Titulo opcional del grafico.
#' @param nivel_confianza Nivel de confianza usado para construir los intervalos
#'   aproximados a partir de \code{estimate +/- z * SE}. Por defecto, \code{0.95}.
#' @param columna_y Columna que se desea graficar. Si es \code{NULL}, se detecta
#'   automaticamente.
#'
#' @return Un objeto \code{ggplot} con las comparaciones post-hoc.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   modelo <- lm(Sepal.Length ~ Species, data = iris)
#'   posthoc <- obtener_posthoc(modelo, "Species")
#'   graficar_posthoc(posthoc, eje_x = "Especies", eje_y = "Diferencia estimada")
#' }
graficar_posthoc <- function(tabla_posthoc,
                             eje_x = "Comparaciones",
                             eje_y = "Diferencia estimada",
                             titulo = "Comparaciones post-hoc",
                             nivel_confianza = 0.95,
                             columna_y = NULL) {
  columnas_requeridas <- c("contrast", "p.value")
  columnas_faltantes <- setdiff(columnas_requeridas, names(tabla_posthoc))

  if (length(columnas_faltantes) > 0) {
    stop(
      "La tabla post-hoc no contiene las columnas requeridas: ",
      paste(columnas_faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(nivel_confianza) ||
      length(nivel_confianza) != 1 ||
      nivel_confianza <= 0 ||
      nivel_confianza >= 1) {
    stop("nivel_confianza debe ser un numero entre 0 y 1.", call. = FALSE)
  }

  if (is.null(columna_y)) {
    columna_y <- detectar_columna_contraste(tabla_posthoc)
  }

  datos_grafico <- tabla_posthoc
  datos_grafico$contrast <- factor(
    datos_grafico$contrast,
    levels = datos_grafico$contrast[order(datos_grafico[[columna_y]])]
  )

  intervalo <- detectar_intervalos(datos_grafico)
  if (is.null(intervalo) && all(c("SE", columna_y) %in% names(datos_grafico))) {
    alfa <- 1 - nivel_confianza
    z <- stats::qnorm(1 - alfa / 2)
    datos_grafico$limite_inferior <- datos_grafico[[columna_y]] - z * datos_grafico$SE
    datos_grafico$limite_superior <- datos_grafico[[columna_y]] + z * datos_grafico$SE
    intervalo <- list(inferior = "limite_inferior", superior = "limite_superior")
  }

  datos_grafico$significancia <- ifelse(datos_grafico$p.value < 0.05, "p < 0.05", "ns")
  referencia <- if (columna_y %in% c("odds.ratio", "ratio", "rate.ratio")) 1 else 0

  ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(
      x = .data[["contrast"]],
      y = .data[[columna_y]],
      fill = .data[["significancia"]]
    )
  ) +
    ggplot2::geom_col(width = 0.72, color = "gray25", linewidth = 0.25) +
    {
      if (!is.null(intervalo)) {
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[intervalo$inferior]],
            ymax = .data[[intervalo$superior]]
          ),
          width = 0.18,
          linewidth = 0.55,
          color = "gray20"
        )
      }
    } +
    ggplot2::geom_hline(yintercept = referencia, linewidth = 0.45, linetype = "dashed", color = "gray35") +
    ggplot2::scale_fill_manual(
      values = c("p < 0.05" = "#2C7FB8", "ns" = "#BDBDBD"),
      name = "Significancia"
    ) +
    ggplot2::labs(
      title = titulo,
      x = eje_x,
      y = eje_y
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.y = ggplot2::element_text(color = "gray15"),
      axis.text.x = ggplot2::element_text(color = "gray15"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "top"
    )
}
