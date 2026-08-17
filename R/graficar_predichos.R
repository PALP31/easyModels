#' Graficar valores predichos con emmeans
#'
#' Genera gráficos de medias marginales estimadas o predichos marginales usando
#' \code{emmeans}. Funciona con modelos unificados de clase \code{easy_model}
#' o modelos compatibles (\code{lm}, \code{glm}, \code{lmer}, \code{glmmTMB}).
#' Permite seleccionar formato de puntos, barras de publicación o líneas, aplicar
#' paletas científicas y agregar letras de significancia (Compact Letter Display - CLD).
#'
#' @param modelo Modelo ajustado (de clase \code{easy_model} o compatible con \code{emmeans}).
#' @param predictor Nombre del predictor que se graficará en el eje X.
#' @param por Variable opcional para separar líneas o grupos de color/relleno.
#' @param tipo_grafico Tipo de representación visual: \code{"puntos"} (por defecto), \code{"barras"} o \code{"lineas"}.
#' @param tipo_respuesta Escala de predicción: \code{"response"} para la escala biológica o \code{"link"}.
#' @param at Lista opcional para definir valores específicos de predicción en \code{emmeans}.
#' @param titulo Título del gráfico.
#' @param eje_x Etiqueta del eje X.
#' @param eje_y Etiqueta del eje Y.
#' @param mostrar_letras Valor lógico. Si es \code{TRUE}, calcula y muestra las letras de Tukey sobre los puntos o barras.
#' @param alfa_letras Nivel de significancia (alfa) para la asignación de letras (por defecto \code{0.05}).
#' @param paleta Paleta de colores: \code{"teal"}, \code{"viridis"}, \code{"cividis"}, \code{"set2"}, \code{"okabe_ito"}.
#'
#' @return Un objeto \code{ggplot} listo para publicación.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_col geom_errorbar geom_ribbon geom_text labs theme_classic theme element_text element_rect scale_color_manual scale_fill_manual scale_color_viridis_d scale_fill_viridis_d scale_color_brewer scale_fill_brewer position_dodge
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#' @importFrom cli cli_warn cli_abort
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   # Grafico de barras con letras de Tukey
#'   graficar_predichos(modelo, "Species", tipo_grafico = "barras", mostrar_letras = TRUE)
#' }
graficar_predichos <- function(modelo,
                               predictor,
                               por = NULL,
                               tipo_grafico = c("puntos", "barras", "lineas"),
                               tipo_respuesta = "response",
                               at = NULL,
                               titulo = "Valores predichos",
                               eje_x = predictor,
                               eje_y = "Predicción marginal",
                               mostrar_letras = FALSE,
                               alfa_letras = 0.05,
                               paleta = c("teal", "viridis", "cividis", "set2", "okabe_ito")) {
  tipo_grafico <- match.arg(tipo_grafico)
  paleta <- match.arg(paleta)
  m_nat <- extraer_modelo(modelo)
  
  validar_predictor_modelo(modelo, predictor)
  if (!is.null(por)) {
    validar_predictor_modelo(modelo, por)
  }

  specs <- if (is.null(por)) {
    stats::as.formula(paste("~", predictor))
  } else {
    stats::as.formula(paste("~", predictor, "|", por))
  }

  emm <- emmeans::emmeans(m_nat, specs = specs, at = at)
  datos <- as.data.frame(summary(emm, type = tipo_respuesta))
  y_col <- detectar_columna_prediccion(datos)
  intervalo <- detectar_intervalos(datos)
  es_numerico <- is.numeric(datos[[predictor]])

  # Calcular e integrar letras de Tukey (CLD)
  if (isTRUE(mostrar_letras)) {
    if (es_numerico) {
      warning("mostrar_letras = TRUE se ignora para predictores numéricos. Solo se admite para factores categóricos.", call. = FALSE)
    } else {
      cld_df <- if (requireNamespace("multcomp", quietly = TRUE) &&
        requireNamespace("multcompView", quietly = TRUE)) {
        tryCatch({
          res <- multcomp::cld(emm, Letters = letters, alpha = alfa_letras, type = tipo_respuesta)
          df_res <- as.data.frame(res)
          df_res$.group <- gsub(" ", "", as.character(df_res$.group))
          df_res
        }, error = function(e) {
          NULL
        })
      } else {
        NULL
      }
      
      if (!is.null(cld_df)) {
        orig_levels <- levels(datos[[predictor]])
        if (is.null(orig_levels)) orig_levels <- unique(datos[[predictor]])
        
        key_cols <- predictor
        if (!is.null(por)) {
          key_cols <- c(key_cols, por)
        }
        
        cld_sub <- cld_df[, c(key_cols, ".group"), drop = FALSE]
        datos <- merge(datos, cld_sub, by = key_cols, all.x = TRUE)
        datos[[predictor]] <- factor(datos[[predictor]], levels = orig_levels)
      }
    }
  }

  # Configurar base de ggplot
  dodge_w <- if (!is.null(por)) 0.8 else 0.2
  dodge <- ggplot2::position_dodge(width = dodge_w)

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data[[predictor]], y = .data[[y_col]])
  )

  # Tipo de gráfico: BARRAS
  if (tipo_grafico == "barras" && !es_numerico) {
    if (!is.null(por)) {
      grafico <- grafico +
        ggplot2::aes(fill = .data[[por]], group = .data[[por]]) +
        ggplot2::geom_col(position = dodge, width = 0.7, color = "black", linewidth = 0.3)
    } else {
      grafico <- grafico +
        ggplot2::geom_col(fill = "#00A88F", width = 0.6, color = "black", linewidth = 0.3)
    }

    if (!is.null(intervalo)) {
      grafico <- grafico +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[intervalo$inferior]],
            ymax = .data[[intervalo$superior]]
          ),
          position = dodge,
          width = 0.25,
          linewidth = 0.6
        )
    }

  # Tipo de gráfico: LÍNEAS
  } else if (tipo_grafico == "lineas" || es_numerico) {
    if (!is.null(por)) {
      grafico <- grafico +
        ggplot2::aes(color = .data[[por]], group = .data[[por]]) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2.8)
    } else {
      grafico <- grafico +
        ggplot2::geom_line(ggplot2::aes(group = 1), color = "#00A88F", linewidth = 0.8) +
        ggplot2::geom_point(color = "#00A88F", size = 2.8)
    }

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
              fill = "#00A88F"
            )
        }
      } else {
        grafico <- grafico +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              ymin = .data[[intervalo$inferior]],
              ymax = .data[[intervalo$superior]]
            ),
            width = 0.18,
            linewidth = 0.6
          )
      }
    }

  # Tipo de gráfico: PUNTOS (Default)
  } else {
    if (!is.null(por)) {
      grafico <- grafico +
        ggplot2::aes(color = .data[[por]], group = .data[[por]]) +
        ggplot2::geom_point(position = dodge, size = 3.0)
    } else {
      grafico <- grafico +
        ggplot2::geom_point(position = dodge, color = "#00A88F", size = 3.0)
    }

    if (!is.null(intervalo)) {
      grafico <- grafico +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[intervalo$inferior]],
            ymax = .data[[intervalo$superior]]
          ),
          position = dodge,
          width = 0.18,
          linewidth = 0.6
        )
    }
  }

  # Paletas de colores
  if (!is.null(por)) {
    if (paleta == "teal") {
      colores_teal <- c("#00A88F", "#E65100", "#1E88E5", "#8E24AA", "#43A047", "#D81B60")
      grafico <- grafico +
        ggplot2::scale_color_manual(values = colores_teal) +
        ggplot2::scale_fill_manual(values = colores_teal)
    } else if (paleta == "viridis") {
      grafico <- grafico +
        ggplot2::scale_color_viridis_d(option = "viridis") +
        ggplot2::scale_fill_viridis_d(option = "viridis")
    } else if (paleta == "cividis") {
      grafico <- grafico +
        ggplot2::scale_color_viridis_d(option = "cividis") +
        ggplot2::scale_fill_viridis_d(option = "cividis")
    } else if (paleta == "set2") {
      grafico <- grafico +
        ggplot2::scale_color_brewer(palette = "Set2") +
        ggplot2::scale_fill_brewer(palette = "Set2")
    } else if (paleta == "okabe_ito") {
      okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      grafico <- grafico +
        ggplot2::scale_color_manual(values = okabe) +
        ggplot2::scale_fill_manual(values = okabe)
    }
  }

  # Agregar letras de significancia al gráfico
  if (mostrar_letras && !es_numerico && ".group" %in% names(datos)) {
    y_text_col <- if (!is.null(intervalo)) intervalo$superior else y_col
    y_max <- max(datos[[y_text_col]], na.rm = TRUE)
    y_min <- min(datos[[if (!is.null(intervalo)) intervalo$inferior else y_col]], na.rm = TRUE)
    rango_y <- y_max - y_min
    offset <- if (rango_y > 0) rango_y * 0.06 else y_max * 0.06
    if (offset == 0) offset <- 0.1
    
    grafico <- grafico +
      ggplot2::geom_text(
        ggplot2::aes(
          y = .data[[y_text_col]] + offset,
          label = .data[[".group"]]
        ),
        position = if (!is.null(por)) dodge else ggplot2::position_identity(),
        vjust = 0,
        fontface = "bold",
        color = "black",
        size = 3.8,
        show.legend = FALSE
      )
  }

  # Configuración estética final
  grafico <- grafico +
    ggplot2::labs(title = titulo, x = eje_x, y = eje_y, color = por, fill = por) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.position = if (is.null(por)) "none" else "top",
      panel.grid.major.y = ggplot2::element_line(color = "grey92", linetype = "dashed")
    )

  return(grafico)
}
