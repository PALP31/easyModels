#' Graficar valores predichos con emmeans
#'
#' Genera graficos de medias marginales estimadas o predichos marginales usando
#' \code{emmeans}. Funciona con modelos unificados de clase \code{easy_model}
#' o modelos compatibles con \code{emmeans}, incluidos
#' \code{lm}, \code{glm}, \code{lmer} y \code{glmer}.
#' Adicionalmente, puede incorporar letras de significancia (Compact Letter Display - CLD)
#' para representar diferencias significativas de Tukey.
#'
#' @param modelo Modelo ajustado (de clase \code{easy_model} o compatible con \code{emmeans}).
#' @param predictor Nombre del predictor que se graficara en el eje X.
#' @param por Variable opcional para separar lineas o grupos de color.
#' @param tipo_respuesta Escala de prediccion: \code{"response"} para la escala
#'   biologica o \code{"link"} para la escala del predictor lineal.
#' @param at Lista opcional para definir valores especificos de prediccion en
#'   \code{emmeans}.
#' @param titulo Titulo del grafico.
#' @param eje_x Etiqueta del eje X.
#' @param eje_y Etiqueta del eje Y.
#' @param mostrar_letras Valor logico. Si es \code{TRUE}, calcula y muestra las
#'   letras de significancia de Tukey sobre los puntos o barras de error.
#'   (Requiere el paquete \code{multcomp}).
#' @param alfa_letras Nivel de significancia (alfa) para la asignacion de letras.
#'   Por defecto, \code{0.05}.
#'
#' @return Un objeto \code{ggplot}.
#' @export
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_classic theme element_text geom_ribbon geom_errorbar geom_text
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   # Grafico con letras de Tukey
#'   graficar_predichos(modelo, "Species", mostrar_letras = TRUE)
#' }
graficar_predichos <- function(modelo,
                               predictor,
                               por = NULL,
                               tipo_respuesta = "response",
                               at = NULL,
                               titulo = "Valores predichos",
                               eje_x = predictor,
                               eje_y = "Prediccion marginal",
                               mostrar_letras = FALSE,
                               alfa_letras = 0.05) {
  m_nat <- extraer_modelo(modelo)
  
  validar_predictor_modelo(m_nat, predictor)
  if (!is.null(por)) {
    validar_predictor_modelo(m_nat, por)
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
      warning("mostrar_letras = TRUE se ignora para predictores numericos. Solo se admite para predictores categoricos (factores).", call. = FALSE)
    } else {
      if (!requireNamespace("multcomp", quietly = TRUE)) {
        stop("El paquete 'multcomp' es necesario para mostrar las letras de significancia. Instale con install.packages('multcomp').", call. = FALSE)
      }
      
      cld_df <- tryCatch({
        res <- multcomp::cld(emm, Letters = letters, alpha = alfa_letras, type = tipo_respuesta)
        df_res <- as.data.frame(res)
        df_res$.group <- gsub(" ", "", as.character(df_res$.group))
        df_res
      }, error = function(e) {
        warning("No se pudieron calcular las letras de significancia (CLD): ", e$message, call. = FALSE)
        NULL
      })
      
      if (!is.null(cld_df)) {
        orig_levels <- levels(datos[[predictor]])
        if (is.null(orig_levels)) orig_levels <- unique(datos[[predictor]])
        
        key_cols <- predictor
        if (!is.null(por)) {
          key_cols <- c(key_cols, por)
        }
        
        # Combinar
        cld_sub <- cld_df[, c(key_cols, ".group"), drop = FALSE]
        datos <- merge(datos, cld_sub, by = key_cols, all.x = TRUE)
        
        # Restaurar orden
        datos[[predictor]] <- factor(datos[[predictor]], levels = orig_levels)
      }
    }
  }

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

  # Agregar letras de significancia al grafico si aplica
  if (mostrar_letras && !es_numerico && ".group" %in% names(datos)) {
    y_text_col <- if (!is.null(intervalo)) {
      intervalo$superior
    } else {
      y_col
    }
    
    y_max <- max(datos[[y_text_col]], na.rm = TRUE)
    y_min <- min(datos[[if (!is.null(intervalo)) intervalo$inferior else y_col]], na.rm = TRUE)
    rango_y <- y_max - y_min
    offset <- if (rango_y > 0) rango_y * 0.05 else y_max * 0.05
    if (offset == 0) offset <- 0.1
    
    grafico <- grafico +
      ggplot2::geom_text(
        ggplot2::aes(
          y = .data[[y_text_col]] + offset,
          label = .data[[".group"]]
        ),
        vjust = 0,
        fontface = "bold",
        color = "black",
        show.legend = FALSE
      )
  }

  return(grafico)
}
