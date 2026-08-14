#' Exportar Tabla ANOVA lista para publicación
#'
#' Transforma la tabla ANOVA de un objeto \code{easy_model} o tabla de \code{car::Anova}
#' en un formato limpio, formateando grados de libertad, estadísticos F/Chisq,
#' p-valores con estrellas de significancia (\code{***}, \code{**}, \code{*}, \code{ns})
#' y calculando tamaños del efecto (Eta-cuadrado parcial \eqn{\eta_p^2}).
#'
#' @param modelo Un objeto de clase \code{easy_model} o una tabla ANOVA.
#' @param formato Formato de salida: \code{"data.frame"} (por defecto), \code{"markdown"} o \code{"latex"}.
#' @param digitos Número de decimales para redondear estadísticos (por defecto \code{3}).
#'
#' @return Un \code{data.frame} formateado o una cadena de texto en Markdown/LaTeX lista para pegar en artículos o tesis.
#' @export
#'
#' @importFrom stats anova
#' @importFrom knitr kable
#' @importFrom cli cli_alert_success
#'
#' @examples
#' \dontrun{
#'   m <- analizar_lm(iris, Sepal.Length ~ Species * Sepal.Width, diagnosticos = FALSE)
#'   exportar_tabla_anova(m, formato = "markdown")
#' }
exportar_tabla_anova <- function(modelo, formato = c("data.frame", "markdown", "latex"), digitos = 3) {
  formato <- match.arg(formato)
  
  tab_raw <- if (inherits(modelo, "easy_model")) {
    modelo$anova
  } else if (inherits(modelo, "anova") || inherits(modelo, "Anova.data.frame") || is.data.frame(modelo)) {
    modelo
  } else {
    tryCatch(stats::anova(extraer_modelo(modelo)), error = function(e) NULL)
  }
  
  if (is.null(tab_raw)) {
    stop("No se pudo extraer la tabla ANOVA del modelo.", call. = FALSE)
  }
  
  df_raw <- as.data.frame(tab_raw)
  filas <- rownames(df_raw)
  
  # Detectar columnas
  col_p <- grep("Pr\\(>|p-value|p.value", names(df_raw), value = TRUE)[1]
  col_stat <- grep("F value|F|Chisq|statistic", names(df_raw), value = TRUE)[1]
  col_df <- grep("^Df$|NumDF", names(df_raw), value = TRUE)[1]
  col_ss <- grep("Sum Sq|Sum of Sq", names(df_raw), value = TRUE)[1]
  
  res_df <- data.frame(
    Fuente = filas,
    stringsAsFactors = FALSE
  )
  
  if (!is.na(col_df)) res_df$gl <- df_raw[[col_df]]
  
  if (!is.na(col_ss)) {
    res_df$Suma_Cuadrados <- round(df_raw[[col_ss]], digitos)
    # Calcular eta-cuadrado parcial si hay residuos
    idx_res <- grep("Residuals|Residual", filas, ignore.case = TRUE)
    if (length(idx_res) > 0) {
      ss_res <- df_raw[idx_res, col_ss]
      eta_sq <- df_raw[[col_ss]] / (df_raw[[col_ss]] + ss_res)
      eta_sq[idx_res] <- NA
      res_df$Eta_Parcial_Sq <- round(eta_sq, digitos)
    }
  }
  
  if (!is.na(col_stat)) {
    nombre_stat <- if (grepl("Chisq", col_stat, ignore.case = TRUE)) "Chisq" else "F_valor"
    res_df[[nombre_stat]] <- round(df_raw[[col_stat]], digitos)
  }
  
  if (!is.na(col_p)) {
    p_vals <- df_raw[[col_p]]
    p_txt <- ifelse(is.na(p_vals), "-",
             ifelse(p_vals < 0.001, "< 0.001 ***",
             ifelse(p_vals < 0.01, paste0(sprintf(paste0("%.", digitos, "f"), p_vals), " **"),
             ifelse(p_vals < 0.05, paste0(sprintf(paste0("%.", digitos, "f"), p_vals), " *"),
             paste0(sprintf(paste0("%.", digitos, "f"), p_vals), " ns")))))
    res_df$p_valor <- p_txt
  }
  
  if (formato == "markdown") {
    if (requireNamespace("knitr", quietly = TRUE)) {
      salida <- knitr::kable(res_df, format = "markdown", caption = "Tabla ANOVA de Efectos Fijos (easyModels)")
      cat(salida, sep = "\n")
      return(invisible(salida))
    }
  } else if (formato == "latex") {
    if (requireNamespace("knitr", quietly = TRUE)) {
      salida <- knitr::kable(res_df, format = "latex", booktabs = TRUE, caption = "Tabla ANOVA de Efectos Fijos (easyModels)")
      cat(salida, sep = "\n")
      return(invisible(salida))
    }
  }
  
  return(res_df)
}

#' Exportar Tabla de Medias y Letras Tukey para Publicación
#'
#' Formatea la tabla de medias post-hoc con letras de significancia (CLD)
#' para su inclusión directa en manuscritos científicos o tesis.
#'
#' @param posthoc_res Resultado de \code{obtener_posthoc(..., letras = TRUE)}.
#' @param formato Formato: \code{"data.frame"} (por defecto), \code{"markdown"} o \code{"latex"}.
#' @param digitos Decimales para redondear las medias y errores estándar.
#'
#' @return Un \code{data.frame} formateado o cadena Markdown/LaTeX.
#' @export
#'
#' @importFrom knitr kable
#'
#' @examples
#' \dontrun{
#'   m <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   ph <- obtener_posthoc(m, "Species", letras = TRUE)
#'   exportar_tabla_posthoc(ph, formato = "markdown")
#' }
exportar_tabla_posthoc <- function(posthoc_res, formato = c("data.frame", "markdown", "latex"), digitos = 2) {
  formato <- match.arg(formato)
  df <- as.data.frame(posthoc_res)
  
  # Formatear columnas numericas
  num_cols <- vapply(df, is.numeric, logical(1))
  df[num_cols] <- lapply(df[num_cols], function(x) round(x, digitos))
  
  if (formato == "markdown" && requireNamespace("knitr", quietly = TRUE)) {
    salida <- knitr::kable(df, format = "markdown", caption = "Medias Marginales Estimadas y Comparaciones Tukey (CLD)")
    cat(salida, sep = "\n")
    return(invisible(salida))
  } else if (formato == "latex" && requireNamespace("knitr", quietly = TRUE)) {
    salida <- knitr::kable(df, format = "latex", booktabs = TRUE, caption = "Medias Marginales Estimadas y Comparaciones Tukey (CLD)")
    cat(salida, sep = "\n")
    return(invisible(salida))
  }
  
  return(df)
}
