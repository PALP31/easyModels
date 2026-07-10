#' Obtener medias marginales estimadas (EMMeans)
#'
#' Esta funcion calcula las medias marginales estimadas (EMMeans) para uno o
#' más predictores a partir de un modelo unificado de clase \code{easy_model}
#' o un modelo nativo.
#'
#' @param modelo Un objeto de clase \code{easy_model} o un modelo nativo (lm, glm, merMod, etc.).
#' @param predictor Vector de caracteres con el nombre de los predictores a evaluar (ej. \code{"tratamiento"} o \code{c("tratamiento", "dosis")}).
#' @param by Vector de caracteres opcional con las variables condicionantes (ej. \code{"bloque"}).
#' @param tipo_respuesta Escala del resultado. Use \code{"response"} para la escala de respuesta biologica (probabilidad, tasa, conteos) o \code{"link"} para la escala lineal del predictor.
#'
#' @return Un objeto de clase \code{emmGrid}.
#' @export
#' @importFrom emmeans emmeans
#' @importFrom stats model.frame
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   obtener_emmeans(modelo, "Species")
#' }
obtener_emmeans <- function(modelo, predictor, by = NULL, tipo_respuesta = "response") {
  m_nat <- extraer_modelo(modelo)
  
  # Validar existencia de predictores
  datos_modelo <- stats::model.frame(m_nat)
  nombres_variables <- names(datos_modelo)
  
  todo_pred <- c(predictor, by)
  if (!all(todo_pred %in% nombres_variables)) {
    variables_invalidas <- setdiff(todo_pred, nombres_variables)
    cli::cli_abort(paste0(
      "Los siguientes predictores no se encuentran en el modelo: ",
      paste(paste0("'", variables_invalidas, "'"), collapse = ", ")
    ))
  }
  
  # Llamar a emmeans
  emm <- emmeans::emmeans(m_nat, specs = predictor, by = by, type = tipo_respuesta)
  return(emm)
}

#' Obtener comparaciones múltiples post-hoc
#'
#' Realiza comparaciones múltiples de medias post-hoc a partir de un modelo
#' de clase \code{easy_model} o un modelo nativo. Sostiene multiples predictores,
#' genera letras de significancia (CLD) y/o gráficos de publicación.
#'
#' @param modelo Un objeto de clase \code{easy_model} o un modelo nativo.
#' @param predictor Vector de caracteres con el nombre de los predictores (ej. \code{c("Tratamiento", "Dosis")}).
#' @param by Vector de caracteres opcional con variables condicionantes (agrupadoras).
#' @param contraste Tipo de contraste. Por defecto, \code{"pairwise"}.
#' @param ajuste Método de ajuste de p-valores. Por defecto, \code{"tukey"}.
#' @param tipo_respuesta Escala del resultado (ej. \code{"response"} o \code{"link"}).
#' @param infer Vector lógico de longitud 2. Define si se calculan intervalos de confianza y pruebas estadísticas (ej. \code{c(TRUE, TRUE)}).
#' @param letras Valor lógico. Si es \code{TRUE}, calcula e integra letras de significancia Tukey (Compact Letter Display - CLD) devolviendo la tabla de medias en lugar de comparaciones pareadas.
#' @param graficar Valor lógico. Si es \code{TRUE}, genera y retorna un gráfico ggplot listo para publicación (gráfico de comparaciones pareadas si letras es FALSE; gráfico de predichos si letras es TRUE).
#'
#' @return Un \code{data.frame} limpio con los contrastes post-hoc (o tabla de medias con letras si \code{letras = TRUE}), o un objeto de clase \code{ggplot} si \code{graficar = TRUE}.
#' @export
#' @importFrom emmeans contrast
#' @importFrom multcomp cld
#' @importFrom cli cli_alert_warning cli_warn
#'
#' @examples
#' \dontrun{
#'   modelo <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
#'   # Retornar tabla de diferencias pareadas
#'   obtener_posthoc(modelo, "Species")
#'   # Retornar medias del grupo con sus letras Tukey
#'   obtener_posthoc(modelo, "Species", letras = TRUE)
#' }
obtener_posthoc <- function(modelo,
                             predictor,
                             by = NULL,
                             contraste = "pairwise",
                             ajuste = "tukey",
                             tipo_respuesta = "response",
                             infer = c(TRUE, TRUE),
                             letras = FALSE,
                             graficar = FALSE) {
  
  # 1. Detectar interacciones significativas en el ANOVA
  if (is.null(by) && detectar_interaccion_significativa(modelo)) {
    cli::cli_alert_warning(
      "Se detectó una interacción significativa en el ANOVA, pero no se especificó el argumento {.var by}. Considere analizar efectos simples para evitar interpretaciones incorrectas."
    )
  }
  
  # 2. Calcular medias marginales estimadas (EMMeans)
  emm <- obtener_emmeans(modelo = modelo, predictor = predictor, by = by, tipo_respuesta = tipo_respuesta)
  
  # 3. Determinar el retorno basado en letras y graficos
  if (isTRUE(letras)) {
    cld_res <- tryCatch({
      if (!requireNamespace("multcomp", quietly = TRUE)) {
        cli::cli_abort("El paquete 'multcomp' es necesario para generar letras de significancia. Instálelo con: install.packages('multcomp')")
      }
      res <- multcomp::cld(emm, Letters = letters, alpha = 0.05, adjust = ajuste, type = tipo_respuesta)
      df_res <- as.data.frame(res)
      df_res$Grupo <- gsub(" ", "", as.character(df_res$.group))
      df_res$.group <- NULL
      df_res
    }, error = function(e) {
      cli::cli_warn("No se pudieron calcular las letras de significancia (CLD): {e$message}")
      NULL
    })
    
    tabla_retorno <- if (!is.null(cld_res)) cld_res else as.data.frame(summary(emm, type = tipo_respuesta))
    
    if (isTRUE(graficar)) {
      # Retornar grafico de predichos con letras
      return(graficar_predichos(
        modelo = modelo,
        predictor = predictor[1],
        por = by,
        tipo_respuesta = tipo_respuesta,
        mostrar_letras = TRUE,
        alfa_letras = 0.05
      ))
    }
    
    return(tabla_retorno)
  } else {
    # Calcular contrastes pareados
    contrastes <- emmeans::contrast(emm, method = contraste, adjust = ajuste)
    tabla_limpia <- as.data.frame(
      summary(contrastes, infer = infer, type = tipo_respuesta)
    )
    
    if (isTRUE(graficar)) {
      return(graficar_posthoc(tabla_limpia))
    }
    
    return(tabla_limpia)
  }
}

#' Detectar interacciones significativas en la tabla ANOVA
#'
#' Helper interno que inspecciona la tabla de ANOVA del modelo para detectar
#' si existe algún término de interacción significativo (p < 0.05).
#'
#' @param modelo Un objeto de clase \code{easy_model} o un modelo nativo.
#'
#' @return Valor lógico: \code{TRUE} si existe interacción significativa, de lo contrario \code{FALSE}.
#' @keywords internal
#' @importFrom car Anova
#' @importFrom stats anova
detectar_interaccion_significativa <- function(modelo) {
  tab_anova <- NULL
  if (inherits(modelo, "easy_model")) {
    tab_anova <- modelo$anova
  } else {
    m_nat <- extraer_modelo(modelo)
    tab_anova <- tryCatch({
      car::Anova(m_nat, type = 3)
    }, error = function(e) {
      tryCatch(stats::anova(m_nat), error = function(e2) NULL)
    })
  }
  
  if (is.null(tab_anova)) return(FALSE)
  
  df_anova <- as.data.frame(tab_anova)
  
  # Buscar columna de p-valor
  p_col <- grep("Pr\\(>|p-value|p.value", names(df_anova), value = TRUE)
  if (length(p_col) == 0) return(FALSE)
  p_col <- p_col[1]
  
  # Buscar terminos con ":" (interacciones)
  filas_interaccion <- grep(":", rownames(df_anova), value = TRUE)
  if (length(filas_interaccion) == 0) return(FALSE)
  
  for (term in filas_interaccion) {
    p_val <- df_anova[term, p_col]
    if (!is.null(p_val) && !is.na(p_val) && p_val < 0.05) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}
