#' Analizar un diseño de Bloques Completos al Azar (RCBD)
#'
#' Esta función es un wrapper especializado de \code{analizar_lmm} para ajustar y
#' analizar un diseño de Bloques Completos al Azar (RCBD - Randomized Complete Block Design).
#' Agrega automáticamente la estructura de efectos aleatorios para el factor de bloqueo
#' (\code{(1 | bloque)}). Retorna un objeto unificado de clase \code{easy_model}.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una fórmula de R o cadena de caracteres para los efectos fijos (ej. \code{y ~ tratamiento}).
#' @param bloque Nombre de la columna en el \code{data.frame} que identifica los bloques.
#' @param REML Valor lógico. Use \code{TRUE} para estimaciones precisas de varianza.
#' @param diagnosticos Valor lógico. Si es \code{TRUE}, genera gráficos de diagnóstico de residuos.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model} con tipo de modelo "RCBD".
#' @export
#' @importFrom cli cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     rendimiento = rnorm(30, mean = 10),
#'     tratamiento = factor(rep(c("Control", "Trat1", "Trat2"), each = 10)),
#'     Bloque = factor(rep(1:10, times = 3))
#'   )
#'   modelo <- analizar_bloques_azar(datos, rendimiento ~ tratamiento, "Bloque")
#'   print(modelo)
#' }
analizar_bloques_azar <- function(datos, formula_fijos, bloque, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("=== Iniciando Ajuste de Diseño de Bloques Completos al Azar (RCBD) ===")
  
  if (!is.character(bloque) || length(bloque) != 1) {
    cli::cli_abort("El argumento 'bloque' debe ser el nombre de una unica columna en 'datos'.")
  }
  
  if (!(bloque %in% names(datos))) {
    cli::cli_abort("La columna de bloque '{bloque}' no existe en los datos.")
  }
  
  aleatorios <- paste0("(1 | ", bloque, ")")
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  modelo$tipo_modelo <- "RCBD"
  return(modelo)
}

#' Analizar un diseño de Parcelas Divididas (Split-Plot)
#'
#' Esta función ajusta y analiza un modelo lineal mixto para un diseño de
#' parcelas divididas (Split-Plot), estándar en agronomía y ciencias biológicas
#' donde un factor principal (ej. riego, calor) se aplica a la parcela mayor y
#' un subfactor (ej. genotipo, fertilización) a las subparcelas.
#' Retorna un objeto unificado de clase combinada \code{c("easy_splitplot", "easy_model")}.
#'
#' @param datos Un \code{data.frame} que contiene las variables del modelo.
#' @param formula_fijos Una fórmula de R para la parte de efectos fijos (ej. \code{y ~ riego * genotipo}).
#' @param bloque Nombre de la columna que identifica el factor de bloque (ej. \code{"Bloque"}).
#' @param parcela_principal Nombre de la columna que identifica el factor asignado a la parcela principal (ej. \code{"Riego"}).
#' @param REML Valor lógico. Use \code{TRUE} para estimaciones de componentes de varianza.
#' @param diagnosticos Valor lógico. Si es \code{TRUE}, genera gráficos de diagnóstico de residuos.
#'
#' @return Un objeto unificado S3 de clase combinada \code{c("easy_splitplot", "easy_model")} con tipo de modelo "Split-Plot".
#' @export
#' @importFrom cli cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#'   datos <- data.frame(
#'     rendimiento = rnorm(48),
#'     Riego = factor(rep(c("Riego", "Secano"), each = 24)),
#'     Genotipo = factor(rep(rep(c("G1", "G2", "G3"), each = 8), times = 2)),
#'     Bloque = factor(rep(1:4, times = 12))
#'   )
#'   modelo <- analizar_parcelas_divididas(
#'     datos = datos,
#'     formula_fijos = rendimiento ~ Riego * Genotipo,
#'     bloque = "Bloque",
#'     parcela_principal = "Riego"
#'   )
#'   print(modelo)
#' }
analizar_parcelas_divididas <- function(datos, formula_fijos, bloque, parcela_principal, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("=== Iniciando Ajuste de Diseño de Parcelas Divididas (Split-Plot) ===")
  
  if (!is.character(bloque) || length(bloque) != 1) {
    cli::cli_abort("El argumento 'bloque' debe ser el nombre de una unica columna en 'datos'.")
  }
  if (!(bloque %in% names(datos))) {
    cli::cli_abort("La columna de bloque '{bloque}' no existe en los datos.")
  }
  if (!is.character(parcela_principal) || length(parcela_principal) != 1) {
    cli::cli_abort("El argumento 'parcela_principal' debe ser el nombre de una unica columna en 'datos'.")
  }
  if (!(parcela_principal %in% names(datos))) {
    cli::cli_abort("La columna de parcela principal '{parcela_principal}' no existe en los datos.")
  }
  
  # Error de parcela principal: bloque:parcela_principal
  aleatorios <- paste0("(1 | ", bloque, ") + (1 | ", bloque, ":", parcela_principal, ")")
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  modelo$tipo_modelo <- "Split-Plot"
  class(modelo) <- c("easy_splitplot", "easy_model")
  return(modelo)
}

#' Analizar un diseño de Cuadrado Latino (Latin Square Design)
#'
#' Ajusta un modelo lineal o mixto para diseños en Cuadrado Latino (LSD),
#' controlando dos fuentes ortogonales de variación ambiental (Filas y Columnas).
#'
#' @param datos Un \code{data.frame} que contiene las variables experimentales.
#' @param formula_fijos Fórmula para los tratamientos (ej. \code{y ~ tratamiento}).
#' @param fila Nombre de la columna de filas (factor de bloqueo 1).
#' @param columna Nombre de la columna de columnas (factor de bloqueo 2).
#' @param aleatorios_bloque Valor lógico. Si es \code{TRUE} (predeterminado), ajusta filas y columnas como efectos aleatorios cruzados mediante LMM (\code{(1|fila) + (1|columna)}). Si es \code{FALSE}, los ajusta como efectos fijos en LM.
#' @param REML Valor lógico para LMM (predeterminado \code{TRUE}).
#' @param diagnosticos Valor lógico. Si es \code{TRUE}, genera diagnósticos de residuos.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model} con tipo de modelo "Cuadrado-Latino".
#' @export
#' @importFrom cli cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#'   datos_lat <- data.frame(
#'     rendimiento = rnorm(16, mean = 20),
#'     Tratamiento = factor(rep(LETTERS[1:4], each = 4)),
#'     Fila = factor(rep(1:4, times = 4)),
#'     Columna = factor(rep(1:4, each = 4))
#'   )
#'   modelo_lat <- analizar_latino(datos_lat, rendimiento ~ Tratamiento, "Fila", "Columna")
#'   print(modelo_lat)
#' }
analizar_latino <- function(datos, formula_fijos, fila, columna, aleatorios_bloque = TRUE, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("=== Iniciando Ajuste de Diseño en Cuadrado Latino (LSD) ===")
  
  if (!is.character(fila) || !(fila %in% names(datos))) {
    cli::cli_abort("La columna de fila '{fila}' no existe en los datos.")
  }
  if (!is.character(columna) || !(columna %in% names(datos))) {
    cli::cli_abort("La columna de columna '{columna}' no existe en los datos.")
  }
  
  if (isTRUE(aleatorios_bloque)) {
    aleatorios <- paste0("(1 | ", fila, ") + (1 | ", columna, ")")
    modelo <- analizar_lmm(
      datos = datos,
      formula_fijos = formula_fijos,
      aleatorios = aleatorios,
      REML = REML,
      diagnosticos = diagnosticos
    )
  } else {
    f_str <- paste(Reduce(paste, deparse(formula_fijos)), "+", fila, "+", columna)
    modelo <- analizar_lm(
      datos = datos,
      formula = stats::as.formula(f_str),
      diagnosticos = diagnosticos
    )
  }
  
  modelo$tipo_modelo <- "Cuadrado-Latino"
  return(modelo)
}

#' Analizar experimentos con Medidas Repetidas en el Tiempo
#'
#' Ajusta modelos lineales mixtos para experimentos longitudinales o con
#' medidas repetidas sobre la misma unidad experimental (planta, maceta, individuo).
#' Controla la correlación intrasujeto estructurando interceptos aleatorios por individuo
#' o anidados dentro de bloques.
#'
#' @param datos Un \code{data.frame} que contiene las variables del ensayo longitudinal.
#' @param formula_fijos Fórmula de efectos fijos, típicamente incluyendo la interacción tratamiento × tiempo (ej. \code{y ~ tratamiento * tiempo}).
#' @param sujeto Nombre de la columna que identifica la unidad experimental individual repetida (ej. \code{"ID_Planta"}, \code{"Maceta"}).
#' @param bloque Nombre de columna opcional si las unidades experimentales están organizadas en bloques de campo o invernadero.
#' @param REML Valor lógico para LMM (predeterminado \code{TRUE}).
#' @param diagnosticos Valor lógico para gráficos de residuos.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model} con tipo de modelo "Medidas-Repetidas".
#' @export
#' @importFrom cli cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#'   datos_rep <- data.frame(
#'     altura = rnorm(60, mean = 25),
#'     tratamiento = factor(rep(c("Ctrl", "Estres"), each = 30)),
#'     tiempo = factor(rep(rep(c("T1", "T2", "T3"), each = 10), times = 2)),
#'     sujeto = factor(rep(1:20, times = 3)),
#'     bloque = factor(rep(1:4, each = 15))
#'   )
#'   modelo_rep <- analizar_medidas_repetidas(
#'     datos = datos_rep,
#'     formula_fijos = altura ~ tratamiento * tiempo,
#'     sujeto = "sujeto",
#'     bloque = "bloque"
#'   )
#'   print(modelo_rep)
#' }
analizar_medidas_repetidas <- function(datos, formula_fijos, sujeto, bloque = NULL, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("=== Iniciando Ajuste de Modelo con Medidas Repetidas en el Tiempo ===")
  
  if (!is.character(sujeto) || !(sujeto %in% names(datos))) {
    cli::cli_abort("La columna de sujeto '{sujeto}' no existe en los datos.")
  }
  
  if (!is.null(bloque)) {
    if (!is.character(bloque) || !(bloque %in% names(datos))) {
      cli::cli_abort("La columna de bloque '{bloque}' no existe en los datos.")
    }
    # Sujeto anidado en bloque: (1 | bloque) + (1 | bloque:sujeto)
    aleatorios <- paste0("(1 | ", bloque, ") + (1 | ", bloque, ":", sujeto, ")")
  } else {
    aleatorios <- paste0("(1 | ", sujeto, ")")
  }
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  modelo$tipo_modelo <- "Medidas-Repetidas"
  return(modelo)
}

#' Analizar un diseño de Bloques Divididos (Strip-Plot / Criss-Cross)
#'
#' Ajusta un modelo lineal mixto para diseños en franjas o bloques divididos (Strip-Plot),
#' donde dos factores principales se aplican en franjas perpendiculares que se intersectan
#' dentro de cada bloque experimental.
#'
#' @param datos Un \code{data.frame} con las variables experimentales.
#' @param formula_fijos Fórmula de efectos fijos (ej. \code{y ~ FactorA * FactorB}).
#' @param bloque Nombre de la columna de bloques.
#' @param factor_a Nombre de la columna del Factor A (franja vertical).
#' @param factor_b Nombre de la columna del Factor B (franja horizontal).
#' @param REML Valor lógico para LMM (predeterminado \code{TRUE}).
#' @param diagnosticos Valor lógico para gráficos de residuos.
#'
#' @return Un objeto unificado S3 de clase \code{easy_model} con tipo de modelo "Strip-Plot".
#' @export
#' @importFrom cli cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#'   datos_strip <- data.frame(
#'     rendimiento = rnorm(36),
#'     Labranza = factor(rep(c("Convencional", "Cero"), each = 18)),
#'     Fertilizacion = factor(rep(rep(c("N0", "N50", "N100"), each = 6), times = 2)),
#'     Bloque = factor(rep(1:6, times = 6))
#'   )
#'   modelo_strip <- analizar_strip_plot(
#'     datos = datos_strip,
#'     formula_fijos = rendimiento ~ Labranza * Fertilizacion,
#'     bloque = "Bloque",
#'     factor_a = "Labranza",
#'     factor_b = "Fertilizacion"
#'   )
#'   print(modelo_strip)
#' }
analizar_strip_plot <- function(datos, formula_fijos, bloque, factor_a, factor_b, REML = TRUE, diagnosticos = TRUE) {
  cli::cli_alert_info("=== Iniciando Ajuste de Diseño en Franjas Divididas (Strip-Plot) ===")
  
  cols_req <- c(bloque, factor_a, factor_b)
  for (col in cols_req) {
    if (!is.character(col) || !(col %in% names(datos))) {
      cli::cli_abort("La columna '{col}' no existe en el data.frame.")
    }
  }
  
  # Error Factor A + Error Factor B + Error Interacción AxB en bloque
  aleatorios <- paste0(
    "(1 | ", bloque, ") + (1 | ", bloque, ":", factor_a, ") + (1 | ", bloque, ":", factor_b, ")"
  )
  
  modelo <- analizar_lmm(
    datos = datos,
    formula_fijos = formula_fijos,
    aleatorios = aleatorios,
    REML = REML,
    diagnosticos = diagnosticos
  )
  
  modelo$tipo_modelo <- "Strip-Plot"
  return(modelo)
}
