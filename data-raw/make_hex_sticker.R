# Generar Hex Sticker oficial de easyModels
#
# Ejecutar desde la raiz del paquete:
# source("data-raw/make_hex_sticker.R")
#
# Si falta hexSticker:
# install.packages("hexSticker")

if (!requireNamespace("hexSticker", quietly = TRUE)) {
  stop(
    "El paquete 'hexSticker' no esta instalado. Ejecuta: install.packages('hexSticker')",
    call. = FALSE
  )
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop(
    "El paquete 'ggplot2' no esta instalado. Ejecuta: install.packages('ggplot2')",
    call. = FALSE
  )
}

if (!dir.exists("man/figures")) {
  dir.create("man/figures", recursive = TRUE)
}

icono_modelos <- ggplot2::ggplot() +
  ggplot2::annotate(
    "segment",
    x = 0.08,
    xend = 0.92,
    y = 0.18,
    yend = 0.78,
    linewidth = 2.2,
    color = "#E8F1F2"
  ) +
  ggplot2::annotate(
    "point",
    x = c(0.16, 0.34, 0.52, 0.70, 0.88),
    y = c(0.24, 0.40, 0.46, 0.64, 0.76),
    size = 6,
    color = "#7FD1B9"
  ) +
  ggplot2::annotate(
    "point",
    x = c(0.16, 0.34, 0.52, 0.70, 0.88),
    y = c(0.24, 0.40, 0.46, 0.64, 0.76),
    size = 2.2,
    color = "#0B3D3A"
  ) +
  ggplot2::annotate(
    "text",
    x = 0.5,
    y = 0.08,
    label = "lm | GLM | LMM | GLMM",
    size = 5,
    fontface = "bold",
    color = "#E8F1F2"
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#0B3D3A", color = NA),
    panel.background = ggplot2::element_rect(fill = "#0B3D3A", color = NA)
  )

hexSticker::sticker(
  subplot = icono_modelos,
  package = "easyModels",
  filename = "man/figures/logo.png",
  p_size = 19,
  p_color = "#F7FFF7",
  p_family = "sans",
  p_fontface = "bold",
  h_fill = "#0B3D3A",
  h_color = "#7FD1B9",
  s_x = 1,
  s_y = 0.78,
  s_width = 0.78,
  s_height = 0.58,
  spotlight = TRUE,
  l_x = 1,
  l_y = 0.50,
  l_width = 4,
  l_height = 4,
  dpi = 320
)

message("Hex Sticker generado en: man/figures/logo.png")
