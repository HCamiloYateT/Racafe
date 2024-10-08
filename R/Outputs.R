#' Generar una Caja con Ícono y Texto
#'
#' Esta función crea una caja visual que incluye un ícono y un texto. La caja puede personalizarse en cuanto a color de fondo, altura, color de letra e ícono.
#'
#' @param texto Un carácter que representa el texto que se mostrará en la caja.
#' @param icono Un carácter que representa el nombre del ícono a utilizar (usando la librería de íconos de `shiny` o `font-awesome`).
#' @param col_fondo Un carácter que representa el color de fondo de la caja (por defecto: `"#FDFEFE"`).
#' @param alto Un número que representa la altura mínima de la caja en píxeles (por defecto: `120`).
#' @param col_letra Un carácter que representa el color del texto (por defecto: `"#17202A"`).
#' @param col_icono Un carácter que representa el color del ícono (por defecto: `"#000000"`).
#' @return Un objeto HTML que contiene la caja con el ícono y el texto estilizados.
#' @import shiny
#' @import colorspace
#' @examples
#' CajaIco("Este es un texto", "info-circle", col_fondo = "#E0E0E0", col_letra = "#333333")
#' @export
CajaIco <- function(texto, icono, col_fondo = "#FDFEFE", alto = 120, col_letra = "#17202A", col_icono = "#000000") {

  # Definición de estilos para la caja
  s_caj <- paste0(
    "display: block; background:", col_fondo, "; min-height:", alto,
    "px; inline-size: 100%; overflow-wrap: break-word; border-radius: 10px;",
    "box-shadow: 1px 1px 2px ", darken(col_fondo, 0.1), ";"
  )

  # Definición de estilos para el ícono
  s_ico <- paste0(
    "position: absolute; text-align: left; font-size: 80px; color:",
    adjust_transparency(col_icono, 0.05),
    "; background: transparent; z-index: 1"
  )

  # Definición de estilos para el contenedor del texto
  s_con <- paste0(
    "inline-size: 50%; overflow-wrap: break-word; hyphens: manual; position: absolute;",
    "z-index: 3; margin: 0px; padding: 5px 10px; margin-top: 20px; margin-left: 10px;",
    "background: transparent;"
  )

  # Definición de estilos para el texto
  s_tex <- paste0(
    "color:", col_letra, "; text-align: left; vertical-align: text-top; font-size: 18px;"
  )

  # Generación de la caja con ícono y texto
  column(12,
         div(class = "row", style = s_caj,
             column(6, style = s_ico, icon(icono)),  # Añadiendo el ícono
             column(6, style = s_con, tags$p(style = s_tex, texto))  # Añadiendo el texto
         )
  )
}
