#' Definir formato numérico
#'
#' @description Crea un formateador de números según el estilo especificado.
#' Los formatos disponibles son: "coma" (sin decimales), "numero" (dos decimales),
#' "dinero" (símbolo "$" y dos decimales), "miles" (en miles con "$"),
#' "porcentaje" (como porcentaje con dos decimales).
#'
#' @param formato Cadena de texto con el formato: "coma", "numero", "dinero",
#'   "miles" o "porcentaje".
#'
#' @return Una función que formatea un vector numérico.
#'
#' @examples
#' DefinirFormato("coma")(1234567.89)
#' DefinirFormato("dinero")(1234567.89)
#'
#' @importFrom scales label_number
#' @export
DefinirFormato <- function(formato) {
  if (formato == "coma") {
    scales::label_number(accuracy = 1, big.mark = ",")
  } else if (formato == "numero") {
    scales::label_number(accuracy = 0.01, big.mark = ",")
  } else if (formato == "dinero") {
    scales::label_number(accuracy = 0.01, prefix = "$", big.mark = ",")
  } else if (formato == "miles") {
    scales::label_number(accuracy = 0.01, scale = 1/1000, prefix = "$", big.mark = ",")
  } else if (formato == "porcentaje") {
    scales::label_number(accuracy = 0.01, scale = 100, suffix = "%", big.mark = ",")
  } else {
    stop("Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'miles' o 'porcentaje'.")
  }
}

#' Definir formato para D3.js
#'
#' @description Genera un string de formato compatible con la librería D3.js.
#'
#' @param formato Cadena de texto: "coma", "numero", "dinero" o "porcentaje".
#'
#' @return Un string que representa el formato en D3.js.
#'
#' @examples
#' FormatoD3("coma")
#' FormatoD3("dinero")
#'
#' @export
FormatoD3 <- function(formato) {
  if (formato == "coma") {
    ",.0f"
  } else if (formato == "numero") {
    ",.2f"
  } else if (formato == "dinero") {
    "$,.2f"
  } else if (formato == "porcentaje") {
    ",.2%"
  } else {
    stop("Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
}

#' Definir formato para JavaScript
#'
#' @description Genera una función en formato string para aplicar en JavaScript.
#'
#' @param formato Cadena de texto: "coma", "numero", "dinero" o "porcentaje".
#'
#' @return Un string con una función en JavaScript que formatea números.
#'
#' @examples
#' FormatoJS("coma")
#' FormatoJS("dinero")
#'
#' @export
FormatoJS <- function(formato) {
  if (formato == "coma") {
    'function(d){return d.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  } else if (formato == "numero") {
    'function(d){return d.toFixed(2);}'
  } else if (formato == "dinero") {
    'function(d){return "$" + d.toFixed(2).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  } else if (formato == "porcentaje") {
    'function(d){return (d*100).toFixed(1) + "%";}'
  } else {
    stop("Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
}

#' Definir formato para Handsontable
#'
#' @description Devuelve un string con el formato numérico para usar en Handsontable.
#'
#' @param formato Cadena de texto: "coma", "numero", "dinero" o "porcentaje".
#'
#' @return Un string con el formato de Handsontable.
#'
#' @examples
#' FormatoHOT("coma")
#' FormatoHOT("dinero")
#'
#' @export
FormatoHOT <- function(formato) {
  if (formato == "coma") {
    "0,0"
  } else if (formato == "numero") {
    "0,0.00"
  } else if (formato == "dinero") {
    "$0,0.00"
  } else if (formato == "porcentaje") {
    "0.00%"
  } else {
    stop("Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }
}

#' Formatear número en HTML
#'
#' @description Aplica formato numérico y estilos visuales (color, negrita) en HTML.
#'
#' @param x Número o vector numérico.
#' @param formato Formato numérico (usado en \code{DefinirFormato}).
#' @param negrita Lógico, si el número debe mostrarse en negrita (default: TRUE).
#' @param color Cadena con el color en formato hexadecimal (default: "#000000").
#' @param meta Valor umbral para asignar colores (opcional).
#' @param prop Lógico, si los colores son proporcionales respecto a \code{meta}.
#'
#' @return Texto en HTML con estilos aplicados.
#'
#' @examples
#' FormatearNumero(2500, "dinero", meta = 2000)
#' FormatearNumero(0.75, "porcentaje", negrita = FALSE, color = "#00FF00")
#'
#' @export
FormatearNumero <- function(x, formato, negrita = TRUE, color = "#000000", meta = NA, prop = TRUE) {
  form <- DefinirFormato(formato)

  # Colores basados en meta
  if (!is.na(meta)) {
    pal <- if (prop) {
      colorRampPalette(c("#CB4335", "orange", "#138D75"))
    } else {
      colorRampPalette(c("#138D75", "orange", "#CB4335"))
    }
    colors <- pal(3)
    col <- ifelse(x < meta, colors[1], ifelse(x == meta, colors[2], colors[3]))
  } else {
    col <- color
  }

  # Generar HTML
  if (negrita) {
    htmltools::HTML(paste0("<span style='font-weight:bold;color:", col, "'>", form(x), "</span>"))
  } else {
    htmltools::HTML(paste0("<span style='color:", col, "'>", form(x), "</span>"))
  }
}

#' Formatear texto en HTML
#'
#' @description Aplica opciones de estilo visual a un texto plano.
#'
#' @param x Cadena de texto.
#' @param negrita Lógico, si el texto se muestra en negrita.
#' @param color Cadena con el color en formato hexadecimal.
#' @param tamano_pct Tamaño relativo en porcentaje (1 = 100%).
#' @param alineacion Alineación: "left", "center" o "right".
#' @param transform Transformación del texto: "none", "capitalize", "uppercase", "lowercase".
#'
#' @return Texto en HTML con estilos aplicados.
#'
#' @examples
#' FormatearTexto("Texto", color = "#0000FF", tamano_pct = 1.2)
#' FormatearTexto("Ejemplo", transform = "uppercase", alineacion = "center")
#'
#' @export
FormatearTexto <- function(x, negrita = TRUE, color = "#000000", tamano_pct = 1, alineacion = "left", transform = "none") {
  neg <- paste0("font-weight:", ifelse(negrita, "bold", "normal"), ";")
  col <- paste0("color:", color, ";")
  tam <- paste0("font-size:", tamano_pct * 100, "%;")
  ali <- paste0("text-align:", alineacion, ";")
  tra <- paste0("text-transform:", transform, ";")

  htmltools::HTML(paste0("<span style='", neg, col, tam, ali, tra, "'>", x, "</span>"))
}

#' Estilo minimalista para tablas gt
#'
#' @description Aplica un diseño limpio y minimalista a un objeto creado con
#' el paquete **gt**, eliminando bordes y resaltando encabezados con una línea sutil.
#'
#' @param gt_table Objeto de clase `gt_tbl` generado con el paquete `gt`.
#'
#' @return Un objeto `gt_tbl` con estilos personalizados.
#'
#' @examples
#' library(gt)
#' gt(head(mtcars)) %>% gt_minimal_style()
#'
#' @export
gt_minimal_style <- function(gt_table) {
  gt_table |>
    gt::tab_options(
      table.width = gt::pct(100),            # Ancho total al 100%
      table.font.size = 12,              # Tamaño de fuente
      data_row.padding = gt::px(3),          # Espaciado en filas de datos
      summary_row.padding = gt::px(3),       # Espaciado en filas de resumen
      grand_summary_row.padding = gt::px(3), # Espaciado en filas de gran resumen
      footnotes.padding = gt::px(3),         # Espaciado en notas al pie
      source_notes.padding = gt::px(3),      # Espaciado en notas de fuente
      row_group.padding = gt::px(3),         # Espaciado en grupos de filas

      # Eliminación de bordes por defecto
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.left.style = "none",
      table.border.right.style = "none",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "none",

      # Colores base: blanco para encabezado y fondo
      column_labels.background.color = "white",
      heading.background.color = "white",

      # Negrita en encabezados de columnas
      column_labels.font.weight = "bold"
    ) |>
    # CSS personalizado para controlar bordes y estilos más allá de tab_options
    gt::opt_css(css = "
      .gt_table {
        border-collapse: collapse !important;
        border: none !important;
        background-color: white !important;
      }
      .gt_table td {
        border: none !important;
      }
      .gt_col_heading {
        font-weight: bold !important;
        background-color: white !important;
        border-top: none !important;
        border-left: none !important;
        border-right: none !important;
        border-bottom: 2px solid #00000033 !important;
      }
    ")
}

#' Color para KPI
#'
#' @description Asigna color según si el valor es positivo, negativo o cero, y según proporcionalidad.
#'
#' @param x Valor numérico.
#' @param prop Lógico, indica si el KPI es proporcional (default: TRUE).
#'
#' @return Cadena con un color en formato hexadecimal.
#'
#' @examples
#' col_kpi(1, TRUE)
#' col_kpi(-1, FALSE)
#'
#' @export
col_kpi <- function(x, prop = TRUE) {
  dplyr::case_when(
    x == 0 ~ "#000000",
    prop & x > 0 ~ "#0B5345",
    prop & x < 0 ~ "#943126",
    !prop & x > 0 ~ "#943126",
    !prop & x < 0 ~ "#0B5345"
  )
}

#' Símbolo para KPI
#'
#' @description Devuelve un símbolo gráfico que representa la dirección de un indicador.
#'
#' @param x Valor numérico.
#'
#' @return Un carácter: ▲ (positivo), ▼ (negativo), ▬ (cero).
#'
#' @examples
#' chr_kpi(1)
#' chr_kpi(-1)
#'
#' @export
chr_kpi <- function(x) {
  dplyr::case_when(
    x == 0 ~ "▬",
    x > 0 ~ "▲",
    x < 0 ~ "▼"
  )
}

#' Color según número
#'
#' @description Asigna color a un número: negro si es mayor o igual a cero y rojo oscuro si es negativo.
#'
#' @param x Valor numérico o vector.
#'
#' @return Vector de colores en formato hexadecimal.
#'
#' @examples
#' col_num(5)
#' col_num(-3)
#' col_num(c(5, -3, 0))
#'
#' @export
col_num <- function(x) {
  dplyr::case_when(
    x >= 0 ~ "#000000",
    x < 0 ~ "#943126"
  )
}
