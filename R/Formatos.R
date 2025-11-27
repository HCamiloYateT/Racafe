#' Definir formato numérico
#'
#' @description Crea un formateador de números según el estilo especificado.
#' Los formatos disponibles son: "coma" (sin decimales), "numero" (dos decimales),
#' "dinero" (símbolo "$" y sin decimales), "dolares" (dos decimales),
#' "miles" (en miles con "$"), "porcentaje" (como porcentaje con dos decimales),
#' "cientifico" (notación científica), "millones" (en millones con "$"),
#' "entero" (sin decimales), "tiempo" (unidades de tiempo), "kwh" (kilovatios-hora)
#' y "log" (escala logarítmica).
#'
#' @details Puede ajustar los parámetros de las funciones de `scales` mediante
#'   argumentos adicionales en `...`. Por ejemplo, es posible modificar la
#'   precisión (`accuracy`), el separador de miles (`big.mark`) o los sufijos y
#'   prefijos sin necesidad de crear nuevos formatos.
#'
#' @param formato Cadena de texto (no sensible a mayúsculas/minúsculas) con el
#'   formato: "coma", "numero", "dinero", "dolares", "miles",
#'   "porcentaje", "cientifico", "millones", "entero", "tiempo", "kwh" o
#'   "log".
#' @param ... Argumentos adicionales que se pasan a la función etiquetadora de
#'   `scales`, permitiendo personalizar aspectos como el número de decimales,
#'   el separador de miles, prefijos, sufijos, etc.
#'
#' @return Una función que formatea un vector numérico.
#'
#' @examples
#' DefinirFormato("coma")(1234567.89)
#' DefinirFormato("dinero")(1234567.89)
#'
#' @importFrom scales label_number label_scientific label_time label_log
#' @export
DefinirFormato <- function(formato, ...) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato válido.")
  }

  formato <- tolower(trimws(as.character(formato[1])))

  generadores <- list(
    coma = function(...) scales::label_number(accuracy = 1, big.mark = ",", ...),
    numero = function(...) scales::label_number(accuracy = 0.01, big.mark = ",", ...),
    dinero = function(...) scales::label_number(accuracy = 1, prefix = "$", big.mark = ",", ...),
    dolares = function(...) scales::label_number(accuracy = 0.01, prefix = "$", big.mark = ",", ...),
    miles = function(...) scales::label_number(accuracy = 0.01, scale = 1e-3, prefix = "$", big.mark = ",", ...),
    porcentaje = function(...) scales::label_number(accuracy = 0.01, scale = 100, suffix = "%", big.mark = ",", ...),
    cientifico = function(...) scales::label_scientific(...),
    millones = function(...) scales::label_number(accuracy = 0.01, scale = 1e-6, prefix = "$", suffix = " M", big.mark = ",", ...),
    entero = function(...) scales::label_number(accuracy = 1, big.mark = ",", ...),
    tiempo = function(...) scales::label_time(...),
    kwh = function(...) scales::label_number(accuracy = 0.01, suffix = " kWh", big.mark = ",", ...),
    log = function(...) scales::label_log(...)
  )

  if (!formato %in% names(generadores)) {
    stop(
      "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', 'miles', ",
      "'porcentaje', 'cientifico', 'millones', 'entero', 'tiempo', 'kwh' o 'log'."
    )
  }

  generadores[[formato]](...)
}

#' Definir formato para D3.js
#'
#' @description Genera un string de formato compatible con la librería D3.js.
#'
#' @param formato Cadena de texto con el formato deseado. Valores permitidos:
#'   "coma", "numero", "dinero", "dolares", "miles", "porcentaje",
#'   "cientifico", "millones", "entero", "tiempo", "kwh" o "log".
#'
#' @return Un string que representa el formato en D3.js.
#'
#' @examples
#' FormatoD3("coma")
#' FormatoD3("dinero")
#'
#' @export
FormatoD3 <- function(formato) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato válido.")
  }

  formato <- tolower(trimws(as.character(formato[1])))

  formatos <- c(
    coma = ",.0f",
    numero = ",.2f",
    dinero = "$,.0f",
    dolares = "$,.2f",
    miles = "$,.2~s",
    porcentaje = ",.2%",
    cientifico = ".2e",
    millones = "$,.2~s",
    entero = ",.0f",
    tiempo = "%H:%M:%S",
    kwh = ",.2f",
    log = ".2e"
  )

  if (!formato %in% names(formatos)) {
    stop(
      "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', 'miles', ",
      "'porcentaje', 'cientifico', 'millones', 'entero', 'tiempo', 'kwh' o 'log'."
    )
  }

  formatos[[formato]]
}

#' Definir formato para JavaScript
#'
#' @description Genera una función en formato string para aplicar en JavaScript.
#'
#' @param formato Cadena de texto con el formato deseado. Valores permitidos:
#'   "coma", "numero", "dinero", "dolares", "miles", "porcentaje",
#'   "cientifico", "millones", "entero", "tiempo", "kwh" o "log".
#'
#' @return Un string con una función en JavaScript que formatea números.
#'
#' @examples
#' FormatoJS("coma")
#' FormatoJS("dinero")
#'
#' @export
FormatoJS <- function(formato) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato válido.")
  }

  formato <- tolower(trimws(as.character(formato[1])))

  formatos <- list(
    coma = 'function(d){return Number(d).toLocaleString(undefined,{minimumFractionDigits:0,maximumFractionDigits:0});}',
    numero = 'function(d){return Number(d).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2});}',
    dinero = 'function(d){return "$" + Number(d).toLocaleString(undefined,{minimumFractionDigits:0,maximumFractionDigits:0});}',
    dolares = 'function(d){return "$" + Number(d).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2});}',
    miles = 'function(d){return "$" + (Number(d)/1000).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2});}',
    porcentaje = 'function(d){return (Number(d)*100).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2}) + "%";}',
    cientifico = 'function(d){return Number(d).toExponential(2);}',
    millones = 'function(d){return "$" + (Number(d)/1000000).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2}) + " M";}',
    entero = 'function(d){return Number(d).toLocaleString(undefined,{minimumFractionDigits:0,maximumFractionDigits:0});}',
    tiempo = 'function(d){var total=Math.max(0,Math.round(Number(d)));var horas=Math.floor(total/3600);var minutos=Math.floor((total%3600)/60);var segundos=total%60;return [horas,minutos,segundos].map(function(v){return String(v).padStart(2,"0");}).join(":");}',
    kwh = 'function(d){return Number(d).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2}) + " kWh";}',
    log = 'function(d){return Number(d).toExponential(2);}'
  )

  if (!formato %in% names(formatos)) {
    stop(
      "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', 'miles', ",
      "'porcentaje', 'cientifico', 'millones', 'entero', 'tiempo', 'kwh' o 'log'."
    )
  }

  formatos[[formato]]
}

#' Definir formato para Handsontable
#'
#' @description Devuelve un string con el formato numérico para usar en Handsontable.
#'
#' @param formato Cadena de texto con el formato deseado. Valores permitidos:
#'   "coma", "numero", "dinero", "dolares", "miles", "porcentaje",
#'   "cientifico", "millones", "entero", "tiempo", "kwh" o "log".
#'
#' @return Un string con el formato de Handsontable.
#'
#' @examples
#' FormatoHOT("coma")
#' FormatoHOT("dinero")
#'
#' @export
FormatoHOT <- function(formato) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato válido.")
  }

  formato <- tolower(trimws(as.character(formato[1])))

  formatos <- c(
    coma = "0,0",
    numero = "0,0.00",
    dinero = "$0,0",
    dolares = "$0,0.00",
    miles = "$0,0.00a",
    porcentaje = "0.00%",
    cientifico = "0.00e+0",
    millones = "$0,0.00a",
    entero = "0,0",
    tiempo = "00:00:00",
    kwh = '0,0.00 "kWh"',
    log = "0.00e+0"
  )

  if (!formato %in% names(formatos)) {
    stop(
      "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', 'miles', ",
      "'porcentaje', 'cientifico', 'millones', 'entero', 'tiempo', 'kwh' o 'log'."
    )
  }

  formatos[[formato]]
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

#' Crear una tabla `gt` para mensajes informativos
#'
#' @description
#' Genera una tabla simple con formato minimalista para mostrar mensajes cuando
#' no hay datos disponibles. Permite personalizar el texto mostrado y conserva
#' un diseño limpio mediante la función [gt_minimal_style()].
#'
#' @param mensaje Cadena de texto con el mensaje a mostrar. Debe ser una
#'   cadena de longitud 1 y no puede ser `NA`.
#'
#' @return Un objeto de clase `gt_tbl` con el mensaje centrado y estilos
#'   minimalistas.
#'
#' @examples
#' library(gt)
#' gt_mensaje_vacio()
#' gt_mensaje_vacio("Sin resultados para los filtros seleccionados")
#'
#' @export
gt_mensaje_vacio <- function(mensaje = "No existen datos en la tabla") {
  if (!is.character(mensaje) || length(mensaje) != 1 || is.na(mensaje)) {
    rlang::abort("`mensaje` debe ser una cadena de texto única y no nula.")
  }

  data.frame(texto = mensaje, stringsAsFactors = FALSE) |>
    gt::gt() |>
    gt::cols_label(texto = "") |>
    gt::tab_options(
      column_labels.hidden = TRUE,
      heading.title.font.size = 0,
      heading.subtitle.font.size = 0,
      table.font.size = 14,
      data_row.padding = gt::px(10)
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "normal", align = "center"),
      locations = gt::cells_body(columns = tidyselect::everything())
    ) |>
    gt_minimal_style()
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
