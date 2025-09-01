#' Definir Formato para Números
#'
#' Esta función asigna un formato específico para los números en función de un parámetro de formato. Los formatos disponibles son:
#' "coma" (números con comas como separadores de miles), "numero" (números con dos decimales),
#' "dinero" (números con símbolo de dólar y comas como separadores de miles), "miles" (números en miles con símbolo de dólar),
#' y "porcentaje" (números expresados como porcentaje con dos decimales).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles.
#'   - "miles": Números en miles con símbolo "$" y comas como separadores de miles.
#'   - "porcentaje": Números expresados como porcentaje (multiplicados por 100 y con "%" al final).
#' @return Un objeto de tipo función que formatea los números según el formato especificado.
#'
#' @examples
#' # Formato de número con comas
#' DefinirFormato("coma")(1234567.89)
#'
#' # Formato de dinero
#' DefinirFormato("dinero")(1234567.89)
#'
#' @import scales
#' @export
DefinirFormato <- function(formato) {

  require(scales)

  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    label_number(accuracy = 1, scale = 1, suffix = "", prefix = "", big.mark = ",")
  } else if (formato == "numero") {
    label_number(accuracy = 0.01, scale = 1, suffix = "", prefix = "", big.mark = ",")
  } else if (formato == "dinero") {
    label_number(accuracy = 1, scale = 1, suffix = "", prefix = "$", big.mark = ",")
  } else if (formato == "miles") {
    label_number(accuracy = 1, scale = 1 / 1000, suffix = "", prefix = "$", big.mark = ",")
  } else if (formato == "porcentaje") {
    label_number(accuracy = 0.01, scale = 100, suffix = "%", prefix = "", big.mark = ",")
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'miles', 'porcentaje'.")
  }

  # Retornar el formato correspondiente
  return(formato)
}

#' Definir Formato de Visualización de Números en D3.js
#'
#' Esta función define el formato de visualización de números en una gráfica utilizando la biblioteca D3.js.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles),
#' "numero" (números con dos decimales), "dinero" (números con símbolo de dólar y comas como separadores de miles),
#' y "porcentaje" (números expresados como porcentaje con dos decimales).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles, sin decimales.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles, sin decimales.
#'   - "porcentaje": Números expresados como porcentaje con dos decimales.
#' @return Un string que representa el formato para visualizar los números en una gráfica utilizando D3.js.
#'
#' @examples
#' # Formato con comas
#' FormatoD3("coma")
#'
#' # Formato de dinero
#' FormatoD3("dinero")
#'
#' @export
FormatoD3 <- function(formato) {

  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    ",.0f"   # Números con comas y sin decimales
  } else if (formato == "numero") {
    ",.2f"   # Números con dos decimales
  } else if (formato == "dinero") {
    "$,.0f"  # Números con símbolo de dólar, con comas como separadores de miles y sin decimales
  } else if (formato == "porcentaje") {
    ",.2%"   # Números expresados como porcentaje con dos decimales
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }

  # Retornar el formato correspondiente
  return(formato)
}

#' Definir Formato de Visualización de Números en JavaScript
#'
#' Esta función define el formato de visualización de números para ser usado en JavaScript.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles),
#' "numero" (números con dos decimales), "dinero" (números con símbolo de dólar y separadores de miles, sin decimales),
#' y "porcentaje" (números expresados como porcentaje con un decimal).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles y sin decimales.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$", comas como separadores de miles y sin decimales.
#'   - "porcentaje": Números expresados como porcentaje con un decimal.
#' @return Un string que representa el formato correspondiente para usar en JavaScript para la visualización de números.
#'
#' @examples
#' # Formato con comas
#' FormatoJS("coma")
#'
#' # Formato de dinero
#' FormatoJS("dinero")
#'
#' @export
FormatoJS <- function(formato) {

  # Asignar el formato adecuado según la opción especificada
  formato <- if (formato == "coma") {
    'function(d){return d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'  # Números con comas y sin decimales
  } else if (formato == "numero") {
    'function(d){return d.toFixed(2)}'  # Números con dos decimales
  } else if (formato == "dinero") {
    'function(d){return "$" + d.toFixed(0).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'  # Números con símbolo de dólar y separadores de miles
  } else if (formato == "porcentaje") {
    'function(d){return (d*100).toFixed(1) + "%"}'  # Números expresados como porcentaje con un decimal
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }

  # Retornar el formato correspondiente
  return(formato)
}

#' Definir el Formato de Números para Handsontable (HOT)
#'
#' Esta función asigna un formato de número adecuado para ser usado en Handsontable, una biblioteca JavaScript.
#' Los formatos disponibles son: "coma" (números con comas como separadores de miles), "numero" (números con dos decimales),
#' "dinero" (números con símbolo de dólar y separadores de miles), y "porcentaje" (números expresados como porcentaje).
#'
#' @param formato Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
#'   - "coma": Números con comas como separadores de miles.
#'   - "numero": Números con dos decimales.
#'   - "dinero": Números con símbolo "$" y comas como separadores de miles.
#'   - "porcentaje": Números expresados como porcentaje con dos decimales.
#' @return Un string que representa el formato correspondiente para usar en Handsontable (HOT).
#'
#' @examples
#' # Formato con comas
#' FormatoHOT("coma")
#'
#' # Formato de dinero
#' FormatoHOT("dinero")
#'
#' @export
FormatoHOT <- function(formato) {

  # Asignar el formato adecuado según el tipo especificado
  formato <- if (formato == "coma") {
    "0,0"  # Números con comas como separadores de miles
  } else if (formato == "numero") {
    "0,0.00"  # Números con dos decimales
  } else if (formato == "dinero") {
    "$0,0.00"  # Números con símbolo de dólar y separadores de miles
  } else if (formato == "porcentaje") {
    "%0,0.00"  # Números expresados como porcentaje con dos decimales
  } else {
    stop("Formato no reconocido. Los formatos disponibles son: 'coma', 'numero', 'dinero', 'porcentaje'.")
  }

  # Retornar el formato correspondiente
  return(formato)
}


#' Formatear un Número con Opciones de Estilo y Colores
#'
#' Esta función permite formatear un número de acuerdo con un formato específico,
#' con opciones para aplicar negrita y cambiar el color en función de un umbral
#' o meta. Se puede utilizar para resaltar visualmente los números en gráficos o tablas.
#'
#' @param x El número o vector de números a formatear.
#' @param formato El formato deseado para el número. Opciones incluyen:
#'   "coma" (con separadores de miles), "numero", "dinero", "miles", "porcentaje".
#' @param negrita Un valor lógico (`TRUE` o `FALSE`) que indica si el número debe aparecer en negrita (por defecto es `TRUE`).
#' @param color El color en formato hexadecimal (por defecto es "#000000"). Si no se proporciona `meta`, este color será usado.
#' @param meta Un valor o vector numérico que representa el umbral o meta para cambiar el color. (opcional)
#' @param prop Un valor lógico (`TRUE` o `FALSE`) que indica si se debe usar una escala de colores proporcional al valor de `x` con respecto a `meta` (por defecto es `TRUE`).
#' @return Un número formateado en HTML con opciones de estilo (negrita, color) y el formato numérico especificado.
#'
#' @examples
#' # Formatear un número en formato "dinero" con color proporcional
#' FormatearNumero(2500, "dinero", negrita = TRUE, meta = 2000)
#'
#' # Formatear un número en formato "porcentaje" sin negrita y con color específico
#' FormatearNumero(0.75, "porcentaje", negrita = FALSE, color = "#00FF00")
#'
#' @import RColorBrewer
#' @import tidyverse
#' @import shiny
#' @export
FormatearNumero <- function(x, formato, negrita = TRUE, color = "#000000", meta = NA, prop = TRUE) {

  # Definir formato numérico usando la función DefinirFormato
  form <- DefinirFormato(formato)

  # Definir un rango de meta para la aplicación de colores
  meta2 <- c(-Inf, meta, Inf)

  # Crear una paleta de colores basada en el valor de 'prop' (proporcional o no)
  pal <- ifelse(prop,
                colorRampPalette(c("#CB4335", "orange", "#138D75")),
                colorRampPalette(c("#138D75", "orange", "#CB4335"))
  )

  # Establecer el número de colores de la paleta
  n <- length(meta) + 1
  colors <- pal(n)

  # Determinar el color basado en la relación del número con la meta
  col <- ifelse(is.null(meta), color, colors[sum(!x < meta2)])

  # Formatear el número en HTML con el color y negrita si corresponde
  res <- ifelse(negrita,
                paste0("<span style='font-weight: bold;color:", col, "'>", form(x), "</span>") %>% HTML,
                paste0("<span style='color:", col, "'>", form(x), "</span>") %>% HTML)

  return(res)
}

#' Formatear Texto con Opciones de Estilo
#'
#' Esta función permite formatear un texto con diversas opciones de estilo, como
#' resaltar en negrita, cambiar el color, ajustar el tamaño de la fuente, alinear el texto
#' y transformar el texto a mayúsculas, minúsculas o capitalización de la primera letra.
#'
#' @param x El texto a formatear.
#' @param negrita Un valor lógico (`TRUE` o `FALSE`) que indica si el texto debe aparecer en negrita (por defecto es `TRUE`).
#' @param color El color en formato hexadecimal (por defecto es "#000000").
#' @param tamano_pct El tamaño de la fuente en porcentaje (por defecto es 1, es decir, tamaño normal).
#' @param alineacion La alineación del texto. Puede ser "left", "center" o "right" (por defecto es "left").
#' @param transform La transformación del texto. Opciones: "none", "capitalize", "uppercase", "lowercase" (por defecto es "capitalize").
#'
#' @return El texto formateado en HTML con las opciones seleccionadas.
#'
#' @examples
#' # Formatear texto en negrita, color azul y tamaño 120%
#' FormatearTexto("Texto formateado", negrita = TRUE, color = "#0000FF", tamano_pct = 1.2)
#'
#' # Texto en mayúsculas y alineado al centro
#' FormatearTexto("Texto en mayúsculas", transform = "uppercase", alineacion = "center")
#'
#' @import shiny
#' @export
FormatearTexto <- function(x, negrita = TRUE, color = "#000000", tamano_pct = 1, alineacion = "left", transform = "none") {

  # Establecer el estilo de negrita
  neg <- paste0("font-weight:", ifelse(negrita, "bold", "normal"), ";")

  # Establecer el color del texto
  col <- paste0("color:", color, ";")

  # Establecer el tamaño de la fuente en porcentaje
  tam <- paste0("font-size:", tamano_pct * 100, "%;")

  # Establecer la alineación del texto
  ali <- paste0("text-align:", alineacion, ";")

  # Establecer la transformación del texto (mayúsculas, minúsculas, etc.)
  tra <- paste0("text-transform:", transform, ";")

  # Generar el HTML con las opciones de formato
  res <- paste0("<span style='", neg, col, tam, ali, tra, "'>", x, "</span>") %>% HTML

  # Retornar el texto formateado
  return(res)
}

#' @title Estilo minimalista para tablas gt
#' @description
#' Aplica un tema visual limpio y minimalista a un objeto de tabla creado con
#' el paquete **gt**, eliminando casi todos los bordes y resaltando únicamente
#' los encabezados de columna con una línea sutil.
#'
#' @param gt_table Un objeto de clase `gt_tbl` creado previamente con el paquete `gt`.
#'
#' @return Un objeto `gt_tbl` con estilos de tabla personalizados.
#'
#' @examples
#' library(gt)
#' library(dplyr)
#'
#' # Ejemplo básico
#' df <- head(mtcars)
#' tab <- gt(df)
#' gt_minimal_style(tab)
#'
gt_minimal_style <- function(gt_table) {
  require(gt)

  gt_table %>%
    # Opciones generales de la tabla
    tab_options(
      table.width = pct(100),            # Ancho total al 100%
      table.font.size = 12,              # Tamaño de fuente
      data_row.padding = px(3),          # Espaciado en filas de datos
      summary_row.padding = px(3),       # Espaciado en filas de resumen
      grand_summary_row.padding = px(3), # Espaciado en filas de gran resumen
      footnotes.padding = px(3),         # Espaciado en notas al pie
      source_notes.padding = px(3),      # Espaciado en notas de fuente
      row_group.padding = px(3),         # Espaciado en grupos de filas

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
    ) %>%
    # CSS personalizado para controlar bordes y estilos más allá de tab_options
    opt_css(css = "
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
        border-bottom: 2px solid #00000033 !important; # línea fina gris bajo encabezado
      }
    ")
}

#' Obtener el color para un indicador
#'
#' Esta función devuelve el color correspondiente para un indicador, según si es proporcional o no.
#'
#' @param x El valor del indicador.
#' @param prop Un valor lógico que indica si el indicador es proporcional (TRUE) o no (FALSE).
#'            El valor predeterminado es TRUE.
#' @return El color correspondiente como una cadena hexadecimal.
#' @examples
#' col_kpi(1, TRUE)  # Indicador proporcional positivo (color verde).
#' col_kpi(-1, FALSE)  # Indicador no proporcional negativo (color rojo).
#' @export

col_kpi <- function(x, prop = TRUE){
  # Definir los colores para los casos proporcionales y no proporcionales
  case_when(
    x == 0 ~ "#000000",  # Color negro si el valor es 0
    prop & x > 0 ~ "#0B5345",  # Verde si es proporcional y positivo
    prop & x < 0 ~ "#943126",  # Rojo si es proporcional y negativo
    !prop & x > 0 ~ "#943126",  # Rojo si no es proporcional y positivo
    !prop & x < 0 ~ "#0B5345"  # Verde si no es proporcional y negativo
  )
}

#' Obtener un carácter que represente la dirección de un indicador
#'
#' Esta función devuelve un carácter que representa el crecimiento o decrecimiento de un indicador numérico.
#'
#' @param x El número del cual se desea obtener el carácter representativo.
#' @return Un carácter: '▲' para valores positivos, '▼' para negativos y '▬' para valores iguales a 0.
#' @examples
#' chr_kpi(1)  # Devuelve "▲" para crecimiento positivo.
#' chr_kpi(-1)  # Devuelve "▼" para decrecimiento negativo.
#' @export
chr_kpi <- function(x){
  case_when(
    x == 0 ~ "▬",  # Neutral si es 0
    x > 0 ~ "▲",   # Flecha hacia arriba si es positivo
    x < 0 ~ "▼"    # Flecha hacia abajo si es negativo
  )
}

#' Asignar color basado en valor numérico
#'
#' Esta función asigna un color a un número dependiendo de si es positivo, cero o negativo.
#'
#' @param x Un valor numérico o vector de valores numéricos.
#' @return Un carácter que representa el color en formato hexadecimal: `#000000` para valores mayores o iguales a cero y `#943126` para valores negativos.
#' @examples
#' col_num(5)   # Devuelve "#000000"
#' col_num(-3)  # Devuelve "#943126"
#' col_num(c(5, -3, 0))  # Devuelve un vector con colores correspondientes a cada valor
#' @export
col_num <- function(x){

  # Asigna color basado en si el valor es mayor o igual a cero, o negativo
  col <- case_when(
    x >= 0 ~ "#000000",   # Color negro para valores positivos o cero
    x < 0 ~ "#943126"     # Color rojo oscuro para valores negativos
  )

  return(col)
}
