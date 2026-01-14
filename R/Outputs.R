#' Crear un botón de descarga con estilo personalizado
#'
#' Genera un botón de descarga con estilos corporativos y opciones de configuración para
#' el ícono, el color y el tamaño. La función valida exhaustivamente los argumentos para
#' entregar mensajes de error claros y puede integrarse fácilmente con módulos de `shiny`
#' mediante el uso de un namespace.
#'
#' @param button_id Identificador del botón de descarga. Debe ser una cadena no vacía.
#' @param icon_name Nombre del ícono a utilizar en el botón. Debe ser una cadena no vacía.
#' @param color Color del texto y borde del botón. Debe ser un color válido reconocido por R.
#' @param ns Función de namespace (generalmente `shiny::NS`) utilizada en módulos. Por defecto `NULL`.
#' @param title Texto que se mostrará como tooltip al pasar el cursor sobre el botón. Debe ser una cadena.
#' @param size Tamaño del botón. Debe ser uno de `"sm"`, `"md"` o `"lg"`. Por defecto `"sm"`.
#' @param align Alineación horizontal del botón dentro del contenedor. Debe ser `"left"`,
#'   `"center"` o `"right"`. Por defecto `"right"`.
#'
#' @return Un objeto HTML que representa el botón de descarga personalizado.
#'
#' @examples
#' BotonDescarga("descargar_reporte")
#' BotonDescarga("descargar_reporte", icon_name = "download", size = "md")
#' BotonDescarga("descargar", color = "steelblue", title = "Descargar informe")
#'
#' @references
#' Font Awesome Icons. 
#' \url{https://fontawesome.com/v4/icons/}
#'
#' @importFrom shiny span downloadButton icon
#' @export
BotonDescarga <- function(button_id, icon_name = "file-excel", color = "#28b78d", ns = NULL,
                          title = "Descargar", size = "sm", align = "right") {

  if (!is.character(button_id) || length(button_id) != 1 || !nzchar(button_id)) {
    stop("'button_id' debe ser una cadena de caracteres no vacía.", call. = FALSE)
  }

  if (!is.character(icon_name) || length(icon_name) != 1 || !nzchar(icon_name)) {
    stop("'icon_name' debe ser una cadena de caracteres no vacía.", call. = FALSE)
  }

  if (!is.character(color) || length(color) != 1 || !nzchar(color)) {
    stop("'color' debe ser una cadena de caracteres no vacía.", call. = FALSE)
  }

  color_is_valid <- TRUE
  tryCatch(grDevices::col2rgb(color), error = function(...) color_is_valid <<- FALSE)
  if (!color_is_valid) {
    stop("'color' debe ser un color reconocido por R (ej. nombre o código hexadecimal).", call. = FALSE)
  }

  if (!is.null(ns) && !is.function(ns)) {
    stop("'ns' debe ser NULL o una función de namespace válida (p. ej. shiny::NS).", call. = FALSE)
  }

  if (!is.character(title) || length(title) != 1) {
    stop("'title' debe ser una cadena de caracteres de longitud uno.", call. = FALSE)
  }

  size_config <- list(
    sm = list(padding = "3px 8px", font_size = "12px"),
    md = list(padding = "6px 12px", font_size = "14px"),
    lg = list(padding = "8px 16px", font_size = "16px")
  )

  if (!is.character(size) || length(size) != 1 || !size %in% names(size_config)) {
    stop("'size' debe ser uno de los valores permitidos: 'sm', 'md' o 'lg'.", call. = FALSE)
  }

  align_options <- c(left = "flex-start", center = "center", right = "flex-end")
  if (!is.character(align) || length(align) != 1 || !align %in% names(align_options)) {
    stop("'align' debe ser uno de los valores permitidos: 'left', 'center' o 'right'.",
         call. = FALSE)
  }

  final_id <- if (is.null(ns)) button_id else ns(button_id)

  current_size <- size_config[[size]]

  button_style <- paste0(
    "-webkit-text-size-adjust: 100%; -webkit-tap-highlight-color: transparent; ",
    "word-wrap: break-word; box-sizing: border-box; line-height: inherit; ",
    "text-transform: none; margin: 0; border-width: 1px; font-weight: 400; ",
    "position: relative; overflow: hidden; border: 1px solid ", color, "40; ",
    "border-radius: 4px; background: transparent; ",
    "transition: all 0.3s cubic-bezier(0.02, 0.01, 0.47, 1); ",
    "outline: none; -webkit-appearance: button; ",
    "padding: ", current_size$padding, "; ",
    "font-size: ", current_size$font_size, "; ",
    "font-family: inherit; color: ", color, "; cursor: pointer !important;"
  )

  container_style <- paste0(
    "display: flex; justify-content: ", align_options[[align]], "; ",
    "width: 100%; cursor: pointer;"
  )

  span(
    title = title,
    style = container_style,
    downloadButton(
      outputId = final_id,
      label = NULL,
      icon = icon(icon_name),
      style = button_style
    )
  )
}

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
             column(10, style = s_con, tags$p(style = s_tex, texto))  # Añadiendo el texto
         )
  )
}

#' Crear una caja de valor para indicadores
#'
#' Genera una caja de indicadores basada en `bs4Dash::bs4ValueBox` que aplica formatos
#' numéricos y de texto personalizados, e incluye opcionalmente un botón de "Ver detalle".
#' La función valida los argumentos de entrada para evitar errores comunes en la
#' construcción de interfaces `shiny`.
#'
#' @param valor Número que se mostrará como valor principal.
#' @param formato Cadena de formato utilizada por [FormatearNumero()] (por ejemplo, "dinero" o "porcentaje").
#' @param texto Texto descriptivo del indicador.
#' @param icono Nombre del ícono a usar (Font Awesome) para la caja de valor.
#' @param inputId Identificador del botón de detalle. Requerido cuando `mostrar_boton` es `TRUE`.
#' @param mostrar_boton Lógico que indica si se debe mostrar el botón de detalle. Por defecto `TRUE`.
#'
#' @return Un objeto `bs4ValueBox` listo para incorporarse en una aplicación `shiny`.
#'
#' @examples
#' \dontrun{
#' CajaValor(1500000, "dinero", "Ingresos totales", "chart-line", "detalle_ingresos")
#' CajaValor(0.87, "porcentaje", "Cumplimiento", "thumbs-up", "detalle_cumplimiento", mostrar_boton = FALSE)
#' }
#'
#' @importFrom shiny actionButton icon
#' @importFrom bs4Dash bs4ValueBox
#' @export
CajaValor <- function(valor, formato, texto, icono, inputId = NULL, mostrar_boton = TRUE) {

  if (!is.numeric(valor) || length(valor) != 1 || is.na(valor)) {
    stop("'valor' debe ser un número de longitud uno y no puede ser NA.", call. = FALSE)
  }

  if (!is.character(formato) || length(formato) != 1 || !nzchar(formato)) {
    stop("'formato' debe ser una cadena de caracteres de longitud uno.", call. = FALSE)
  }

  if (!is.character(texto) || length(texto) != 1 || !nzchar(texto)) {
    stop("'texto' debe ser una cadena de caracteres de longitud uno.", call. = FALSE)
  }

  if (!is.character(icono) || length(icono) != 1 || !nzchar(icono)) {
    stop("'icono' debe ser una cadena con el nombre del ícono a utilizar.", call. = FALSE)
  }

  if (!is.logical(mostrar_boton) || length(mostrar_boton) != 1 || is.na(mostrar_boton)) {
    stop("'mostrar_boton' debe ser TRUE o FALSE.", call. = FALSE)
  }

  if (isTRUE(mostrar_boton)) {
    if (is.null(inputId) || !is.character(inputId) || length(inputId) != 1 || !nzchar(inputId)) {
      stop("'inputId' debe ser una cadena de caracteres no vacía cuando se solicita el botón.", call. = FALSE)
    }
  } else {
    inputId <- NULL
  }

  boton_style <- paste(
    "background-color: transparent !important;",
    "background-image: none !important; border: none !important;",
    "box-shadow: none !important; text-decoration: underline !important;",
    "margin-right: auto; margin-left: 0; display: block; font-size: 10px;",
    "cursor: pointer;"
  )

  footer_con_boton <- if (isTRUE(mostrar_boton)) {
    shiny::actionButton(
      inputId,
      "Ver detalle",
      icon = shiny::icon("search"),
      style = boton_style
    )
  } else {
    NULL
  }

  valor_formateado <- FormatearTexto(FormatearNumero(valor, formato = formato), tamano_pct = 2)
  subtitulo <- FormatearTexto(texto, tamano_pct = 1.2)

  bs4Dash::bs4ValueBox(
    value = valor_formateado,
    subtitle = subtitulo,
    icon = shiny::icon(icono),
    color = "white",
    footer = footer_con_boton
  )
}

#' Imprime un diagrama de Sankey con datos de agrupación
#'
#' Esta función crea un diagrama de Sankey con la biblioteca `plotly`, basado en las agrupaciones especificadas
#' en los datos de entrada y permite visualizar relaciones entre variables.
#'
#' @param data Data frame con los datos de entrada para el diagrama de Sankey.
#' @param vars Vector de nombres de columnas a utilizar en el diagrama.
#' @param fun Función de agregación a aplicar, como "n" para contar o funciones estadísticas (e.g., "sum", "mean").
#' @param var Nombre de la variable sobre la cual se aplica `fun` (opcional).
#' @param colores Vector de colores a aplicar a cada variable especificada en `vars`.
#'
#' @return Una lista con el gráfico Sankey (`plot`) y los datos de nodos y enlaces (`nodos`, `arcos`).
#'
#' @examples
#' ImprimeSankey(data = df, vars = c("Var1", "Var2"), fun = "n", colores = c("blue", "green"))
#'
#' @importFrom dplyr group_by summarise mutate select left_join across bind_rows
#' @importFrom plotly plot_ly layout
#' @importFrom plotly event_register config
#' @importFrom scales comma percent
#' @export
ImprimeSankey <- function(data, vars, fun, var = NULL, colores) {

  # Validación de la correspondencia de colores con variables
  if (length(vars) != length(colores)) {
    stop("El número de colores debe coincidir con el número de variables.")
  }

  # Crear tabla de frecuencias para valores únicos de las variables especificadas
  tb <- table(unlist(sapply(vars, function(x) Unicos(data[[x]]))) %>% as.character())
  tb <- tb[tb > 1] %>% unlist() %>% names()

  # Resumir los datos según la función especificada (conteo o agregación sobre `var`)
  aux1 <- if (fun == "n") {
    data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = n(), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA), paste0(cur_column(), .), .)))
  } else {
    data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = !!parse_expr(paste0(fun, "(", var, ", na.rm = TRUE)")), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA), paste0(cur_column(), .), .)))
  }

  # Definir etiquetas de nodos y colores
  vec <- c("Origen", vars)
  nds <- c("TOTAL", unlist(sapply(vars, function(x) Unicos(aux1[[x]]))) %>% as.character())

  # Generar colores personalizados para cada variable
  aux_col <- do.call("bind_rows", lapply(vars, function(x) {
    i <- which(vars == x)
    color_base <- colores[i]
    n_colores <- length(unique(aux1[[x]]))
    data.frame(
      label = Unicos(aux1[[x]]),
      colores = colorRampPalette(c(lighten(color_base, 0.5), color_base, darken(color_base, 0.5)))(n_colores)
    )
  })) %>%
    bind_rows(data.frame(label = "TOTAL", colores = "#000000"))

  # Crear descripciones de texto para cada nodo
  aux_txt <- do.call("bind_rows", lapply(vec, function(x) {
    tot <- sum(aux1$Tot)
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / tot, .groups = 'drop') %>%
      rowwise() %>%
      mutate(txt = paste0("<b>Clientes: </b>", scales::comma(Tot, accuracy = 1),
                          "<br><b>Pct. del Total: </b>", scales::percent(Pct, accuracy = 0.01))) %>%
      select(label = 1, txt)
  }))

  # Crear etiquetas de variable para los nodos
  aux_var <- do.call("bind_rows", lapply(vec, function(x) {
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / sum(aux1$Tot), .groups = 'drop') %>%
      mutate(Var = x) %>%
      select(label = 1, Var)
  }))

  # Data frame de nodos con etiquetas, colores y descripciones
  nodos <- data.frame(label = nds) %>%
    left_join(aux_col, by = "label") %>%
    left_join(aux_txt, by = "label") %>%
    left_join(aux_var, by = "label") %>%
    mutate(label2 = Reduce(function(x, pattern) gsub(pattern, "", x), vars, nds),
           indices = seq_along(nds) - 1,
           texto = paste0("<b>", label2, "</b><br>", txt))

  # Crear enlaces entre nodos
  n <- length(vec) - 1
  aux_lista <- lapply(1:n, function(i) c(vec[i], vec[i + 1]))
  links <- do.call("bind_rows", lapply(aux_lista, function(x) {
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), .groups = 'drop') %>%
      left_join(nodos %>% select(label, indices, VarSource = Var) %>% rename(source = indices), by = setNames("label", x[1])) %>%
      left_join(nodos %>% select(label, indices, VarTarget = Var) %>% rename(target = indices), by = setNames("label", x[2])) %>%
      select(source, target, value = Tot, VarSource, VarTarget)
  }))

  # Calcular valores totales para los enlaces
  total_values <- aggregate(value ~ source, data = links, sum)
  links <- links %>%
    left_join(total_values, by = "source", suffix = c("", "_total")) %>%
    left_join(nodos %>% select(source = indices, Origen = label2), by = "source") %>%
    left_join(nodos %>% select(target = indices, Destino = label2), by = "target") %>%
    mutate(
      PctTot = value / sum(aux1$Tot),
      PctSource = value / value_total,
      texto = paste0("<b>Origen: </b>", Origen,
                     "<br><b>Destino: </b>", Destino,
                     "<br><b>Clientes: </b>", scales::comma(value, accuracy = 1),
                     "<br><b>Pct. del Total: </b>", scales::percent(PctTot, accuracy = 0.01),
                     "<br><b>Pct. del Origen: </b>", scales::percent(PctSource, accuracy = 0.01))
    )

  # Crear el gráfico Sankey con `plotly`
  sankey <- plot_ly(
    type = "sankey",
    arrangement = "fixed",
    orientation = "h",
    node = list(
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      label = nodos$label2,
      color = nodos$colores,
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = nodos$texto
    ),
    link = list(
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = links$texto,
      source = links$source,
      target = links$target,
      value = links$value,
      color = "rgba(0,0,0,0.3)"
    )
  ) %>%
    layout(clickmode = "event+select") %>%
    event_register("plotly_click") %>%
    config(locale = "es",displayModeBar=F)


  return(list(plot = sankey, nodos = nodos, arcos = links))
}
