#' Crear un input numérico estilizado para Shiny
#'
#' @param id Identificador único del input.
#' @param label Etiqueta que describe el campo.
#' @param value Valor inicial del input.
#' @param dec Número de decimales a mostrar (por defecto 2).
#' @param max Valor máximo permitido (por defecto NULL).
#' @param min Valor mínimo permitido (por defecto NULL).
#' @param type Tipo de input: "dinero", "porcentaje" o "numero".
#' @param label_col Ancho de la columna para la etiqueta (por defecto 6).
#' @param input_col Ancho de la columna para el campo numérico (por defecto 6).
#' @param width Ancho del control `autonumericInput` (por defecto "100%").
#'
#' @return Un objeto de tipo `fluidRow` con el diseño del input.
#' @examples
#' # Ejemplo para un input de dinero
#' InputNumerico("dinero_input", "Monto:", 1000, type = "dinero")
#' 
#' # Ejemplo para un input de porcentaje
#' InputNumerico("porcentaje_input", "Porcentaje:", 50, type = "porcentaje")
#' 
#' # Ejemplo para un input numérico general
#' InputNumerico("numero_input", "Cantidad:", 10, max = 100, min = 0, dec = 3, type = "numero")
InputNumerico <- function(id, label, value, dec = 2, max = NULL, min = NULL, type = "numero", label_col = 6, input_col = 6, width = "100%") {
  type <- match.arg(type, c("dinero", "porcentaje", "numero"))

  # Validaciones de tipo de datos
  stopifnot(is.numeric(value))
  if (!is.null(max)) stopifnot(is.numeric(max))
  if (!is.null(min)) stopifnot(is.numeric(min))

  # Configuración específica según el tipo de input
  config <- switch(type,
                   dinero = list(currencySymbol = "$", decimalPlaces = dec, max = max, min = min),
                   porcentaje = list(currencySymbol = "%", currencySymbolPlacement = "s", decimalPlaces = 2, max = 100, min = 0),
                   numero = list(currencySymbol = NULL, decimalPlaces = dec, max = max, min = min),
                   stop("Tipo de input no soportado. Use 'dinero', 'porcentaje' o 'numero'."))

  # Validaciones de rangos
  if (!is.null(config$min) && !is.null(config$max)) {
    stopifnot(config$min <= config$max)
    stopifnot(config$min <= value, value <= config$max)
  } else if (!is.null(config$min)) {
    stopifnot(config$min <= value)
  } else if (!is.null(config$max)) {
    stopifnot(value <= config$max)
  }

  # Construcción del componente visual
  res <- shiny::fluidRow(
    shiny::column(label_col, FormatearTexto(label, tamano_pct = 0.8)),
    shiny::column(input_col, shiny::autonumericInput(
      id,
      label = NULL,
      value = value,
      maximumValue = config$max,
      minimumValue = config$min,
      currencySymbol = config$currencySymbol,
      currencySymbolPlacement = config$currencySymbolPlacement,
      decimalPlaces = config$decimalPlaces,
      width = width,
      style = "height: 25px !important; font-size: 14px;"
    ))
  )
  return(res)
}

#' Genera opciones para selectPicker
#'
#' Esta función construye una lista de opciones personalizadas para el widget `selectPicker` de la librería `shinyWidgets`.
#' Permite configurar opciones de selección masiva y formato de texto seleccionado.
#'
#' @param cho Vector con las opciones del selector. Se utiliza para configurar límites del formato de selección.
#' @param fem Lógico. Indica si el texto debe ser femenino ("Todas") o masculino ("Todos"). Por defecto, \code{TRUE} (femenino).
#'
#' @return Una lista con las opciones necesarias para personalizar el comportamiento de `selectPicker`.
#' @export
#'
#' @examples
#' pick_opt(letters) # Opciones con valores femeninos "Todas"
#' pick_opt(letters, fem = FALSE) # Opciones con valores masculinos "Todos"
pick_opt <- function(cho, fem = TRUE) {

  # Texto dinámico dependiendo del género
  tod <- ifelse(fem, "Todas", "Todos")

  # Lista de opciones configurables para selectPicker
  res <- list(
    `live-search` = TRUE,                       # Habilita la búsqueda en vivo
    `actions-box` = TRUE,                       # Muestra botones de selección/deselección
    `deselect-all-text` = paste("Deseleccionar", tod), # Texto para deseleccionar todos
    `select-all-text` = paste("Seleccionar", tod),     # Texto para seleccionar todos
    `selected-text-format` = paste0("count > ", max(length(cho) - 1, 0)), # Formato para mostrar la cantidad seleccionada
    `count-selected-text` = tod,                # Texto mostrado al seleccionar todas las opciones
    `none-selected-text` = ""                   # Texto cuando no hay opciones seleccionadas
  )

  # Retorna la lista de opciones
  return(res)
}
