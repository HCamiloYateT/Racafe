#' Crear un input numérico estilizado para Shiny
#'
#' @param id Identificador único del input.
#' @param label Etiqueta que describe el campo.
#' @param value Valor inicial del input. Puede ser `NULL` o `NA`.
#' @param dec Número de decimales a mostrar (por defecto 2).
#' @param max Valor máximo permitido. Para `type = "porcentaje"` el valor
#'   por defecto es 100; en otros casos es `NULL`.
#' @param min Valor mínimo permitido. Para `type = "porcentaje"` el valor
#'   por defecto es 0; en otros casos es `NULL`.
#' @param type Tipo de input: "dinero", "porcentaje" o "numero".
#' @param label_col Ancho de la columna para la etiqueta (por defecto 6).
#' @param input_col Ancho de la columna para el campo numérico (por defecto 6).
#' @param width Ancho del control `autonumericInput` (por defecto "100%").
#'
#' @details
#' Cuando `type = "porcentaje"`, el rango permitido por defecto es de 0 a 100.
#' Estos límites pueden modificarse mediante los argumentos `min` y `max`.
#' Si `value` es `NULL` o `NA`, las validaciones de rango se omiten.
#'
#' @return Un objeto de tipo `fluidRow` con el diseño del input.
#' @export
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
  stopifnot(is.numeric(value) || is.null(value) || (length(value) == 1 && is.na(value)))
  if (!is.null(max)) stopifnot(is.numeric(max))
  if (!is.null(min)) stopifnot(is.numeric(min))

  # Configuración específica según el tipo de input
  config <- switch(
    type,
    dinero = list(currencySymbol = "$", decimalPlaces = dec, max = max, min = min),
    porcentaje = list(
      currencySymbol = "%",
      currencySymbolPlacement = "s",
      decimalPlaces = 2,
      max = if (is.null(max)) 100 else max,
      min = if (is.null(min)) 0 else min
    ),
    numero = list(currencySymbol = NULL, decimalPlaces = dec, max = max, min = min),
    stop("Tipo de input no soportado. Use 'dinero', 'porcentaje' o 'numero'.")
  )

  # Validaciones de rangos
  if (!is.null(value) && !(length(value) == 1 && is.na(value))) {
    if (!is.null(config$min) && !is.null(config$max)) {
      stopifnot(config$min <= config$max)
      stopifnot(config$min <= value, value <= config$max)
    } else if (!is.null(config$min)) {
      stopifnot(config$min <= value)
    } else if (!is.null(config$max)) {
      stopifnot(value <= config$max)
    }
  }

  # Construcción del componente visual
  res <- shiny::fluidRow(
    shiny::column(label_col, FormatearTexto(label, tamano_pct = 0.8)),
    shiny::column(input_col, shinyWidgets::autonumericInput(
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

#' Lista Desplegable Personalizada
#'
#' @title Lista Desplegable con Búsqueda y Selección Múltiple
#' @description Crea un input de selección múltiple personalizado con funcionalidades
#'   de búsqueda en vivo, selección/deselección masiva y estilos CSS personalizados.
#'   Incluye soporte para módulos de Shiny y textos dinámicos según género.
#'
#' @param inputId Cadena de caracteres. ID único del input para identificarlo en el servidor.
#' @param label Cadena de caracteres o NULL. Etiqueta que se mostrará encima del input.
#'   Si es NULL, no se mostrará etiqueta.
#' @param choices Vector nombrado o lista. Opciones disponibles para seleccionar.
#'   Puede ser un vector de caracteres o una lista nombrada.
#' @param selected Vector. Opciones que estarán seleccionadas por defecto.
#'   Por defecto selecciona todas las opciones disponibles.
#' @param multiple Lógico. Si TRUE permite selección múltiple, si FALSE solo una opción.
#'   Por defecto es TRUE.
#' @param fem Lógico. Si TRUE usa terminología femenina ("Todas", "Ninguna"),
#'   si FALSE usa terminología masculina ("Todos", "Ninguno"). Por defecto es FALSE.
#' @param ns Función de namespace o NULL. Función de namespace para módulos de Shiny.
#'   Si se proporciona, se aplicará al inputId automáticamente.
#'
#' @return Un objeto tagList de Shiny que contiene el CSS personalizado y el
#'   pickerInput configurado con todas las opciones especificadas.
#'
#' @examples
#' # Ejemplo básico con opciones de texto
#' ListaDesplegable(
#'   inputId = "mi_selector",
#'   label = "Selecciona opciones:",
#'   choices = c("Opción 1", "Opción 2", "Opción 3")
#' )
#'
#' # Ejemplo con terminología femenina
#' ListaDesplegable(
#'   inputId = "categorias",
#'   label = "Categorías:",
#'   choices = c("Categoría A", "Categoría B", "Categoría C"),
#'   fem = TRUE
#' )
#'
#' # Ejemplo para uso en módulos
#' # En el UI del módulo:
#' ListaDesplegable(
#'   inputId = "selector_modulo",
#'   label = "Opciones del módulo:",
#'   choices = c("A", "B", "C"),
#'   ns = ns
#' )
#'
#' # Ejemplo con selección única
#' ListaDesplegable(
#'   inputId = "unica_opcion",
#'   label = "Selecciona una opción:",
#'   choices = c("Solo A", "Solo B", "Solo C"),
#'   selected = "Solo A",
#'   multiple = FALSE
#' )
#'
#' @export
ListaDesplegable <- function(inputId, label = NULL, choices, selected = choices, multiple = TRUE, fem = FALSE, ns = NULL) {

  # Detectar si estamos en un módulo y manejar el ID correctamente
  final_id <- if (!is.null(ns)) ns(inputId) else inputId

  # Textos dinámicos según género
  tod <- ifelse(fem, "Todas", "Todos")
  nin <- ifelse(fem, "Ninguna", "Ninguno")
  sel <- ifelse(fem, "seleccionadas", "seleccionados")

  # Opciones del picker
  picker_options <- shinyWidgets::pickerOptions(
    liveSearch            = TRUE,
    liveSearchNormalize   = TRUE,
    liveSearchPlaceholder = "Buscar...",
    liveSearchStyle       = "contains",
    actionsBox            = TRUE,
    selectAllText         = paste("Seleccionar", tod),
    deselectAllText       = paste("Deseleccionar", tod),
    noneSelectedText      = nin,
    noneResultsText       = "No hay resultados {0}",
    showTick              = TRUE,
    width                 = "100%",
    style                 = "btn-default"
  )

  # CSS personalizado
  custom_css <- tags$style(HTML("
    .custom-picker .dropdown-menu {
      max-height: 250px;
      overflow-y: auto;
    }
    .custom-picker .dropdown-item {
      padding: 6px 12px;
      font-size: 14px;
      border-top: 1px solid #F4F4F5;
      border-bottom: 1px solid #F4F4F5;
    }
    .custom-picker .bs-searchbox input {
      border: 1px solid #ccc;
      border-radius: 4px;
      padding: 5px;
    }
    .custom-picker .bs-searchbox input:focus {
      border-color: #71717B !important;
      box-shadow: 0 0 0 0.2rem rgba(128, 128, 128, 0.25) !important;
      outline: none !important;
    }
    .custom-picker .dropdown-item.active {
      background-color: #71717B !important;
      color: #fff !important;
      font-weight: bold;
    }
    .custom-picker .dropdown-menu.show {
      border: 1px solid #ddd;
      border-radius: 5px;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
    }
    .custom-picker .bs-actionsbox {
      padding: 4px 8px;
    }
    .custom-picker .bs-actionsbox .btn {
      font-size: 12px;
      padding: 2px 6px;
    }
  "))

  # Construcción final del input
  res <- tagList(
    custom_css,
    div(class = "custom-picker",
        pickerInput(
          inputId = final_id,
          label = label,
          choices = choices,
          selected = selected,
          multiple = multiple,
          options = picker_options,
          width = "100%"
        )
    )
  )
  return(res)
}
