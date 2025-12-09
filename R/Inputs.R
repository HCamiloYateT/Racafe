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
#' @return Un objeto de tipo `tagList` con el input numérico alineado al
#'   formato visual de `ListaDesplegable`.
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
  custom_css <- shiny::tags$style(htmltools::HTML("\n    .custom-numeric-input {\n      width: 100% !important;\n    }\n    .custom-numeric-input .form-group,\n    .custom-numeric-input .shiny-input-container {\n      width: 100% !important;\n      margin-bottom: 0;\n    }\n    .custom-numeric-input input.form-control {\n      width: 100% !important;\n      height: 35px !important;\n      font-size: 14px !important;\n      box-sizing: border-box !important;\n      border-radius: 5px;\n    }\n    .custom-numeric-input input.form-control:focus {\n      border-color: #71717B !important;\n      box-shadow: 0 0 0 0.2rem rgba(128, 128, 128, 0.25) !important;\n      outline: none !important;\n    }\n  "))

  label_tag <- if (is.null(label)) {
    NULL
  } else {
    shiny::div(class = "custom-numeric-label", FormatearTexto(label, tamano_pct = 0.85))
  }

  shiny::tagList(
    custom_css,
    shiny::div(
      class = "custom-numeric-input",
      label_tag,
      shinyWidgets::autonumericInput(
        id,
        label = NULL,
        value = value,
        maximumValue = config$max,
        minimumValue = config$min,
        currencySymbol = config$currencySymbol,
        currencySymbolPlacement = config$currencySymbolPlacement,
        decimalPlaces = config$decimalPlaces,
        width = width
      )
    )
  )
}

#' Lista Desplegable Personalizada
#'
#' @title Lista Desplegable con Búsqueda, Selección Múltiple y Etiquetas Dinámicas
#' @description Crea un `shinyWidgets::pickerInput()` totalmente estilizado que
#'   adapta su etiqueta según la cantidad de elementos seleccionados. Incluye
#'   búsqueda en vivo, botones para seleccionar/deseleccionar todas las opciones,
#'   soporte para textos en femenino o masculino y compatibilidad con módulos de
#'   Shiny mediante namespaces.
#'
#' @param inputId Cadena de caracteres. ID único del input para identificarlo en el
#'   servidor.
#' @param label Cadena de caracteres o `NULL`. Etiqueta que se mostrará encima del
#'   input. Si es `NULL`, no se mostrará etiqueta.
#' @param choices Vector nombrado o lista. Opciones disponibles para seleccionar.
#'   Puede ser un vector de caracteres o una lista nombrada.
#' @param selected Vector. Opciones que estarán seleccionadas por defecto. Por
#'   defecto se seleccionan todas las opciones disponibles.
#' @param multiple Lógico. Si es `TRUE` permite selección múltiple, si es `FALSE`
#'   solo una opción. Por defecto es `TRUE`.
#' @param fem Lógico. Si es `TRUE` usa terminología femenina ("Todas",
#'   "Ninguna"), si es `FALSE` usa terminología masculina ("Todos",
#'   "Ninguno"). Por defecto es `FALSE`.
#' @param ns Función de namespace o `NULL`. Cuando se suministra, se aplica al
#'   `inputId` automáticamente, lo que facilita su uso dentro de módulos de Shiny.
#'
#' @return Un objeto `tagList` con el CSS personalizado y el `pickerInput`
#'   configurado con todas las opciones especificadas.
#'
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom shiny tags tagList
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
  add_empty <- is.null(selected)

  if (is.null(selected)) selected <- ""
  if (add_empty && !("" %in% choices)) choices <- c("", choices)

  # ID final para el JavaScript (con namespace si existe)
  final_id <- if (!is.null(ns)) ns(inputId) else inputId
  safe_id  <- gsub("[^A-Za-z0-9]", "_", final_id)

  # Textos dinámicos según género
  tod <- ifelse(fem, "Todas", "Todos")
  nin <- ifelse(fem, "Ninguna", "Ninguno")
  sel <- ifelse(fem, "seleccionadas", "seleccionados")

  # Calcular umbral para selected-text-format en función de la cantidad de opciones
  n_choices <- length(choices)
  threshold <- max(n_choices - 1, 0)

  # Opciones del picker (incluye selected-text-format y count-selected-text)
  if (multiple) {
    picker_options <- pickerOptions(
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
      style                 = "btn-default",
      "selected-text-format" = paste0("count > ", threshold),
      "count-selected-text"  = tod
    )
  } else {
    picker_options <- pickerOptions(
      liveSearch            = TRUE,
      liveSearchNormalize   = TRUE,
      liveSearchPlaceholder = "Buscar...",
      liveSearchStyle       = "contains",
      actionsBox            = FALSE,
      noneSelectedText      = nin,
      noneResultsText       = "No hay resultados {0}",
      showTick              = TRUE,
      style                 = "btn-default",
      "selected-text-format" = "values"
    )
  }

  # CSS personalizado (barra de búsqueda 100%)
  custom_css <- tags$style(htmltools::HTML("
    .custom-picker {
      width: 100% !important;
    }
    .custom-picker .form-group,
    .custom-picker .shiny-input-container,
    .custom-picker .bootstrap-select,
    .custom-picker .btn-group {
      width: 100% !important;
      margin-bottom: 0;
      box-sizing: border-box !important;
    }
    .custom-picker .dropdown-toggle {
      width: 100% !important;
    }

    /* Menú del dropdown */
    .custom-picker .dropdown-menu {
      max-height: 250px;
      overflow-y: auto;
      width: 100% !important;
      box-sizing: border-box !important;
    }

    /* Contenedor de búsqueda */
    .custom-picker .bs-searchbox {
      width: 100% !important;
      padding: 5px !important;
      box-sizing: border-box !important;
    }

    /* Input de búsqueda forzado a 100% real */
    .custom-picker .bs-searchbox input.form-control,
    .custom-picker .bs-searchbox input[type='search'],
    .custom-picker .bs-searchbox input {
      width: 100% !important;
      height: 35px !important;
      max-width: 100% !important;
      display: block !important;
      box-sizing: border-box !important;
      border: 1px solid #ccc;
      border-radius: 3px;
      padding: 6px 10px;
    }

    .custom-picker .bs-searchbox input:focus {
      border-color: #71717B !important;
      box-shadow: 0 0 0 0.2rem rgba(128, 128, 128, 0.25) !important;
      outline: none !important;
    }

    /* Items del dropdown */
    .custom-picker .dropdown-item {
      padding: 6px 12px;
      font-size: 14px;
      border-top: 1px solid #F4F4F5;
      border-bottom: 1px solid #F4F4F5;
    }
    .custom-picker .dropdown-menu.show {
      border: 1px solid #ddd;
      border-radius: 5px;
      box-shadow: 0px 2px 8px rgba(0,0,0,0.15);
    }

    /* Botones de acciones (Seleccionar/Deseleccionar) */
    .custom-picker .bs-actionsbox {
      padding: 4px 8px;
    }
    .custom-picker .bs-actionsbox .btn {
      font-size: 12px;
      padding: 2px 6px;
    }
  "))

  # JavaScript dinámico para actualizar la etiqueta del botón
  custom_js <- if (multiple) {
    tags$script(htmltools::HTML(paste0("\n      $(document).ready(function() {\n        function updateLabel_", safe_id, "() {\n          var select   = $('#", final_id, "');\n          var selected = select.val();\n          var total    = select.find('option').length;\n          var button   = select.next('.dropdown-toggle');\n\n          if (selected && selected.length === total) {\n            button.find('.filter-option-inner-inner').text('", tod, "');\n          } else if (selected && selected.length > 0) {\n            button.find('.filter-option-inner-inner').text(selected.length + ' ", sel, "');\n          } else {\n            button.find('.filter-option-inner-inner').text('", nin, "');\n          }\n        }\n        setTimeout(function() { updateLabel_", safe_id, "(); }, 100);\n        $('#", final_id, "').on('changed.bs.select', updateLabel_", safe_id, ");\n      });\n    ")))
  } else {
    NULL
  }

  # Construcción final - pasar inputId original (no final_id) al pickerInput
  tagList(
    custom_css,
    custom_js,
    div(
      class = "custom-picker",
      pickerInput(
        inputId  = inputId,
        label    = label,
        choices  = choices,
        selected = selected,
        multiple = multiple,
        options  = picker_options,
        width    = "100%"
      )
    )
  )
}

#' Opciones personalizadas para pickerInput
#'
#' @description Genera un conjunto consistente de opciones para
#'   `shinyWidgets::pickerInput()` con textos en español y soporte para
#'   terminología femenina o masculina.
#'
#' @param cho Vector o lista de opciones utilizadas en el picker. Puede ser
#'   un vector simple o una lista nombrada como la que acepta `pickerInput()`.
#' @param fem Lógico. Cuando es `TRUE` utiliza terminología femenina ("Todas",
#'   "seleccionadas"), de lo contrario emplea la forma masculina. El valor por
#'   defecto es `TRUE`.
#'
#' @return Un objeto de clase `pickerOptions`, equivalente a una lista, que
#'   puede pasarse al argumento `options` de `pickerInput()`.
#' @export
#'
#' @examples
#' pick_opt(letters[1:5])
pick_opt <- function(cho, fem = TRUE) {
  fem <- isTRUE(fem)

  # Calcular el total de opciones, soportando listas nombradas
  total <- if (is.null(cho)) {
    0L
  } else if (is.list(cho) && !is.data.frame(cho)) {
    sum(vapply(cho, length, integer(1)))
  } else {
    length(cho)
  }

  tod <- if (fem) "Todas" else "Todos"
  nin <- if (fem) "Ninguna" else "Ninguno"
  sel <- if (fem) "seleccionadas" else "seleccionados"

  shinyWidgets::pickerOptions(
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
    style                 = "btn-default",
    selectedTextFormat    = sprintf("count > %d", total),
    countSelectedText     = sprintf("{0} de %d %s", total, sel)
  )
}

#' Botones radiales estilizados
#'
#' @description Genera un conjunto de botones radiales basados en
#'   `shinyWidgets::radioGroupButtons()` con estilos personalizados y soporte
#'   para íconos y tooltips. Funciona tanto en aplicaciones Shiny comunes como
#'   dentro de módulos utilizando un namespace opcional.
#'
#' @param inputId Cadena de caracteres. Identificador único del input.
#' @param label Cadena de caracteres o `NULL`. Etiqueta que se mostrará junto al
#'   grupo de botones.
#' @param choices Vector con las opciones disponibles. Puede ser nombrado para
#'   mostrar etiquetas distintas a los valores enviados al servidor.
#' @param selected Valor inicialmente seleccionado. Debe coincidir con uno de
#'   los elementos de `choices`.
#' @param usar_iconos Lógico. Si es `TRUE` utiliza los nombres de iconos de
#'   Font Awesome especificados en `iconos`.
#' @param iconos Vector de caracteres con los nombres de los iconos a usar. Debe
#'   tener la misma longitud que `choices` cuando `usar_iconos = TRUE`.
#' @param tooltips Vector de caracteres con los textos de ayuda que se mostrarán
#'   como tooltips al pasar el cursor sobre cada botón. Debe tener la misma
#'   longitud que `choices`.
#' @param alineacion Cadena de caracteres que define la alineación horizontal
#'   del grupo de botones. Puede ser "left", "center" o "right". Por defecto
#'   es "left".
#' @param ns Función de namespace o `NULL`. Se utiliza para adaptar el
#'   identificador cuando el input se usa dentro de módulos.
#'
#' @return Un objeto `tagList` listo para ser incluido en una interfaz Shiny.
#'
#' @importFrom shinyWidgets radioGroupButtons
#'
#' @examples
#' BotonesRadiales(
#'   inputId = "estado",
#'   label = "Estado:",
#'   choices = c(Activo = "activo", Inactivo = "inactivo"),
#'   selected = "activo"
#' )
#'
#' @export
BotonesRadiales <- function(inputId, label = NULL, choices, selected = NULL,
                            usar_iconos = FALSE, iconos = NULL, tooltips = NULL,
                            alineacion = c("left", "center", "right"),
                            ns = NULL) {

  alineacion <- match.arg(alineacion)

  justify_value <- switch(
    alineacion,
    left   = "flex-start",
    center = "center",
    right  = "flex-end"
  )

  final_id <- if (!is.null(ns)) ns(inputId) else inputId

  if (usar_iconos && !is.null(iconos)) {
    choiceNames <- lapply(seq_along(choices), function(i) {
      shiny::tags$span(
        shiny::icon(iconos[i]),
        if (!is.null(names(choices)) && nzchar(names(choices)[i])) {
          paste0(" ", names(choices)[i])
        } else {
          ""
        }
      )
    })
    choiceValues <- unname(choices)
  } else {
    choiceNames <- if (!is.null(names(choices))) names(choices) else choices
    choiceValues <- unname(choices)
  }

  result <- htmltools::tagList(
    shiny::tags$style(htmltools::HTML(sprintf(
      "\n      #%s .btn-group-toggle .btn {\n        border: none !important;\n        border-radius: 5px !important;\n        margin: 0 1px !important;\n        background-color: transparent !important;\n        color: #dc3545 !important;\n        box-shadow: none !important;\n      }\n      #%s .btn-group-toggle .btn:hover {\n        background-color: #f8f9fa !important;\n      }\n      #%s .btn-group-toggle .btn.active {\n        background-color: #dc3545 !important;\n        color: #fff !important;\n      }\n    ", final_id, final_id, final_id))),
    shiny::div(
      style = sprintf(
        "display: flex; justify-content: %s; align-items: center; margin-top: 25px; width: 100%%;",
        justify_value
      ),
      shinyWidgets::radioGroupButtons(
        inputId = final_id,
        label = label,
        choiceNames = choiceNames,
        choiceValues = choiceValues,
        selected = selected,
        justified = FALSE,
        individual = TRUE,
        status = "primary"
      )
    )
  )

  if (!is.null(tooltips) && length(tooltips) == length(choices)) {
    tooltip_script <- shiny::tags$script(htmltools::HTML(sprintf(
      "\n      $(document).ready(function() {\n        var buttons = $('#%s .btn-group-toggle .btn');\n        var tooltips = %s;\n        buttons.each(function(i) {\n          $(this).attr('title', tooltips[i]).tooltip({container: 'body'});\n        });\n      });\n    ", final_id, jsonlite::toJSON(tooltips))))

    result <- htmltools::tagList(result, tooltip_script)
  }

  return(result)
}

#' Botón de estado con iconos dinámicos
#'
#' @description Genera un botón estilizado que refleja un estado binario mediante
#'   iconos y títulos dinámicos. El color, el tamaño y los textos asociados pueden
#'   personalizarse para adaptarse a diferentes interfaces dentro de aplicaciones
#'   Shiny.
#'
#' @param button_id Cadena de caracteres. Identificador único del botón.
#' @param estado Lógico. Define el estado inicial del botón. Cuando es `TRUE`,
#'   se muestra el icono y título asociados al estado activo.
#' @param icono_verdadero Cadena de caracteres. Nombre del icono de Font Awesome
#'   que se mostrará cuando `estado` sea `TRUE`.
#' @param icono_falso Cadena de caracteres. Nombre del icono de Font Awesome que
#'   se mostrará cuando `estado` sea `FALSE`.
#' @param color Cadena de caracteres. Color principal utilizado para el borde y
#'   el texto del botón.
#' @param titulo_verdadero Cadena de caracteres. Texto utilizado como título
#'   (tooltip) cuando `estado` es `TRUE`.
#' @param titulo_falso Cadena de caracteres. Texto utilizado como título
#'   (tooltip) cuando `estado` es `FALSE`.
#' @param size Cadena de caracteres. Tamaño del botón. Puede ser "sm", "md" o
#'   "lg".
#'
#' @return Un objeto `shiny::tags$span` que contiene un `shiny::actionButton`
#'   configurado con estilos e iconos dinámicos.
#'
#' @examples
#' BotonEstado(
#'   button_id = "toggle_estado",
#'   estado = TRUE,
#'   icono_verdadero = "check-circle",
#'   icono_falso = "times-circle",
#'   color = "#28a745",
#'   titulo_verdadero = "Activo",
#'   titulo_falso = "Inactivo"
#' )
#'
#' @export
BotonEstado <- function(
  button_id,
  estado = FALSE,
  icono_verdadero = "check-circle",
  icono_falso = "x-circle",
  color = "#FF3222",
  titulo_verdadero = "Activo",
  titulo_falso = "Inactivo",
  size = "sm"
) {

  size_config <- list(
    sm = list(padding = "3px 8px", font_size = "12px"),
    md = list(padding = "6px 12px", font_size = "14px"),
    lg = list(padding = "8px 16px", font_size = "16px")
  )

  size <- match.arg(size, names(size_config))
  current_size <- size_config[[size]]

  button_style <- paste0(
    "-webkit-text-size-adjust:100%;-webkit-tap-highlight-color:transparent;",
    "word-wrap:break-word;box-sizing:border-box;line-height:inherit;",
    "text-transform:none;margin:0;border-width:1px;font-weight:400;",
    "position:relative;overflow:hidden;border:1px solid ", color, "40;",
    "border-radius:4px;background:transparent;",
    "transition:all .3s cubic-bezier(0.02,0.01,0.47,1);",
    "outline:none;-webkit-appearance:button;",
    "padding:", current_size$padding, ";",
    "font-size:", current_size$font_size, ";",
    "font-family:inherit;color:", color, ";cursor:pointer!important;"
  )

  icon_obj <- if (isTRUE(estado)) {
    shiny::icon(icono_verdadero)
  } else {
    shiny::icon(icono_falso)
  }

  titulo <- if (isTRUE(estado)) titulo_verdadero else titulo_falso

  shiny::tags$span(
    title = titulo,
    shiny::actionButton(
      inputId = button_id,
      label = NULL,
      icon = icon_obj,
      class = "btn-estado",
      style = button_style
    )
  )
}
