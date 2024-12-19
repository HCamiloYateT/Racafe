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
    `selected-text-format` = paste0("count > ", length(cho) - 1), # Formato para mostrar la cantidad seleccionada
    `count-selected-text` = tod,                # Texto mostrado al seleccionar todas las opciones
    `none-selected-text` = ""                   # Texto cuando no hay opciones seleccionadas
  )

  # Retorna la lista de opciones
  return(res)
}
