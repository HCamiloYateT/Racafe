#' Envolver un elemento del sidebar en un contenedor con ID
#'
#' Esta función envuelve un `bs4Dash::bs4SidebarMenuItem()` en un `div`
#' cuyo ID sigue el patrón `wrap_{tabName}` para facilitar reglas CSS
#' globales como `[id^='wrap_']`.
#'
#' @param label Etiqueta visible del elemento del menú.
#' @param tabName Nombre de la pestaña asociada al elemento.
#' @param icon Ícono del elemento del menú.
#'
#' @return Un contenedor `shiny::div()` con ID `wrap_{tabName}` que incluye
#'   el `bs4SidebarMenuItem`.
#' @export
SidebarItemWrap <- function(label, tabName, icon) {
  shiny::div(
    id = paste0("wrap_", tabName),
    bs4Dash::bs4SidebarMenuItem(label, tabName = tabName, icon = icon)
  )
}
