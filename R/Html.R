#' Insertar Saltos de Línea HTML
#'
#' Esta función genera una cadena con un número especificado de saltos de línea en formato HTML (\code{<br/>}).
#'
#' @param n Un valor entero que indica el número de saltos de línea a generar. El valor predeterminado es \code{1}.
#' @return Una cadena de texto que contiene \code{<br/>} repetido \code{n} veces.
#' @examples
#' Saltos(3)  # Devuelve "<br/><br/><br/>"
#' @export
Saltos <- function(n = 1) {
  strrep('<br/>', n) %>% HTML()
}

#' Insertar Espacios en HTML
#'
#' Esta función genera una cadena con un número especificado de espacios en formato HTML (\code{&emsp;}).
#'
#' @param n Un valor entero que indica el número de espacios a generar. El valor predeterminado es \code{1}.
#' @return Una cadena de texto que contiene \code{&emsp;} repetido \code{n} veces.
#' @examples
#' Espacios(4)  # Devuelve "&emsp;&emsp;&emsp;&emsp;"
#' @export
Espacios <- function(n = 1) {
  strrep('&emsp;', n)
}

#' @title Obligatorio: Añade un asterisco rojo a un campo obligatorio
#' @description Esta función genera una etiqueta que indica que un campo es obligatorio, añadiendo un asterisco de color rojo junto al texto de la etiqueta.
#' @param s Un string que representa el texto de la etiqueta que será mostrado en el formulario.
#' @details La función envuelve el texto de la etiqueta proporcionada en un encabezado HTML de nivel 6 (`h6`) y añade un asterisco en color rojo, utilizando la funcionalidad de `shiny`.
#' @return Un objeto `HTML` que será renderizado en la interfaz de usuario de `shiny`, mostrando una etiqueta con un asterisco rojo que denota obligatoriedad.
#' @examples
#' Obligatorio("Nombre")  # Crea una etiqueta con la palabra "Nombre" y un asterisco rojo.
#' @export
Obligatorio <- function(s) {
  # Crea un encabezado HTML de nivel 6 con el texto y un asterisco rojo.
  h6(paste(s, span("*", style = "color: red;")) %>% HTML)
}



