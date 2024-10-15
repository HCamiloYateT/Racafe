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
  strrep('<br/>', n)
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

#' @title Obligatorio: Campo obligatorio con un asterisco rojo
#' @description Esta función genera un componente visual de campo obligatorio con un asterisco rojo al lado de la etiqueta.
#' @param s Texto que se mostrará como etiqueta del campo obligatorio.
#' @return Un objeto HTML que contiene el texto de la etiqueta con un asterisco rojo que indica que el campo es obligatorio.
#' @examples
#' Obligatorio("Nombre")  # Añade un asterisco rojo a la etiqueta "Nombre" para indicar que es obligatorio.
#' @export
Obligatorio <- function(s) {
  # Genera un campo de etiqueta con un asterisco rojo indicando obligatoriedad
  tagList(
    # Usa h6 para crear un encabezado de tamaño pequeño con el texto de la etiqueta
    # y un span que contiene el asterisco con estilo inline (color rojo)
    h6(HTML(s, span("*", style = "color: red;")))
  )
}



