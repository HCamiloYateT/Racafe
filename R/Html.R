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

#' Crear una Etiqueta Obligatoria con un Asterisco
#'
#' Esta función genera una etiqueta HTML que incluye el texto de la etiqueta y un asterisco (`*`)
#' para indicar que el campo es obligatorio. El asterisco es presentado con una clase CSS llamada
#' `mandatory_star` para facilitar el estilo visual.
#'
#' @param label Texto de la etiqueta que se desea mostrar.
#' @return Un objeto `tagList` que representa la etiqueta HTML con el asterisco visual.
#'
#' @examples
#' # Crear una etiqueta obligatoria
#' labelObligatorio("Nombre del Usuario")
#'
#' @export
labelObligatorio <- function(label) {
  # Retorna una lista de etiquetas HTML que incluye el texto y un asterisco para marcar como obligatorio
  tagList(
    label,  # El texto de la etiqueta
    span("*", class = "mandatory_star")  # Asterisco con clase CSS para estilo
  )
}
