#' Reemplazar Valores NaN, Inf y -Inf por 0
#'
#' Esta función reemplaza los valores `NaN`, `Inf` y `-Inf` en un vector numérico por 0.
#'
#' @param x Un vector numérico en el que se buscarán los valores `NaN`, `Inf` y `-Inf`.
#' @return Un nuevo vector numérico con los valores `NaN`, `Inf` y `-Inf` reemplazados por 0.
#' @examples
#' SiError_0(c(1, NaN, 2, Inf, -Inf))  # Devuelve: 1 0 2 0 0
#' @export
SiError_0 <- function(x) {
  # Reemplazar valores NaN, Inf y -Inf por 0 usando is.finite para la comprobación
  x[!is.finite(x)] <- 0
  return(x)
}

#' Calcula la variación relativa entre dos valores
#'
#' Esta función calcula la variación relativa entre dos vectores de valores iniciales y finales.
#' Si el valor inicial es cero, se define una variación especial basada en el signo del valor final.
#'
#' @param ini Vector numérico con los valores iniciales.
#' @param fin Vector numérico con los valores finales.
#' @return Un vector numérico con la variación relativa entre `ini` y `fin`.
#'   - Retorna `NA` si alguno de los valores es `NA`.
#'   - Retorna `0` si ambos valores son `0`.
#'   - Retorna `sign(y)` si `ini` es `0` y `fin` no es `0`.
#'   - En otros casos, retorna `(y - x) / abs(x)`.
#' @examples
#' Variacion(c(10, 0, NA, 5), c(15, 5, 20, 10))
#' # Retorna: c(0.5, 1, NA, 1)
#' @export
Variacion <- function(ini, fin) {
  mapply(function(x, y) {
    if (is.na(x) || is.na(y)) return(NA)
    if (x == 0 && y == 0) return(0)
    if (x == 0) return(sign(y))
    return((y - x) / abs(x))
  }, ini, fin)
}

#' Calcular la Moda de un Vector
#'
#' Esta función calcula la moda de un vector numérico o categórico. La moda es el valor que aparece con mayor frecuencia en el vector.
#' Si hay más de una moda, la función devuelve solo una de ellas.
#'
#' @param x El vector numérico o categórico del cual se desea calcular la moda.
#' @param na.rm Un valor lógico que indica si se deben excluir los valores `NA` en el cálculo de la moda.
#' El valor predeterminado es `TRUE`.
#' @return El valor de la moda del vector, es decir, el valor que aparece con mayor frecuencia. Si hay más de una moda, devuelve el primero encontrado.
#' @examples
#' Moda(c(1, 2, 2, 3, 4))  # Devuelve 2
#' Moda(c("a", "b", "b", "c", "c", "c"))  # Devuelve "c"
#' Moda(c(1, 1, 2, 2, 3), na.rm = TRUE)  # Devuelve 1
#' @export
Moda <- function(x, na.rm = TRUE) {
  # Eliminar valores NA si na.rm es TRUE
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Si el vector está vacío después de eliminar NA, devolver NA
  if (length(x) == 0) {
    return(NA)
  }

  # Calcular los valores únicos y determinar la frecuencia máxima
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' Redondea un número al múltiplo más cercano
#'
#' La función redondea un número al múltiplo más cercano de un valor especificado, similar a `REDOND.MULT` en Excel.
#'
#' @param x Un número. Valor que se desea redondear.
#' @param multiple Un número. Múltiplo al que se redondeará el valor de \code{x}.
#'
#' @return El número redondeado al múltiplo más cercano del valor especificado en \code{multiple}.
#' @examples
#' RedondearMultiplo(453, 100) # Devuelve 500
#' RedondearMultiplo(1234, 50) # Devuelve 1250
#'
#' @export
RedondearMultiplo <- function(x, multiple) {
  round(x / multiple) * multiple
}

