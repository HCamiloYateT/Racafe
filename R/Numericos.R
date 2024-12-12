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

#' Calcula la variación porcentual entre dos valores.
#'
#' Esta función calcula la variación porcentual entre un valor inicial (`ini`) y un valor final (`fin`).
#' La variación se calcula en relación con el valor absoluto de `ini`, y maneja correctamente
#' los casos en que `ini` o `fin` pueden ser cero, positivos o negativos.
#'
#' @param ini Valor inicial. Puede ser un número positivo, negativo o cero.
#' @param fin Valor final. Puede ser un número positivo, negativo o cero.
#'
#' @return Un número que representa la variación porcentual entre `ini` y `fin`.
#' Si `ini` es cero y `fin` también es cero, devuelve 0. Si `ini` es cero y `fin`
#' es diferente de cero, devuelve el signo de `fin` (1 o -1).
#' En caso de que ambos sean distintos de cero, calcula la variación como
#' \eqn{(fin - ini) / |ini|}.
#'
#' @examples
#' Variacion(10, 15)   # Devuelve 0.5, lo que representa un aumento del 50%.
#' Variacion(-10, -5)  # Devuelve 0.5, lo que representa un aumento del 50%.
#' Variacion(0, 5)     # Devuelve 1, porque pasa de 0 a un valor positivo.
#' Variacion(0, -5)    # Devuelve -1, porque pasa de 0 a un valor negativo.
#' Variacion(10, -5)   # Devuelve -1.5, representa una disminución significativa.
#'
#' @export
Variacion <- function(ini, fin) {
  if (ini == 0 & fin == 0) {
    return(0)
  }
  else if (ini == 0) {
    return(sign(fin))
  }
  else {
    return((fin - ini) / abs(ini))
  }
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

