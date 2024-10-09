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

#' Calcular Variación Porcentual
#'
#' Esta función calcula la variación porcentual entre un número inicial y uno final.
#' La variación porcentual se calcula de acuerdo con las siguientes reglas:
#'  - Si ambos valores son 0, la variación es 0.
#'  - Si el valor inicial es 0 y el final es distinto de 0, la variación es 0.
#'  - Si ambos valores tienen el mismo signo (o son ambos positivos o ambos negativos), se calcula como \code{(fin - ini) / ini}.
#'  - Si los valores tienen signos opuestos, la variación se calcula como \code{(fin - ini) / abs(ini)}.
#'
#' @param ini El número inicial (denominador).
#' @param fin El número final (numerador).
#' @return Un valor numérico que representa la variación porcentual.
#' @examples
#' Variacion(10, 15)   # Devuelve 0.5 (50% de aumento)
#' Variacion(0, 10)    # Devuelve 0 (por convención en este caso)
#' Variacion(-10, 5)   # Devuelve 1.5 (150% de aumento, en valor absoluto del denominador)
#' @export
Variacion <- function(ini, fin) {
  # Verificar casos especiales para evitar divisiones innecesarias
  if (ini == 0 & fin == 0) {
    return(0)
  } else if (ini == 0) {
    return(0)  # Si el inicial es 0, la variación es 0 por convención
  } else if (ini * fin >= 0) {
    # Si ambos números tienen el mismo signo, calcular la variación normal
    return((fin - ini) / ini)
  } else {
    # Si los números tienen signos opuestos, calcular la variación sobre el valor absoluto del inicial
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
