#' Operador coalescente nulo
#'
#' @description Devuelve el primer argumento cuando no es nulo ni un vector vacío
#'   o con un único valor \code{NA}; en caso contrario, devuelve el segundo
#'   argumento.
#'
#' @param a,b Valores a evaluar.
#'
#' @return El valor de \code{a} cuando está definido; en caso contrario, \code{b}.
#'
#' @examples
#' NULL %||% "valor"
#' NA %||% 10
#' "texto" %||% "alternativa"
#'
#' @export
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) {
    return(b)
  }
  a
}

#' Unir iterativamente múltiples tablas
#'
#' @description
#' Realiza un proceso iterativo de unión entre un data.frame inicial y una lista
#' de data.frames empleando las funciones de unión de `dplyr`. Dependiendo del
#' valor de `type` se utiliza `left_join()`, `inner_join()`, `right_join()` o
#' `full_join()` en cada paso.
#'
#' @param x Un data.frame inicial que servirá como base para las uniones.
#' @param y_list Una lista de data.frames que se unirán secuencialmente a `x`.
#' @param by Vector de nombres de columnas o una lista nombrada que especifica
#'   las claves de unión, tal como se utiliza en las funciones de `dplyr`.
#' @param type Tipo de unión a realizar. Debe ser uno de "left", "inner",
#'   "right" o "full". Por defecto se realiza un `left_join()`.
#'
#' @return Un data.frame con el resultado de aplicar las uniones en orden.
#'
#' @examples
#' library(dplyr)
#' base <- tibble::tibble(id = 1:3, valor = letters[1:3])
#' extras <- list(
#'   tibble::tibble(id = c(1, 2), extra = c("A", "B")),
#'   tibble::tibble(id = c(2, 3), otra = c("X", "Y"))
#' )
#' left_join_all(base, extras, by = "id")
#'
#' @export
#' @importFrom dplyr left_join inner_join right_join full_join
left_join_all <- function(x, y_list, by, type = "left") {
  result <- x

  for (y in y_list) {
    if (!is.null(y) && nrow(y) > 0) {
      if (type == "left") {
        result <- left_join(result, y, by = by)
      } else if (type == "inner") {
        result <- inner_join(result, y, by = by)
      } else if (type == "right") {
        result <- right_join(result, y, by = by)
      } else if (type == "full") {
        result <- full_join(result, y, by = by)
      }
    }
  }

  result
}
