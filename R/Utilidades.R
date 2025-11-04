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
