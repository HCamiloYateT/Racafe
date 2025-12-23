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

#' Revisar llaves duplicadas entre dos tablas
#'
#' @description
#' Evalúa si dos data.frames tienen filas duplicadas según un conjunto de llaves
#' y devuelve el detalle de las filas repetidas en cada tabla. Admite llaves con
#' nombres idénticos o mapeos distintos entre `x` y `y` siguiendo la sintaxis de
#' `dplyr`.
#'
#' @param x,y Data.frames a evaluar.
#' @param by Vector de caracteres con los nombres de las columnas llave o un
#'   vector nombrado en el formato `col_x = col_y` cuando las llaves difieren.
#'
#' @return Una lista con cuatro elementos: `x_tiene_duplicados` y
#'   `y_tiene_duplicados` (valores lógicos) y `duplicados_x` y `duplicados_y`
#'   (data.frames con las filas duplicadas en cada tabla).
#'
#' @examples
#' library(dplyr)
#'
#' datos_a <- tibble::tibble(id = c(1, 1, 2), valor = c("a", "a", "b"))
#' datos_b <- tibble::tibble(id = c(1, 2, 2), valor_extra = c("x", "y", "y"))
#'
#' RevisarDuplicados(datos_a, datos_b, by = "id")
#'
#' # Llaves con nombres distintos
#' datos_c <- tibble::tibble(id_local = c(10, 10), valor = c("A", "B"))
#' datos_d <- tibble::tibble(id_remoto = c(10, 20), valor = c("C", "D"))
#' RevisarDuplicados(datos_c, datos_d, by = c(id_local = "id_remoto"))
#'
#' @references Wickham H., François R., Henry L. & Müller K. (2023). dplyr: A
#'   Grammar of Data Manipulation. R package version 1.1.4.
#'
#' @export
RevisarDuplicados <- function(x, y, by) {
  if (missing(by) || is.null(by) || length(by) == 0) {
    stop("El argumento 'by' debe contener al menos una columna llave.")
  }

  if (!is.data.frame(x)) {
    stop("El objeto 'x' debe ser un data.frame.")
  }

  if (!is.data.frame(y)) {
    stop("El objeto 'y' debe ser un data.frame.")
  }

  if (!is.character(by)) {
    stop("El argumento 'by' debe ser un vector de caracteres.")
  }

  if (!is.null(names(by)) && any(names(by) == "")) {
    stop("Cuando se usan llaves con nombres distintos, todos los elementos de 'by' deben estar nombrados.")
  }

  if (any(is.na(by))) {
    stop("El argumento 'by' no puede contener valores NA.")
  }

  if (is.null(names(by)) || all(names(by) == "")) {
    llaves_x <- by
    llaves_y <- by
  } else {
    llaves_x <- names(by)
    llaves_y <- unname(by)
  }

  if (any(duplicated(llaves_x))) {
    stop("Las llaves en 'x' no deben contener nombres duplicados.")
  }

  if (any(duplicated(llaves_y))) {
    stop("Las llaves en 'y' no deben contener nombres duplicados.")
  }

  faltan_x <- setdiff(llaves_x, colnames(x))
  faltan_y <- setdiff(llaves_y, colnames(y))

  if (length(faltan_x) > 0) {
    stop("Llaves inexistentes en 'x': ", paste(faltan_x, collapse = ", "))
  }

  if (length(faltan_y) > 0) {
    stop("Llaves inexistentes en 'y': ", paste(faltan_y, collapse = ", "))
  }

  dup_x <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(llaves_x))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(llaves_x)))

  dup_y <- y %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(llaves_y))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(llaves_y)))

  list(
    x_tiene_duplicados = nrow(dup_x) > 0,
    y_tiene_duplicados = nrow(dup_y) > 0,
    duplicados_x = dup_x,
    duplicados_y = dup_y
  )
}
