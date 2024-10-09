#' Limpiar y Normalizar Nombres
#'
#' Esta función limpia y normaliza una cadena de texto eliminando espacios repetidos y
#' convirtiendo todo a mayúsculas. Se asegura de que los espacios múltiples sean reducidos a uno solo
#' y elimina espacios en blanco al principio y al final de la cadena.
#'
#' @param s Un vector de caracteres con los nombres o cadenas de texto a limpiar.
#' @return Un vector de caracteres con las cadenas normalizadas, sin espacios en blanco duplicados y en mayúsculas.
#' @examples
#' LimpiarNombres("  Camilo    Yate     Támara   ")  # Devuelve "CAMILO YATE TÄMARA"
#' @export
LimpiarNombres <- function(s) {
  # Quitar espacios extra, reducir múltiples espacios a uno, y convertir a mayúsculas
  x <- trimws(str_to_upper(gsub("([\\s])\\1+", "\\1", s, perl = TRUE)))

  return(x)
}

#' Limpiar y Normalizar una Cadena de Texto
#'
#' Esta función limpia y normaliza una cadena o un vector de texto aplicando diferentes transformaciones,
#' tales como la eliminación de espacios, números, caracteres especiales y acentos, según se especifique.
#'
#' @param x Un vector de caracteres con las cadenas de texto a limpiar.
#' @param rem_espacios Un valor lógico (TRUE o FALSE) que indica si se deben eliminar todos los espacios en blanco.
#' El valor predeterminado es \code{FALSE}.
#' @param rem_numeros Un valor lógico que indica si se deben eliminar los números.
#' El valor predeterminado es \code{TRUE}.
#' @param rem_caresp Un valor lógico que indica si se deben eliminar los caracteres especiales (no alfanuméricos).
#' El valor predeterminado es \code{TRUE}.
#' @param rem_acentos Un valor lógico que indica si se deben eliminar los acentos.
#' El valor predeterminado es \code{TRUE}.
#' @return Un vector de caracteres con las cadenas de texto limpias, después de aplicar las transformaciones especificadas.
#' @examples
#' LimpiarCadena(c("Â¡Hola, mundo!", "Texto con números 123 y acentos áéíóú."))
#' # Devuelve: "HOLA MUNDO" "TEXTO CON NUMEROS Y ACENTOS AEIOU"
#' @export
LimpiarCadena <- function(x, rem_espacios = FALSE, rem_numeros = TRUE, rem_caresp = TRUE, rem_acentos = TRUE) {
  require(stringr)

  # Convertir a mayúsculas y eliminar espacios repetidos
  x <- trimws(str_to_upper(gsub("([\\s])\\1+", "\\1", x, perl = TRUE)))

  # Eliminar espacios si se indica
  if (rem_espacios) {
    x <- gsub("\\s", "", x)
  }

  # Eliminar números si se indica
  if (rem_numeros) {
    x <- gsub("\\d", "", x)
  }

  # Eliminar caracteres especiales si se indica
  if (rem_caresp) {
    x <- gsub("[^[:alnum:][:space:]]", "", x)
  }

  # Eliminar acentos si se indica
  if (rem_acentos) {
    x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  }

  return(x)
}


#' Unir Múltiples Cadenas de Texto
#'
#' Esta función une múltiples cadenas de texto en una sola cadena, con la opción de omitir valores \code{NA}.
#' Es similar a \code{paste}, pero permite controlar la eliminación de valores ausentes.
#'
#' @param ... Varios vectores de caracteres que se desean unir.
#' @param sep Separador entre las cadenas. El valor predeterminado es un espacio en blanco (\code{" "}).
#' @param collapse Un valor opcional para definir un separador entre los elementos resultantes,
#' similar a \code{paste}'s \code{collapse}. El valor predeterminado es \code{NULL}.
#' @param na.rm Un valor lógico que indica si los valores \code{NA} deben ser eliminados.
#' El valor predeterminado es \code{FALSE}, lo que incluye los \code{NA} en el resultado.
#' @return Una cadena de texto resultante de la unión de las cadenas especificadas.
#' @examples
#' UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
#' # Devuelve "Hola-Mundo"
#' UnirCadenas("Hola", NA, "Mundo", sep = " ", na.rm = FALSE)
#' # Devuelve "Hola NA Mundo"
#' @export
UnirCadenas <- function(..., sep = " ", collapse = NULL, na.rm = FALSE) {

  # Función auxiliar para unir, omitiendo NA y espacios en blanco si na.rm es TRUE
  paste.na <- function(x, sep) {
    x <- gsub("^\\s+|\\s+$", "", x)  # Eliminar espacios en los extremos
    ret <- paste(na.omit(x), collapse = sep)  # Omitir NA y unir
    if (ret == "") return(NA)  # Si el resultado está vacío, retornar NA
    return(ret)
  }

  if (!na.rm) {
    # Usar paste normal si no se omiten los NA
    return(paste(..., sep = sep, collapse = collapse))
  } else {
    # Si na.rm es TRUE, creamos un data.frame para unir fila por fila, evitando NA
    df <- data.frame(..., stringsAsFactors = FALSE)
    ret <- apply(df, 1, function(row) paste.na(row, sep))

    # Si collapse es NULL, devolver vector; si no, aplicar collapse
    if (is.null(collapse)) {
      return(ret)
    } else {
      return(paste.na(ret, sep = collapse))
    }
  }
}

#' Obtener Valores Únicos Ordenados
#'
#' Esta función recibe un vector, elimina los valores `NA`, extrae los valores únicos y los devuelve en orden ascendente.
#'
#' @param x Un vector de cualquier tipo (numérico, carácter, etc.) del que se desea obtener los valores únicos ordenados.
#' @return Un vector con los valores únicos de `x`, ordenados de menor a mayor. Los valores `NA` son eliminados.
#' @examples
#' Unicos(c(3, 1, NA, 2, 3, 1, 4, NA))  # Devuelve: 1, 2, 3, 4
#' Unicos(c("b", "a", NA, "a", "c"))    # Devuelve: "a", "b", "c"
#' @export
Unicos <- function(x) {
  require(dplyr)
  x[!is.na(x)]  %>%   # Elimina los valores NA
    unique() %>%     # Extrae los valores únicos
    sort()         # Ordena los valores
}
