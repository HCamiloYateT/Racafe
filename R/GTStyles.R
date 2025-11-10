#' Aplicar estilo porcentual a columnas de una tabla `gt`
#'
#' @description
#' Resalta columnas numéricas de una tabla creada con `gt`, aplicando colores al texto
#' según el rango del porcentaje.
#'
#' @param gt_table Un objeto de clase [`gt::gt()`] al que se le aplicarán los estilos.
#' @param ... Nombres de columnas dentro de `gt_table` que contienen porcentajes.
#'
#' @return El objeto `gt_table` con los estilos agregados.
#'
#' @examples
#' \dontrun{
#' library(gt)
#' data <- data.frame(etiqueta = c("A", "B"), pct = c(0.45, 0.92))
#' data %>%
#'   gt() %>%
#'   gt_pct_style(pct)
#' }
#'
#' @export
#' @seealso [gt_var_style()], [gt_color_columns()]
gt_pct_style <- function(gt_table, ...) {
  cols <- rlang::ensyms(...)

  for (col in cols) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_text(color = "#C11007", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col < 0.5)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#FF8904", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col >= 0.5 & !!col < 0.8)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#62A602", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col >= 0.8 & !!col < 1)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#62748e", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col >= 1)
      )
  }

  gt_table
}

#' Aplicar estilo de variación a columnas de una tabla `gt`
#'
#' @description
#' Colorea el texto de columnas numéricas de una tabla `gt` para distinguir valores
#' negativos y positivos.
#'
#' @inheritParams gt_pct_style
#'
#' @return El objeto `gt_table` con los estilos agregados.
#'
#' @examples
#' \dontrun{
#' library(gt)
#' data <- data.frame(etiqueta = c("A", "B"), variacion = c(-0.1, 0.25))
#' data %>%
#'   gt() %>%
#'   gt_var_style(variacion)
#' }
#'
#' @export
#' @seealso [gt_pct_style()], [gt_color_columns()]
gt_var_style <- function(gt_table, ...) {
  cols <- rlang::ensyms(...)

  for (col in cols) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_text(color = "#C11007", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col < 0)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#62A602", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col >= 0)
      )
  }

  gt_table
}

#' Aplicar estilo según el signo de los valores en columnas de una tabla `gt`
#'
#' @description
#' Colorea el texto de columnas numéricas de una tabla creada con `gt` usando
#' un esquema tricolor: rojo para valores negativos, verde para valores
#' positivos y negro para los valores exactamente iguales a cero.
#'
#' @inheritParams gt_pct_style
#'
#' @return El objeto `gt_table` con los estilos agregados.
#'
#' @examples
#' \dontrun{
#' library(gt)
#' data <- data.frame(etiqueta = c("A", "B", "C"), signo = c(-2, 0, 5))
#' data %>%
#'   gt() %>%
#'   gt_sign_style(signo)
#' }
#'
#' @export
#' @seealso [gt_var_style()], [gt_pct_style()]
gt_sign_style <- function(gt_table, ...) {
  if (!inherits(gt_table, "gt_tbl")) {
    rlang::abort("`gt_table` debe ser un objeto creado con `gt::gt()`.")
  }

  cols <- rlang::ensyms(...)

  if (length(cols) == 0) {
    rlang::abort("Debe especificar al menos una columna para aplicar el estilo.")
  }

  gt_data <- gt_table[['_data']]

  if (is.null(gt_data)) {
    rlang::abort("No fue posible acceder a los datos internos de `gt_table`.")
  }

  for (col in cols) {
    col_name <- rlang::as_name(col)

    if (!col_name %in% names(gt_data)) {
      rlang::abort(sprintf("La columna '%s' no existe en `gt_table`.", col_name))
    }

    if (!is.numeric(gt_data[[col_name]])) {
      rlang::abort(sprintf("La columna '%s' debe ser numérica para aplicar `gt_sign_style()`.", col_name))
    }

    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_text(color = "#C11007", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col < 0)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#62A602", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col > 0)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#000000", weight = "bold"),
        locations = gt::cells_body(columns = !!col, rows = !!col == 0)
      )
  }

  gt_table
}

#' Colorear columnas completas en una tabla `gt`
#'
#' @description
#' Aplica un color de relleno a columnas completas de una tabla `gt`, incluyendo los
#' encabezados de las columnas.
#'
#' @param gt_table Un objeto de clase [`gt::gt()`] al que se le aplicarán los estilos.
#' @param columns Un vector de caracteres con los nombres de las columnas a colorear.
#' @param color Un color válido reconocido por `gt` para usar como relleno.
#'
#' @return El objeto `gt_table` con los estilos agregados.
#'
#' @examples
#' \dontrun{
#' library(gt)
#' data <- data.frame(etiqueta = c("A", "B"), valor = c(10, 20))
#' data %>%
#'   gt() %>%
#'   gt_color_columns(columns = "valor", color = "#f0f0f0")
#' }
#'
#' @export
#' @seealso [gt_pct_style()], [gt_var_style()]
gt_color_columns <- function(gt_table, columns, color) {
  gt_table %>%
    gt::tab_style(
      style = gt::cell_fill(color = color),
      locations = gt::cells_body(columns = tidyselect::all_of(columns))
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = color),
      locations = gt::cells_column_labels(columns = tidyselect::all_of(columns))
    )
}
