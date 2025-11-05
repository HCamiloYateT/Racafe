
#' Crear Línea Vertical en un Gráfico
#'
#' Esta función genera una línea vertical que puede ser utilizada en gráficos interactivos,
#' como los creados con la librería \code{plotly}. La línea se dibuja en una posición especificada
#' sobre el eje x y se puede personalizar su color.
#'
#' @param x Posición en el eje x donde se desea trazar la línea (por defecto: 0).
#' @param color Color de la línea (por defecto: "red").
#' @return Una lista con las propiedades necesarias para dibujar la línea vertical en un gráfico.
#' @examples
#' vline(2)  # Traza una línea vertical en x = 2 con color rojo.
#' vline(3, "blue")  # Traza una línea vertical en x = 3 con color azul.
#' @export
vline <- function(x = 0, color = "red") {
  # Definir las propiedades de la línea vertical
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",  # Usa "paper" para que la línea cubra todo el gráfico
    x0 = x,
    x1 = x,
    line = list(color = color)  # Asigna el color a la línea
  )
}

#' Crear Línea Horizontal en un Gráfico
#'
#' Esta función genera una línea horizontal que puede ser utilizada en gráficos interactivos,
#' como los creados con la librería \code{plotly}. La línea se dibuja en una posición especificada
#' sobre el eje y y se puede personalizar su color.
#'
#' @param y Posición en el eje y donde se desea trazar la línea (por defecto: 0).
#' @param color Color de la línea (por defecto: "#ff3a21").
#' @return Una lista con las propiedades necesarias para dibujar la línea horizontal en un gráfico.
#' @examples
#' hline(3)  # Traza una línea horizontal en y = 3 con color #ff3a21.
#' hline(5, "green")  # Traza una línea horizontal en y = 5 con color verde.
#' @export
hline <- function(y = 0, color = "#ff3a21") {
  # Definir las propiedades de la línea horizontal
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",  # Usa "paper" para que la línea cubra todo el gráfico
    y0 = y,
    y1 = y,
    line = list(color = color)  # Asigna el color a la línea
  )
}

#' Generar Colores para Gráficos según Valores
#'
#' Esta función asigna colores a un conjunto de valores, con una excepción específica
#' para los valores que contienen la cadena "RACAFE & CIA S C A", que siempre se asigna un color particular
#' (firebrick). Para los otros valores, se asignan colores graduados desde el gris claro
#' hasta el gris oscuro, dependiendo de la cantidad de valores.
#'
#' @param input_values Un vector de valores para los cuales se desea asignar colores.
#'                     Los valores que contengan la cadena "RACAFE & CIA S C A" serán asignados
#'                     el color 'firebrick'. El resto de los valores recibirá colores graduales de gris.
#' @return Un vector de colores asignados a cada valor en el vector de entrada.
#'         Los valores que coincidan con "RACAFE & CIA S C A" (usando `grepl`) tendrán el color 'firebrick',
#'         y el resto de los valores se asignarán colores graduales de gris (desde gris claro a gris oscuro).
#'
#' @examples
#' # Asignar colores a un vector de valores
#' ColoresRacafe(c("RACAFE & CIA S C A", "Otro Valor", "Valor Importante"))
#'
#' @import grDevices
#' @export
ColoresRacafe <- function(input_values) {

  # Verificar si la entrada es un vector
  if (!is.vector(input_values)) {
    stop("El parámetro 'input_values' debe ser un vector.")
  }

  # Calcular la cantidad de valores en el vector
  num_valores <- length(input_values)

  # Crear una paleta de colores desde gris claro (gray90) a gris oscuro (gray20)
  pal <- colorRampPalette(c("gray90", "gray20"))

  # Asignar los colores graduales a los valores (de acuerdo con el número de valores)
  cols <- pal(num_valores)

  # Usar grepl para buscar coincidencias parciales con la cadena "RACAFE"
  # y asignar 'firebrick' a los valores coincidentes, el resto recibirá colores graduales
  colores_asignados <- ifelse(grepl("RACAFE", input_values, ignore.case = T), 'firebrick', cols)

  # Retornar los colores asignados
  return(colores_asignados)
}


#' Generar Colores Graduales entre Verde y Azul
#'
#' Esta función genera una paleta de colores graduales que van desde el color verde
#' "forestgreen" hasta el color azul oscuro "royalblue4", según el número de valores que se pasen como argumento.
#'
#' @param value Un vector de valores para los cuales se desea generar una paleta de colores.
#'              La cantidad de valores determinará cuántos colores se generarán en la paleta.
#' @return Un vector de colores graduales entre verde y azul, con una longitud igual al número de valores de entrada.
#'
#' @examples
#' # Generar una paleta de colores para 5 valores
#' ColoresGreenBlue(c("A", "B", "C", "D", "E"))
#'
#' @import grDevices
#' @export
ColoresGreenBlue <- function(value) {

  # Verificar si la entrada es un vector
  if (!is.vector(value)) {
    stop("El parámetro 'value' debe ser un vector.")
  }

  # Calcular la cantidad de valores
  num_valores <- length(value)

  # Crear una paleta de colores desde verde "forestgreen" hasta azul oscuro "royalblue4"
  pal <- colorRampPalette(c("forestgreen", "royalblue4"))

  # Asignar los colores graduales a los valores
  cols <- pal(num_valores)

  # Retornar los colores asignados
  return(cols)
}

#' Crear Gráfico de Anillo Interactivo
#'
#' Genera un gráfico tipo anillo (gráfico de dona) usando \pkg{plotly} a partir de un
#' conjunto de datos y una variable categórica. Los valores de cada segmento se calculan
#' utilizando un conteo simple o aplicando una función de agregación sobre otra variable.
#'
#' @param data Un objeto `data.frame` que contiene la información a graficar.
#' @param var_label Cadena de caracteres con el nombre de la variable categórica que se usará como etiqueta.
#' @param var_medida Cadena de caracteres con el nombre de la variable numérica sobre la cual se aplicará la función de agregación cuando `funcion != "n"`.
#' @param funcion Función a utilizar para calcular los valores de los segmentos. Las opciones disponibles son `"sum"` y `"n"`.
#' @param colores Vector opcional de colores que se asignará a los segmentos del gráfico.
#'
#' @return Un objeto de clase `plotly` con el gráfico tipo anillo.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Categoria = c("A", "B", "C"),
#'   Valor = c(10, 20, 30)
#' )
#'
#' # Utilizar una función de suma para calcular los valores
#' ImprimirAnillo(data, "Categoria", "Valor", funcion = "sum")
#'
#' # Realizar un conteo simple de observaciones por categoría
#' ImprimirAnillo(data, "Categoria", funcion = "n")
#' }
#'
#' @importFrom dplyr count group_by summarise mutate
#' @importFrom forcats fct_reorder
#' @importFrom plotly plot_ly add_pie layout config
#' @importFrom rlang sym
#' @importFrom stringr str_to_sentence
#' @export
ImprimirAnillo <- function(data, var_label, var_medida = NULL, funcion = c("sum", "n"), colores = NULL) {
  funcion  <- match.arg(funcion)
  var_lab  <- sym(var_label)

  if (funcion == "n") {
    aux1 <- data %>%
      count(!!var_lab, name = "Var")
  } else {
    if (is.null(var_medida)) {
      stop("Debe especificar 'var_medida' cuando funcion != 'n'")
    }
    var_med <- sym(var_medida)
    fun_ag  <- match.fun(funcion)

    aux1 <- data %>%
      group_by(!!var_lab) %>%
      summarise(Var = fun_ag(!!var_med, na.rm = TRUE), .groups = "drop")
  }

  total <- sum(aux1$Var, na.rm = TRUE)

  aux1 <- aux1 %>%
    mutate(Lab  = as.character(!!var_lab),
           Lab  = sapply(Lab, function(x) paste(strwrap(str_to_sentence(x), width = 30), collapse = "<br>")),
           Pct  = Var / total,
           Text = sprintf("%.1f%%", Pct * 100))

  aux1$Lab <- fct_reorder(aux1$Lab, aux1$Var, max)

  # Colores
  if (!is.null(colores)) {
    colores <- rep(colores, length.out = nrow(aux1))
    marker_list <- list(line = list(width = 2), colors = colores)
  } else {
    marker_list <- list(line = list(width = 2))
  }

  # Texto del hover: etiqueta, valor y porcentaje
  hover_tmp <- paste0(
    "<b>%{label}</b><br>",
    if (funcion == "n") "%{value} <b>(" else paste0("%{value} <b>("),
    "%{percent:.1%})</b><extra></extra>"
  )

  plot_ly(aux1) %>%
    add_pie(labels = ~Lab, values = ~Var, type = "pie", sort = TRUE,
            hole = 0.4, text = ~Text, textinfo = "text", textposition = "inside",
            hovertemplate = hover_tmp, marker = marker_list) %>%
    layout(margin = list(t = 40, b = 40, l = 40, r = 40),
           legend = list(orientation = "h", xanchor = "center",
                         x = 0.5, y = -0.1, font = list(size = 9, color = "black"))) %>%
    config(displayModeBar = FALSE)
}
