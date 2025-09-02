#' Generar una Caja con Ícono y Texto
#'
#' Esta función crea una caja visual que incluye un ícono y un texto. La caja puede personalizarse en cuanto a color de fondo, altura, color de letra e ícono.
#'
#' @param texto Un carácter que representa el texto que se mostrará en la caja.
#' @param icono Un carácter que representa el nombre del ícono a utilizar (usando la librería de íconos de `shiny` o `font-awesome`).
#' @param col_fondo Un carácter que representa el color de fondo de la caja (por defecto: `"#FDFEFE"`).
#' @param alto Un número que representa la altura mínima de la caja en píxeles (por defecto: `120`).
#' @param col_letra Un carácter que representa el color del texto (por defecto: `"#17202A"`).
#' @param col_icono Un carácter que representa el color del ícono (por defecto: `"#000000"`).
#' @return Un objeto HTML que contiene la caja con el ícono y el texto estilizados.
#' @import shiny
#' @import colorspace
#' @examples
#' CajaIco("Este es un texto", "info-circle", col_fondo = "#E0E0E0", col_letra = "#333333")
#' @export
CajaIco <- function(texto, icono, col_fondo = "#FDFEFE", alto = 120, col_letra = "#17202A", col_icono = "#000000") {

  # Definición de estilos para la caja
  s_caj <- paste0(
    "display: block; background:", col_fondo, "; min-height:", alto,
    "px; inline-size: 100%; overflow-wrap: break-word; border-radius: 10px;",
    "box-shadow: 1px 1px 2px ", darken(col_fondo, 0.1), ";"
  )

  # Definición de estilos para el ícono
  s_ico <- paste0(
    "position: absolute; text-align: left; font-size: 80px; color:",
    adjust_transparency(col_icono, 0.05),
    "; background: transparent; z-index: 1"
  )

  # Definición de estilos para el contenedor del texto
  s_con <- paste0(
    "inline-size: 50%; overflow-wrap: break-word; hyphens: manual; position: absolute;",
    "z-index: 3; margin: 0px; padding: 5px 10px; margin-top: 20px; margin-left: 10px;",
    "background: transparent;"
  )

  # Definición de estilos para el texto
  s_tex <- paste0(
    "color:", col_letra, "; text-align: left; vertical-align: text-top; font-size: 18px;"
  )

  # Generación de la caja con ícono y texto
  column(12,
         div(class = "row", style = s_caj,
             column(6, style = s_ico, icon(icono)),  # Añadiendo el ícono
             column(10, style = s_con, tags$p(style = s_tex, texto))  # Añadiendo el texto
         )
  )
}

#' Imprime un diagrama de Sankey con datos de agrupación
#'
#' Esta función crea un diagrama de Sankey con la biblioteca `plotly`, basado en las agrupaciones especificadas
#' en los datos de entrada y permite visualizar relaciones entre variables.
#'
#' @param data Data frame con los datos de entrada para el diagrama de Sankey.
#' @param vars Vector de nombres de columnas a utilizar en el diagrama.
#' @param fun Función de agregación a aplicar, como "n" para contar o funciones estadísticas (e.g., "sum", "mean").
#' @param var Nombre de la variable sobre la cual se aplica `fun` (opcional).
#' @param colores Vector de colores a aplicar a cada variable especificada en `vars`.
#'
#' @return Una lista con el gráfico Sankey (`plot`) y los datos de nodos y enlaces (`nodos`, `arcos`).
#'
#' @examples
#' ImprimeSankey(data = df, vars = c("Var1", "Var2"), fun = "n", colores = c("blue", "green"))
#'
#' @importFrom dplyr \%>% group_by summarise mutate select left_join across bind_rows
#' @importFrom plotly plot_ly layout
#' @importFrom plotly event_register config
#' @export
ImprimeSankey <- function(data, vars, fun, var = NULL, colores) {

  # Validación de la correspondencia de colores con variables
  if (length(vars) != length(colores)) {
    stop("El número de colores debe coincidir con el número de variables.")
  }

  # Crear tabla de frecuencias para valores únicos de las variables especificadas
  tb <- table(unlist(sapply(vars, function(x) Unicos(data[[x]]))) %>% as.character())
  tb <- tb[tb > 1] %>% unlist() %>% names()

  # Resumir los datos según la función especificada (conteo o agregación sobre `var`)
  aux1 <- if (fun == "n") {
    data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = n(), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA), paste0(cur_column(), .), .)))
  } else {
    data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = !!parse_expr(paste0(fun, "(", var, ", na.rm = TRUE)")), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA), paste0(cur_column(), .), .)))
  }

  # Definir etiquetas de nodos y colores
  vec <- c("Origen", vars)
  nds <- c("TOTAL", unlist(sapply(vars, function(x) Unicos(aux1[[x]]))) %>% as.character())

  # Generar colores personalizados para cada variable
  aux_col <- do.call("bind_rows", lapply(vars, function(x) {
    i <- which(vars == x)
    color_base <- colores[i]
    n_colores <- length(unique(aux1[[x]]))
    data.frame(
      label = Unicos(aux1[[x]]),
      colores = colorRampPalette(c(lighten(color_base, 0.5), color_base, darken(color_base, 0.5)))(n_colores)
    )
  })) %>%
    bind_rows(data.frame(label = "TOTAL", colores = "#000000"))

  # Crear descripciones de texto para cada nodo
  aux_txt <- do.call("bind_rows", lapply(vec, function(x) {
    tot <- sum(aux1$Tot)
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / tot, .groups = 'drop') %>%
      rowwise() %>%
      mutate(txt = paste0("<b>Clientes: </b>", scales::comma(Tot, accuracy = 1),
                          "<br><b>Pct. del Total: </b>", scales::percent(Pct, accuracy = 0.01))) %>%
      select(label = 1, txt)
  }))

  # Crear etiquetas de variable para los nodos
  aux_var <- do.call("bind_rows", lapply(vec, function(x) {
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / sum(aux1$Tot), .groups = 'drop') %>%
      mutate(Var = x) %>%
      select(label = 1, Var)
  }))

  # Data frame de nodos con etiquetas, colores y descripciones
  nodos <- data.frame(label = nds) %>%
    left_join(aux_col, by = "label") %>%
    left_join(aux_txt, by = "label") %>%
    left_join(aux_var, by = "label") %>%
    mutate(label2 = Reduce(function(x, pattern) gsub(pattern, "", x), vars, nds),
           indices = seq_along(nds) - 1,
           texto = paste0("<b>", label2, "</b><br>", txt))

  # Crear enlaces entre nodos
  n <- length(vec) - 1
  aux_lista <- lapply(1:n, function(i) c(vec[i], vec[i + 1]))
  links <- do.call("bind_rows", lapply(aux_lista, function(x) {
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), .groups = 'drop') %>%
      left_join(nodos %>% select(label, indices, VarSource = Var) %>% rename(source = indices), by = setNames("label", x[1])) %>%
      left_join(nodos %>% select(label, indices, VarTarget = Var) %>% rename(target = indices), by = setNames("label", x[2])) %>%
      select(source, target, value = Tot, VarSource, VarTarget)
  }))

  # Calcular valores totales para los enlaces
  total_values <- aggregate(value ~ source, data = links, sum)
  links <- links %>%
    left_join(total_values, by = "source", suffix = c("", "_total")) %>%
    left_join(nodos %>% select(source = indices, Origen = label2), by = "source") %>%
    left_join(nodos %>% select(target = indices, Destino = label2), by = "target") %>%
    mutate(
      PctTot = value / sum(aux1$Tot),
      PctSource = value / value_total,
      texto = paste0("<b>Origen: </b>", Origen,
                     "<br><b>Destino: </b>", Destino,
                     "<br><b>Clientes: </b>", scales::comma(value, accuracy = 1),
                     "<br><b>Pct. del Total: </b>", scales::percent(PctTot, accuracy = 0.01),
                     "<br><b>Pct. del Origen: </b>", scales::percent(PctSource, accuracy = 0.01))
    )

  # Crear el gráfico Sankey con `plotly`
  sankey <- plot_ly(
    type = "sankey",
    arrangement = "fixed",
    orientation = "h",
    node = list(
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      label = nodos$label2,
      color = nodos$colores,
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = nodos$texto
    ),
    link = list(
      hovertemplate = paste0("%{customdata}", "<extra></extra>"),
      customdata = links$texto,
      source = links$source,
      target = links$target,
      value = links$value,
      color = "rgba(0,0,0,0.3)"
    )
  ) %>%
    layout(clickmode = "event+select") %>%
    event_register("plotly_click") %>%
    config(locale = "es",displayModeBar=F)


  return(list(plot = sankey, nodos = nodos, arcos = links))
}
