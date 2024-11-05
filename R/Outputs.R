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
             column(6, style = s_con, tags$p(style = s_tex, texto))  # Añadiendo el texto
         )
  )
}

#' Imprime un diagrama de Sankey utilizando datos agrupados.
#'
#' Esta función crea un diagrama de Sankey que representa las relaciones entre múltiples variables.
#' Permite resumir los datos utilizando una función específica y personalizar los colores de los nodos.
#'
#' @param data Un data frame que contiene los datos a visualizar.
#' @param vars Un vector de caracteres que especifica las variables a agrupar.
#' @param fun Una función de resumen que se aplicará a `var` (por ejemplo, "n" para contar).
#' @param var Una variable numérica opcional que se utilizará con `fun` para calcular el resumen.
#' @param colores Un vector de caracteres que define los colores para cada variable en `vars`.
#'
#' @return Una lista que contiene el objeto `plot` del diagrama de Sankey,
#'         el data frame `nodos` y el data frame `links`.
#' @export
ImprimeSankey <- function(data, vars, fun, var = NULL, colores) {

  # Verifica que el número de colores coincida con el número de variables
  if (length(vars) != length(colores)) {
    stop("El número de colores debe coincidir con el número de variables.")
  }

  # Crear una tabla de frecuencias para los valores únicos en las variables especificadas
  tb <- table(unlist(sapply(vars, function(x) unique(data[[x]]))) %>% as.character())
  tb <- tb[tb > 1] %>% unlist() %>% names()

  # Resumir los datos utilizando la función especificada
  if (fun == "n") {
    # Contar filas si `fun` es "n"
    aux1 <- data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = n(), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA),
                                           paste0(cur_column(), .), .)))
  } else {
    # Aplicar la función especificada a `var` si `fun` no es "n"
    aux1 <- data %>%
      group_by(Origen = "TOTAL", across(all_of(vars))) %>%
      summarise(Tot = !!parse_expr(paste0(fun, "(",
                                          var, ", na.rm = TRUE)")), .groups = 'drop') %>%
      mutate(across(all_of(vars), ~ ifelse(. %in% c(tb, "", NA),
                                           paste0(cur_column(), .), .)))
  }

  # Preparar etiquetas de nodos y colores
  vec <- c("Origen", vars)
  nds <- c("TOTAL", unlist(sapply(vars, function(x) Unicos(aux1[[x]]))) %>% as.character())

  # Generar paletas de colores personalizadas para cada variable en `vars`
  aux_col <- unlist(sapply(vars, function(x) {
    i = {parent.frame()$i[]}
    color_base <- colores[i]
    n_colores <- length(unique(aux1[[x]]))
    colorRampPalette(c(lighten(color_base, 0.5), color_base, darken(color_base, 0.5)))(n_colores)
  })) %>% as.character()

  # Crear descripciones de texto para cada nodo
  aux_txt <- unlist(sapply(vec, function(x) {
    tot <- sum(aux1$Tot)
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / tot, .groups = 'drop') %>%
      rowwise() %>%
      mutate(txt = paste0("<b>Clientes: </b>", comma(Tot, accuracy = 1),
                          "<br><b>Pct. del Total: </b>", percent(Pct, accuracy = 0.01))) %>%
      pull(txt)
  })) %>% as.character()

  # Crear etiquetas para cada variable en los nodos
  aux_var <- unlist(sapply(vec, function(x) {
    tot <- sum(aux1$Tot)
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), Pct = Tot / tot, .groups = 'drop') %>%
      mutate(Var = x) %>%
      pull(Var)
  })) %>% as.character()

  # Crear el data frame de nodos con etiquetas, colores y descripciones
  nodos <- data.frame(
    label = nds,
    label2 = Reduce(function(x, pattern) gsub(pattern, "", x), vars, nds),
    indices = seq_along(nds) - 1,
    colores = c("black", aux_col),
    AuxTexto = aux_txt,
    Var = aux_var
  ) %>%
    mutate(texto = paste0("<b>", label2, "</b><br>", AuxTexto))

  # Crear el data frame de enlaces (links) entre nodos
  n <- length(vec) - 1
  aux_lista <- lapply(1:n, function(i) c(vec[i], vec[i + 1]))
  links <- do.call("bind_rows", lapply(aux_lista, function(x) {
    aux1 %>%
      group_by(across(all_of(x))) %>%
      summarise(Tot = sum(Tot), .groups = 'drop') %>%
      left_join(nodos %>% select(label, indices, VarSource = Var) %>% rename(source = indices), by = setNames("label", x[1]))  %>%
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
                     "<br><b>Clientes: </b>", comma(value, accuracy = 1),
                     "<br><b>Pct. del Total: </b>", percent(PctTot, accuracy = 0.01),
                     "<br><b>Pct. del Origen: </b>", percent(PctSource, accuracy = 0.01)
      )
    )

  # Crear el diagrama de Sankey con `plotly`
  sankey <- plot_ly(
    type = "sankey",
    arrangement= "fixed",
    orientation = "h",
    node = list(
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      label = nodos$label2,
      color = nodos$colores,
      hovertemplate = paste0("%{customdata}%", "<extra></extra>"),
      customdata = nodos$texto
    ),
    link = list(
      hovertemplate = paste0("%{customdata}%", "<extra></extra>"),
      customdata = links$texto,
      source = links$source,
      target = links$target,
      value = links$value,
      color = "rgba(0,0,0,0.3)")
  ) %>%
    layout(clickmode = "event+select") %>%
    event_register("plotly_click")

  return(list(plot = sankey, nodos = nodos, arcos = links))
}
