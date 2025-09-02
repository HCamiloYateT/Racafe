#' Consulta a una base de datos SQL Server.
#'
#' Esta función se conecta a una base de datos SQL Server y ejecuta una consulta SQL.
#' Dependiendo del valor del parámetro `bd`, se selecciona la base de datos correspondiente.
#' Después de ejecutar la consulta, limpia los nombres de las columnas en el dataframe resultante.
#'
#' @param bd Una cadena de texto que especifica el nombre de la base de datos a la que conectarse.
#'           Puede ser uno de los siguientes valores: "syscafe", "cafesys" o "estad".
#' @param uid El nombre de usuario para la conexión a la base de datos.
#' @param pwd La contraseña del usuario para la conexión.
#' @param query La consulta SQL que se ejecutará en la base de datos.
#'
#' @return Un dataframe con los resultados de la consulta, con los nombres de las columnas limpiados.
#' @export
ConsultaSistema <- function(bd, uid, pwd, query) {

  # Asigna el nombre de la base de datos en función del valor de `bd`
  base <- dplyr::case_when(
    bd == "syscafe" ~ "ContabRacafe",
    bd == "cafesys" ~ "Cafesys",
    bd == "estad" ~ "EstadRacafe"
  )

  if (is.na(base)) stop("las bases de datos disponibles son: 'syscafe', 'cafesys' o 'estad'")

  # Establece la conexión con la base de datos SQL Server
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "ODBC Driver 18 for SQL Server",
                        Server = "172.16.19.21",
                        Database = base,
                        uid = uid,
                        pwd = pwd,
                        port = 1433,
                        TrustServerCertificate = "yes")

  # Ejecuta la consulta SQL y limpia los nombres de las columnas
  df <- DBI::dbGetQuery(con, query) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), racafe::LimpiarNombres))

  # Cierra la conexión con la base de datos
  DBI::dbDisconnect(con)

  # Retorna el dataframe con los resultados
  df
}


#' Recodificación de Categorías Menos Frecuentes
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode El nombre de la variable que se desea recodificar.
#' @param var_top El nombre de la variable a partir de la cual se calcularÃ¡n las frecuencias o la función de resumen.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param n El número mÃ¡ximo de categorías principales a conservar (predeterminado: 10).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @export
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, nom_var, lab_recodificar = "OTROS"){

  datos <- data

  # Calcula las frecuencias o las estadísticas según la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos |>
      dplyr::mutate(Tot = dplyr::n()) |>
      dplyr::group_by_at(var_recode) |>
      dplyr::summarise(Var = dplyr::n(),
                       Pct = Var/unique(Tot))
  } else {
    aux1 <- datos |>
      dplyr::mutate(Tot = !!rlang::parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) |>
      dplyr::group_by_at(var_recode) |>
      dplyr::summarise(Var = !!rlang::parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                       Pct = Var/unique(Tot))
  }

  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 |>
    dplyr::arrange(dplyr::desc(Var)) |>
    dplyr::mutate(Seq = dplyr::row_number(),
                  !!nom_var := ifelse(Seq <= n, as.character(!!rlang::parse_expr(var_recode)), lab_recodificar)
    ) |>
    dplyr::select(dplyr::all_of(var_recode), dplyr::all_of(nom_var))

  # Recodifica las categorías en el dataset original
  data <- datos |>
    dplyr::left_join(aux2, by = var_recode) |>
    dplyr::mutate(!!nom_var := factor(!!rlang::sym(nom_var), levels = unique(aux2[[nom_var]]), ordered = TRUE),
                  !!nom_var := forcats::fct_relevel(!!rlang::sym(nom_var), lab_recodificar, after = Inf))

  data
}


#' Recodificación de Categorías Menos Frecuentes (Relativa)
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor relativo o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode El nombre de la variable que se desea recodificar.
#' @param var_top El nombre de la variable a partir de la cual se calcularÃ¡n las frecuencias o la función de resumen.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param pct_min El porcentaje mínimo necesario para considerar una categoría principal (predeterminado: 0.05).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @export
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, nom_var, lab_recodificar = "OTROS") {
  datos <- data

  # Calcula las frecuencias o estadísticas relativas de acuerdo a la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos |>
      dplyr::mutate(Tot = dplyr::n()) |>
      dplyr::group_by_at(var_recode) |>
      dplyr::summarise(Var = dplyr::n(),
                       Pct = Var / unique(Tot))
  } else {
    aux1 <- datos |>
      dplyr::mutate(Tot = !!rlang::parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) |>
      dplyr::group_by_at(var_recode) |>
      dplyr::summarise(Var = !!rlang::parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                       Pct = Var / unique(Tot))
  }

  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 |>
    dplyr::arrange(dplyr::desc(Var)) |>
    dplyr::mutate(Seq = dplyr::row_number(),
                  !!nom_var := !!rlang::parse_expr(paste0("ifelse(Pct > pct_min, as.character(",
                                                           var_recode, "), '",
                                                           lab_recodificar, "')"))) |>
    dplyr::select(dplyr::all_of(var_recode), dplyr::all_of(nom_var))

  # Recodifica las categorías en el dataset original
  data <- datos |>
    dplyr::left_join(aux2, by = var_recode) |>
    dplyr::mutate(!!nom_var := factor(!!rlang::sym(nom_var), levels = unique(aux2[[nom_var]]), ordered = TRUE),
                  !!nom_var := forcats::fct_relevel(!!rlang::sym(nom_var), lab_recodificar, after = Inf))

  data
}

#' Adiciona botones interactivos a una tabla.
#'
#' Esta función permite agregar botones interactivos a una tabla, cada uno
#' representando una acción específica. Los botones se pueden usar para
#' abrir detalles, registrar contactos, editar, eliminar o duplicar.
#'
#' @param tabla Un `data.frame` o `tibble` al que se le desean agregar los botones.
#' @param botones Un vector de caracteres que especifica qué botones agregar.
#' Las opciones disponibles son: "Detalle", "Contacto", "Editar", "Eliminar", "Duplicar".
#'
#' @return Un `data.frame` o `tibble` con nuevas columnas para los botones seleccionados.
#' @export
AdicionarBotones <- function(tabla, botones) {
  shiny::req(tabla)  # Asegura que la tabla no sea nula

  # Verifica si la tabla tiene filas
  if (nrow(tabla) > 0) {
    # Mapa de botones a su HTML correspondiente
    botones_html <- list(
      Detalle = "<span title='Abrir Detalle' style='cursor:pointer'>&#128270;</span>",
      Contacto = "<span title='Registrar Contacto' style='cursor:pointer'>&#128172;</span>",
      Editar = "<span title='Editar' style='cursor:pointer'>&#9997;</span>",
      Eliminar = "<span title='Eliminar' style='cursor:pointer'>&#9940;</span>",
      Duplicar = "<span title='Duplicar' style='cursor:pointer'>&#128203;</span>"
    )

    # Filtra solo los botones que se desean agregar
    botones_seleccionados <- intersect(botones, names(botones_html))

    # Crea nuevas columnas en la tabla para cada botón seleccionado
    for (boton in botones_seleccionados) {
      tabla <- tabla |>
        dplyr::mutate(!!boton := htmltools::HTML(botones_html[[boton]]))
    }
  }

  tabla  # Devuelve la tabla modificada
}


#' Combina múltiples data frames en uno solo, ignorando aquellos que están vacíos.
#'
#' Esta función toma varios data frames como entrada y los combina en un solo
#' data frame. Los data frames vacíos se ignoran para evitar errores al
#' realizar la unión.
#'
#' @param ... Data frames a combinar. Puede recibir uno o más data frames.
#'
#' @return Un data frame que resulta de la combinación de los data frames
#'         proporcionados. Si no hay data frames no vacíos, se devuelve un
#'         data frame vacío.
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = letters[1:3])
#' df2 <- data.frame(a = numeric(0), b = character(0))  # Data frame vacío
#' df3 <- data.frame(a = 4:5, b = letters[4:5])
#'
#' # Combina df1 y df3, ignorando df2
#' result <- bind_rows_na(df1, df2, df3)
#'
#' @export
bind_rows_na <- function(...) {
  # Crear una lista de data frames a partir de los argumentos
  df_list <- list(...)

  # Filtrar solo aquellos data frames que tienen filas
  df_list <- df_list[sapply(df_list, nrow) > 0]

  # Combinar los data frames filtrados
  dplyr::bind_rows(df_list)
}
