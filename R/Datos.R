#' Consulta a una base de datos SQL Server.
#'
#' Esta función se conecta a una base de datos SQL Server y ejecuta una consulta SQL.
#' Dependiendo del valor del parámetro `bd`, se selecciona la base de datos correspondiente.
#' Después de ejecutar la consulta, limpia los nombres de las columnas en el dataframe resultante
#' y convierte a fecha las variables que empiezan por "Fec" o se llaman exactamente "Fecha".
#'
#' @param bd Una cadena de texto que especifica el nombre de la base de datos a la que conectarse.
#'           Puede ser uno de los siguientes valores: "syscafe", "cafesys" o "estad".
#' @param uid El nombre de usuario para la conexión a la base de datos.
#' @param pwd La contraseña del usuario para la conexión.
#' @param query La consulta SQL que se ejecutará en la base de datos.
#' @param server La dirección del servidor SQL Server. Por defecto "172.16.19.21".
#' @param port El puerto del servidor SQL Server. Por defecto 1433.
#'
#' @return Un dataframe con los resultados de la consulta, con los nombres de las columnas limpiados.
#' @export

ConsultaSistema <- function(bd, uid = Sys.getenv("SYS_UID"), pwd = Sys.getenv("SYS_PWD"), query, server = "172.16.19.21", port = 1433) {

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
                        Server = server,
                        Database = base,
                        uid = uid,
                        pwd = pwd,
                        port = port,
                        TrustServerCertificate = "yes")

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- tryCatch(
    DBI::dbGetQuery(con, query) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), racafe::LimpiarNombres),
        dplyr::across(c(dplyr::starts_with("Fec"), dplyr::matches("^Fecha$")), as.Date)
      ),
    error = function(e) {
      stop("Error al ejecutar la consulta: ", e$message)
    }
  )

  df
}

#' Conectar a la base de datos MySQL usando variables de entorno
#'
#' Establece una conexión a la base de datos MySQL usando las variables de entorno:
#' DB_NAME, DB_HOST, DB_PORT, DB_USER, DB_PASSWORD, DB_ENCODING.
#'
#' @return Un objeto de conexión DBI (DBIConnection).
#' @details Esta función ejecuta `SET NAMES 'utf8'` luego de abrir la conexión.
#' Asegúrate de que las variables de entorno estén definidas en el entorno donde se ejecuta el paquete.
#' @examples
#' \dontrun{
#' Sys.setenv(DB_NAME = "mi_bd", DB_HOST = "localhost", DB_PORT = "3306",
#'            DB_USER = "usuario", DB_PASSWORD = "secreto", DB_ENCODING = "latin1")
#' con <- ConectarBD()
#' DBI::dbDisconnect(con)
#' }
#' @export
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom RMySQL MySQL
ConectarBD <- function() {
  con <- dbConnect(
    RMySQL::MySQL(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    DBMSencoding = Sys.getenv("DB_ENCODING")
  )
  dbGetQuery(con, "SET NAMES 'utf8'")
  return(con)
}

#' Escribir (sobrescribir) un data.frame en la base de datos
#'
#' Escribe un data.frame en la tabla especificada. Si la tabla existe, se sobrescribe.
#'
#' @param df Un data.frame o tibble que se va a escribir en la base de datos.
#' @param tabla Nombre de la tabla destino (carácter).
#' @return Invisiblemente TRUE si la operación tiene éxito (comportamiento de dbWriteTable).
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3, y = letters[1:3])
#' EscribirDatos(df, "mi_tabla")
#' }
#' @export
#' @importFrom DBI dbDisconnect dbWriteTable
EscribirDatos <- function(df, tabla) {
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  invisible(DBI::dbWriteTable(con, tabla, df, row.names = FALSE, overwrite = TRUE, encoding = "latin1"))
}

#' Agregar (append) un data.frame a una tabla existente
#'
#' Añade las filas de un data.frame a una tabla ya existente en la base de datos.
#'
#' @param df Un data.frame o tibble con las filas a añadir.
#' @param tabla Nombre de la tabla destino (carácter).
#' @return Invisiblemente TRUE si la operación tiene éxito (comportamiento de dbWriteTable).
#' @examples
#' \dontrun{
#' df_nuevos <- data.frame(x = 4:6, y = letters[4:6])
#' AgregarDatos(df_nuevos, "mi_tabla")
#' }
#' @export
#' @importFrom DBI dbDisconnect dbWriteTable
AgregarDatos <- function(df, tabla) {
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  invisible(DBI::dbWriteTable(con, tabla, df, row.names = FALSE, append = TRUE, encoding = "latin1"))
}

#' Cargar datos desde una tabla de la base de datos
#'
#' Recupera filas de la tabla indicada. Se puede pasar una condición WHERE opcional.
#'
#' @param tabla Nombre de la tabla a leer (carácter).
#' @param condicion (opc.) Cadena con la cláusula WHERE (sin la palabra WHERE). Ej: "fecha >= '2025-01-01'".
#' @return Un data.frame con las filas resultantes de la consulta.
#' @examples
#' \dontrun{
#' df <- CargarDatos("mi_tabla")
#' df_filtrado <- CargarDatos("mi_tabla", "x > 10")
#' }
#' @export
#' @importFrom DBI dbDisconnect dbGetQuery
CargarDatos <- function(tabla, condicion = NULL) {
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))

  consulta <- paste("SELECT * FROM", tabla)
  if (!is.null(condicion)) {
    consulta <- paste(consulta, "WHERE", condicion)
  }

  DBI::dbGetQuery(con, consulta)
}

#' Ejecutar una consulta SQL arbitraria
#'
#' Ejecuta una consulta SQL y retorna el resultado como data.frame.
#'
#' @param consulta Cadena con la consulta SQL a ejecutar.
#' @return Un data.frame con el resultado de la consulta.
#' @examples
#' \dontrun{
#' resultado <- Consulta("SELECT COUNT(*) AS n FROM mi_tabla")
#' }
#' @export
#' @importFrom DBI dbDisconnect dbGetQuery
Consulta <- function(consulta) {
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, consulta)
}

# Función interna que calcula tablas auxiliares y recodifica según el criterio
.top_auxiliar <- function(datos, var_recode, var_top, fun_Top, criterio, tipo, nom_var, lab_recodificar) {
  by_var <- rlang::as_name(var_recode)

  if (fun_Top == "n") {
    tot <- nrow(datos)
    aux1 <- datos |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by_var))) |>
      dplyr::summarise(Var = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(Pct = Var / tot)
  } else {
    fun <- match.fun(fun_Top)
    tot <- fun(dplyr::pull(datos, !!var_top), na.rm = TRUE)
    aux1 <- datos |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by_var))) |>
      dplyr::summarise(Var = fun(!!var_top, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Pct = Var / tot)
  }

  aux2 <- aux1 |>
    dplyr::arrange(dplyr::desc(Var)) |>
    dplyr::mutate(Seq = dplyr::row_number(),
                  !!nom_var := dplyr::case_when(
                    tipo == "n" & Seq <= criterio ~ as.character(!!var_recode),
                    tipo == "pct" & Pct > criterio ~ as.character(!!var_recode),
                    TRUE ~ lab_recodificar
                  )) |>
    dplyr::select(dplyr::all_of(by_var), dplyr::all_of(nom_var))

  datos |>
    dplyr::left_join(aux2, by = by_var) |>
    dplyr::mutate(
      !!nom_var := factor(!!rlang::sym(nom_var), levels = unique(aux2[[nom_var]]), ordered = TRUE),
      !!nom_var := forcats::fct_relevel(!!rlang::sym(nom_var), lab_recodificar, after = Inf)
    )
}

#' Recodificación de Categorías Menos Frecuentes
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode Variable que se desea recodificar. Se pasa sin comillas.
#' @param var_top Variable a partir de la cual se calcularÃ¡n las frecuencias o la función de resumen. Se pasa sin comillas.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param n El número mÃ¡ximo de categorías principales a conservar (predeterminado: 10).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @export
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n = 10, nom_var, lab_recodificar = "OTROS") {

  var_recode <- dplyr::enquo(var_recode)
  var_top <- dplyr::enquo(var_top)
  .top_auxiliar(data, var_recode, var_top, fun_Top, n, "n", nom_var, lab_recodificar)
}


#' Recodificación de Categorías Menos Frecuentes (Relativa)
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor relativo o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode Variable que se desea recodificar. Se pasa sin comillas.
#' @param var_top Variable a partir de la cual se calcularÃ¡n las frecuencias o la función de resumen. Se pasa sin comillas.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param pct_min El porcentaje mínimo necesario para considerar una categoría principal (predeterminado: 0.05).
#' @param nom_var El nombre para la nueva variable recodificada.
#' @param lab_recodificar El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
#' @return El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
#' @export
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min = 0.05, nom_var, lab_recodificar = "OTROS") {
  var_recode <- dplyr::enquo(var_recode)
  var_top <- dplyr::enquo(var_top)
  .top_auxiliar(data, var_recode, var_top, fun_Top, pct_min, "pct", nom_var, lab_recodificar)
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
