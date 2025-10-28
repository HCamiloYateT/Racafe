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

#' @title ObtenerTokenAcceso
#' @description Obtiene un token de acceso (client_credentials) para Microsoft Graph API usando las variables de ambiente `MS_CLIENT_ID`, `MS_CLIENT_SECRET` y `MS_TENANT_ID`.
#' @return Cadena con el token de acceso.
#' @examples
#' # token <- ObtenerTokenAcceso()
#' @export
ObtenerTokenAcceso <- function() {
  client_id <- Sys.getenv("MS_CLIENT_ID", unset = NA_character_)
  client_secret <- Sys.getenv("MS_CLIENT_SECRET", unset = NA_character_)
  tenant_id <- Sys.getenv("MS_TENANT_ID", unset = NA_character_)

  faltantes <- c(
    if (is.na(client_id) || identical(client_id, "")) "MS_CLIENT_ID" else NULL,
    if (is.na(client_secret) || identical(client_secret, "")) "MS_CLIENT_SECRET" else NULL,
    if (is.na(tenant_id) || identical(tenant_id, "")) "MS_TENANT_ID" else NULL
  )

  if (length(faltantes) > 0) {
    stop(
      "Faltan variables de ambiente requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  url <- paste0(
    "https://login.microsoftonline.com/",
    tenant_id,
    "/oauth2/v2.0/token"
  )

  payload <- list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret,
    scope = "https://graph.microsoft.com/.default"
  )

  resp <- httr::POST(url, body = payload, encode = "form")
  if (httr::http_error(resp)) {
    stop("Error al obtener token de acceso: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")
  token <- data$access_token
  if (is.null(token)) stop("No se recibió 'access_token' en la respuesta.")
  token
}

#' @title CabecerasGraph
#' @description Construye cabeceras HTTP con el token Bearer para Microsoft Graph.
#' @return Objeto de cabeceras para usar con httr.
#' @examples
#' # headers <- CabecerasGraph()
#' @export
CabecerasGraph <- function() {
  token <- ObtenerTokenAcceso()
  httr::add_headers(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )
}

#' @title ObtenerIdDrive
#' @description Obtiene el ID de OneDrive (drive id) de un usuario de dominio racafe.com.
#' @param usuario Cadena con el alias o correo (sin dominio) del usuario.
#' @return Cadena con el identificador de la unidad (drive id) del usuario.
#' @examples
#' # drive_id <- ObtenerIdDrive("hcyate")
#' @export
ObtenerIdDrive <- function(usuario) {
  stopifnot(is.character(usuario), length(usuario) == 1)
  headers <- CabecerasGraph()
  url <- paste0("https://graph.microsoft.com/v1.0/users/", usuario, "@racafe.com/drive")
  resp <- httr::GET(url, headers)
  if (httr::http_error(resp)) {
    stop("Error al obtener el ID de la unidad: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")
  id <- data$id
  if (is.null(id)) stop("No se encontró 'id' de la unidad para el usuario: ", usuario)
  id
}

#' @title CargarExcelDesdeOneDrive
#' @description Descarga un archivo Excel desde OneDrive y lo abre como objeto de openxlsx2::wb_load sin persistir el archivo.
#' @param usuario Alias del usuario sin dominio (ej. "juan.perez").
#' @param ruta Ruta dentro de OneDrive donde se encuentra el archivo.
#' @param archivo Nombre del archivo Excel (con extensión).
#' @return Objeto workbook de openxlsx2.
#' @examples
#' # wb <- CargarExcelDesdeOneDrive("juan.perez", "Carpeta/Reportes", "informe.xlsx")
#' @export
CargarExcelDesdeOneDrive <- function(usuario, ruta, archivo) {
  stopifnot(is.character(usuario), is.character(ruta), is.character(archivo))
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)

  # Construcción de URL cuidando codificación por segmentos
  path_full <- paste(ruta, archivo, sep = "/")
  url <- paste0(
    "https://graph.microsoft.com/v1.0/drives/",
    drive_id, "/root:/",
    utils::URLencode(path_full, reserved = TRUE),
    ":/content"
  )

  resp <- httr::GET(url, headers)
  if (httr::status_code(resp) == 200) {
    tmp <- tempfile(fileext = ".xlsx")
    on.exit({
      if (file.exists(tmp)) file.remove(tmp)
    }, add = TRUE)
    writeBin(httr::content(resp, "raw"), tmp)
    wb <- openxlsx2::wb_load(tmp)
    return(wb)
  } else {
    stop("Error al descargar archivo: ",
         httr::status_code(resp), " - ",
         httr::content(resp, "text"))
  }
}

#' @title DescargarExcelDesdeOneDrive
#' @description Descarga un archivo Excel desde OneDrive y lo guarda localmente con el nombre indicado.
#' @param usuario Alias del usuario sin dominio.
#' @param ruta Ruta dentro de OneDrive.
#' @param archivo Nombre del archivo en OneDrive.
#' @param nombre_salida Nombre base del archivo de salida (sin extensión).
#' @return TRUE si la descarga fue exitosa, en caso contrario lanza error.
#' @examples
#' # ok <- DescargarExcelDesdeOneDrive("juan.perez", "Carpeta/Reportes", "informe.xlsx", "informe_local")
#' @export
DescargarExcelDesdeOneDrive <- function(usuario, ruta, archivo, nombre_salida) {
  stopifnot(is.character(usuario), is.character(ruta), is.character(archivo), is.character(nombre_salida))
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)

  path_full <- paste(ruta, archivo, sep = "/")
  url <- paste0(
    "https://graph.microsoft.com/v1.0/drives/",
    drive_id, "/root:/",
    utils::URLencode(path_full, reserved = TRUE),
    ":/content"
  )

  resp <- httr::GET(url, headers)
  if (httr::status_code(resp) == 200) {
    out <- paste0(nombre_salida, ".xlsx")
    writeBin(httr::content(resp, "raw"), out)
    return(TRUE)
  } else {
    stop("Error al descargar archivo: ",
         httr::status_code(resp), " - ",
         httr::content(resp, "text"))
  }
}

#' @title ListarCarpetas
#' @description Lista el nombre de las carpetas en la raíz del OneDrive del usuario.
#' @param usuario Alias del usuario sin dominio.
#' @return Vector de caracteres con nombres de carpetas.
#' @examples
#' # carpetas <- ListarCarpetas("juan.perez")
#' @export
ListarCarpetas <- function(usuario) {
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/root/children")
  resp <- httr::GET(url, headers)
  if (httr::http_error(resp)) {
    stop("Error al obtener carpetas: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")
  folders <- vapply(
    data$value,
    function(it) if (!is.null(it$folder)) it$name else NA_character_,
    FUN.VALUE = character(1)
  )
  stats::na.omit(folders)
}

#' @title ObtenerIdCarpeta
#' @description Obtiene el ID de una carpeta en la raíz del OneDrive del usuario por su nombre.
#' @param usuario Alias del usuario sin dominio.
#' @param nombre_carpeta Nombre exacto de la carpeta.
#' @return Cadena con el ID de la carpeta si existe; en caso contrario, error.
#' @examples
#' # carpeta_id <- ObtenerIdCarpeta("juan.perez", "Reportes")
#' @export
ObtenerIdCarpeta <- function(usuario, nombre_carpeta) {
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/root/children")
  resp <- httr::GET(url, headers)
  if (httr::http_error(resp)) {
    stop("Error al obtener carpetas: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")
  for (item in data$value) {
    if (!is.null(item$folder) && isTRUE(item$name == nombre_carpeta)) {
      return(item$id)
    }
  }
  stop("Carpeta no encontrada: ", nombre_carpeta)
}

#' @title ListarContenidoCarpetaNombre
#' @description Lista los elementos (archivos y carpetas) dentro de una carpeta especificada por nombre en la raíz.
#' @param usuario Alias del usuario sin dominio.
#' @param nombre_carpeta Nombre de la carpeta.
#' @return Tibble con columnas: name, type, id.
#' @examples
#' @export
#' # df <- ListarContenidoCarpetaNombre("juan.perez", "Reportes")
ListarContenidoCarpetaNombre <- function(usuario, nombre_carpeta) {
  carpeta_id <- ObtenerIdCarpeta(usuario, nombre_carpeta)
  ListarContenidoCarpetaId(usuario, carpeta_id)
}

#' @title ListarContenidoCarpetaId
#' @description Lista los elementos (archivos y carpetas) dentro de una carpeta (por ID).
#' @param usuario Alias del usuario sin dominio.
#' @param carpeta_id ID de la carpeta.
#' @return Tibble con columnas: name, type, id.
#' @examples
#' # df <- ListarContenidoCarpetaId("juan.perez", "0123ABC...")
#' @export
ListarContenidoCarpetaId <- function(usuario, carpeta_id) {
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", carpeta_id, "/children")
  resp <- httr::GET(url, headers)
  if (httr::http_error(resp)) {
    stop("Error al listar contenido de carpeta: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")

  elements <- lapply(data$value, function(item) {
    list(
      name = item$name,
      type = if (!is.null(item$folder)) "Folder" else "File",
      id   = item$id
    )
  })

  tibble::as_tibble(do.call(rbind, lapply(elements, as.data.frame)))
}

#' @title ListarContenidoCarpetaRecursivo
#' @description Lista recursivamente todos los archivos dentro de una carpeta (por ID) incluyendo sus rutas relativas.
#' @param usuario Alias del usuario sin dominio.
#' @param carpeta_id ID de la carpeta raíz a explorar.
#' @param ruta_padre Ruta relativa acumulada (para uso interno).
#' @return Lista de listas con elementos: name, type = "File", id, path.
#' @examples
#' # lst <- ListarContenidoCarpetaRecursivo("juan.perez", "0123ABC...")
#' @export
ListarContenidoCarpetaRecursivo <- function(usuario, carpeta_id, ruta_padre = "") {
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", carpeta_id, "/children")
  resp <- httr::GET(url, headers)
  if (httr::http_error(resp)) {
    stop("Error al listar contenido de carpeta: ", httr::content(resp, as = "text"))
  }
  data <- httr::content(resp, as = "parsed", type = "application/json")

  elementos <- list()
  for (item in data$value) {
    ruta_item <- file.path(ruta_padre, item$name)
    if (!is.null(item$folder)) {
      sub <- ListarContenidoCarpetaRecursivo(usuario, item$id, ruta_item)
      elementos <- append(elementos, sub)
    } else {
      elementos <- append(elementos, list(list(
        name = item$name,
        type = "File",
        id   = item$id,
        path = ruta_item
      )))
    }
  }
  elementos
}

#' @title ListarTodoContenidoCarpeta
#' @description Retorna un tibble con todos los archivos dentro de una carpeta (por ID) de manera recursiva.
#' @param usuario Alias del usuario sin dominio.
#' @param carpeta_id ID de la carpeta raíz.
#' @return Tibble con columnas: name, type, id, path.
#' @examples
#' # df <- ListarTodoContenidoCarpeta("juan.perez", "0123ABC...")
#' @export
ListarTodoContenidoCarpeta <- function(usuario, carpeta_id) {
  elementos <- ListarContenidoCarpetaRecursivo(usuario, carpeta_id)
  tibble::as_tibble(do.call(rbind, lapply(elementos, as.data.frame)))
}

#' @title DescargarArchivoId
#' @description Descarga el contenido de un archivo (por ID) de OneDrive a un archivo temporal y devuelve su ruta.
#' @param archivo_id ID del archivo en OneDrive.
#' @param usuario Alias del usuario sin dominio.
#' @return Ruta del archivo temporal creado.
#' @examples
#' # tmp <- DescargarArchivoId("ABC123...", "juan.perez")
#' @export
DescargarArchivoId <- function(archivo_id, usuario) {
  headers <- CabecerasGraph()
  drive_id <- ObtenerIdDrive(usuario)
  url <- paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", archivo_id, "/content")

  tmp <- tempfile(fileext = ".xlsx")
  resp <- httr::GET(url, headers, httr::write_disk(tmp, overwrite = TRUE))
  if (httr::http_error(resp)) {
    # Limpieza si falló
    if (file.exists(tmp)) unlink(tmp)
    stop("Error al descargar archivo: ", httr::content(resp, as = "text"))
  }
  tmp
}

#' @title ListarHojasExcelOneDrive
#' @description Descarga un archivo Excel (por ID) y retorna el vector de nombres de hojas.
#' @param archivo_id ID del archivo en OneDrive.
#' @param usuario Alias del usuario sin dominio.
#' @return Vector de caracteres con los nombres de hojas del Excel.
#' @examples
#' # tmp <- DescargarArchivoId("ABC123...", "juan.perez")
#' # hojas <- ListarHojasExcelOneDrive("ABC123...", "juan.perez")
#' @export
ListarHojasExcelOneDrive <- function(archivo_id, usuario) {
  ruta <- DescargarArchivoId(archivo_id, usuario)
  on.exit({ if (file.exists(ruta)) file.remove(ruta) }, add = TRUE)
  readxl::excel_sheets(ruta)
}

#' @title LeerExcelDesdeOneDrive
#' @description Descarga un archivo Excel (por ID) y lo carga con readxl::read_excel con los argumentos adicionales provistos.
#' @param archivo_id ID del archivo en OneDrive.
#' @param usuario Alias del usuario sin dominio.
#' @param ... Argumentos adicionales para readxl::read_excel.
#' @return Un tibble/data.frame con los datos leídos.
#' @examples
#' # tmp <- DescargarArchivoId("ABC123...", "juan.perez")
#' # df <- LeerExcelDesdeOneDrive("ABC123...", "juan.perez", sheet = "Datos", skip = 1)
#' @export
LeerExcelDesdeOneDrive <- function(archivo_id, usuario, ...) {
  ruta <- DescargarArchivoId(archivo_id, usuario)
  on.exit({ if (file.exists(ruta)) file.remove(ruta) }, add = TRUE)
  readxl::read_excel(ruta, ...)
}

# Función interna que calcula tablas auxiliares y recodifica según el criterio
.resolve_column_quosure <- function(var, datos) {
  if (rlang::quo_is_symbol(var)) {
    return(var)
  }

  valor <- tryCatch(rlang::eval_tidy(var), error = function(...) NULL)

  if (rlang::is_string(valor) && length(valor) == 1 && valor %in% names(datos)) {
    return(rlang::new_quosure(rlang::sym(valor), env = rlang::empty_env()))
  }

  var
}

.top_auxiliar <- function(datos, var_recode, var_top, fun_Top, criterio, tipo, nom_var, lab_recodificar) {
  var_recode <- .resolve_column_quosure(var_recode, datos)
  var_top <- .resolve_column_quosure(var_top, datos)

  by_var <- names(dplyr::select(datos, !!var_recode))

  if (length(by_var) != 1) {
    rlang::abort("`var_recode` debe hacer referencia exactamente a una columna del conjunto de datos.")
  }

  var_sym <- rlang::sym(by_var)
  var_top_name <- rlang::as_name(var_top)
  nom_var_sym <- rlang::sym(nom_var)

  if (fun_Top == "n") {
    tot <- nrow(datos)
    aux1 <- datos |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by_var))) |>
      dplyr::summarise(Var = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(Pct = Var / tot)
  } else {
    fun <- match.fun(fun_Top)
    tot <- fun(datos[[var_top_name]], na.rm = TRUE)
    aux1 <- datos |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by_var))) |>
      dplyr::summarise(Var = fun(rlang::.data[[var_top_name]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Pct = Var / tot)
  }

  aux2 <- aux1 |>
    dplyr::arrange(dplyr::desc(Var)) |>
    dplyr::mutate(Seq = dplyr::row_number(),
                  !!nom_var_sym := dplyr::case_when(
                    tipo == "n" & Seq <= criterio ~ as.character(!!var_sym),
                    tipo == "pct" & Pct > criterio ~ as.character(!!var_sym),
                    TRUE ~ lab_recodificar
                  )) |>
    dplyr::select(dplyr::all_of(by_var), dplyr::all_of(nom_var))

  datos |>
    dplyr::left_join(aux2, by = by_var) |>
    dplyr::mutate(
      !!nom_var_sym := factor(!!nom_var_sym, levels = unique(aux2[[nom_var]]), ordered = TRUE),
      !!nom_var_sym := forcats::fct_relevel(!!nom_var_sym, lab_recodificar, after = Inf)
    )
}

#' Recodificación de Categorías Menos Frecuentes
#'
#' Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
#'
#' @param data El conjunto de datos en el cual se encuentra la variable a recodificar.
#' @param var_recode Variable que se desea recodificar. Se pasa sin comillas.
#' @param var_top Variable a partir de la cual se calcularán las frecuencias o la función de resumen. Se pasa sin comillas.
#' @param fun_Top La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
#' @param n El número máximo de categorías principales a conservar (predeterminado: 10).
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
#' @param var_top Variable a partir de la cual se calcularán las frecuencias o la función de resumen. Se pasa sin comillas.
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
#' data frame. Los valores `NULL` o que no sean data frame se ignoran, y los
#' data frames vacíos se omiten para evitar errores al realizar la unión.
#'
#' @param ... Objetos a combinar. Solo se utilizan los data frames con filas;
#'            los valores `NULL` y objetos no válidos se ignoran.
#'
#' @return Un data frame que resulta de la combinación de los data frames
#'         proporcionados. Si no hay data frames válidos o no vacíos, se
#'         devuelve un data frame vacío.
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = letters[1:3])
#' df2 <- data.frame(a = numeric(0), b = character(0))  # Data frame vacío
#' df3 <- data.frame(a = 4:5, b = letters[4:5])
#'
#' # Combina df1 y df3, ignorando df2 y otros valores no válidos
#' result <- bind_rows_na(df1, df2, df3, NULL, "texto")
#'
#' @export
bind_rows_na <- function(...) {
  # Crear una lista de objetos a partir de los argumentos
  df_list <- list(...)

  # Conservar únicamente los data frames no vacíos
  df_list <- Filter(is.data.frame, df_list)
  df_list <- Filter(NROW, df_list)

  # Combinar los data frames filtrados
  dplyr::bind_rows(df_list)
}
