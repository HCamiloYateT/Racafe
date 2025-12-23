#' Consulta a una base de datos SQL Server.
#'
#' Esta función se conecta a una base de datos SQL Server y ejecuta una consulta SQL.
#' Dependiendo del valor del parámetro `bd`, se selecciona la base de datos correspondiente.
#' Después de ejecutar la consulta, limpia los nombres de las columnas en el dataframe resultante
#' y convierte a fecha las variables que empiezan por "Fec" o se llaman exactamente "Fecha".
#'
#' @param bd Una cadena de texto que especifica el nombre de la base de datos a la que conectarse.
#'           Puede ser uno de los siguientes valores: "syscafe", "cafesys" o "estad".
#' @param query La consulta SQL que se ejecutará en la base de datos.
#' @param uid El nombre de usuario para la conexión a la base de datos.
#' @param pwd La contraseña del usuario para la conexión.
#' @param server La dirección del servidor SQL Server. Por defecto "172.16.19.21".
#' @param port El puerto del servidor SQL Server. Por defecto 1433.
#'
#' @return Un dataframe con los resultados de la consulta, con los nombres de las columnas limpiados.
#' @export

ConsultaSistema <- function(bd, query, uid = Sys.getenv("SYS_UID"), pwd = Sys.getenv("SYS_PWD"), server = "172.16.19.21", port = 1433) {

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

# Valida que un argumento sea un carácter escalar no vacío.
validar_cadena_scalar <- function(valor, nombre) {
  if (!is.character(valor) || length(valor) != 1 || is.na(valor) || valor == "") {
    stop("`", nombre, "` debe ser una cadena de texto única y no vacía.")
  }
  invisible(TRUE)
}

#' @title ObtenerIdSite
#' @description Obtiene el identificador de un sitio de SharePoint usando Microsoft Graph.
#' @param hostname Hostname del tenant (por ejemplo, "contoso.sharepoint.com").
#' @param site_path Ruta relativa del sitio en SharePoint (por ejemplo, "sites/mi-sitio").
#' @return Cadena con el ID del sitio.
#' @examples
#' \dontrun{
#' site_id <- ObtenerIdSite("contoso.sharepoint.com", "sites/mi-sitio")
#' }
#' @references
#' https://learn.microsoft.com/graph/api/site-get
#' @export
ObtenerIdSite <- function(hostname, site_path) {
  validar_cadena_scalar(hostname, "hostname")
  validar_cadena_scalar(site_path, "site_path")

  headers <- CabecerasGraph()
  site_path <- gsub("^/+", "", trimws(site_path))
  site_path <- utils::URLencode(site_path, reserved = TRUE)

  url <- paste0("https://graph.microsoft.com/v1.0/sites/", hostname, ":/", site_path)
  resp <- httr::GET(url, headers)

  if (httr::http_error(resp)) {
    stop(
      "Error al obtener id del site: ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }

  data <- httr::content(resp, as = "parsed", type = "application/json")
  if (is.null(data$id)) {
    stop("No se encontró `id` en la respuesta del sitio.")
  }

  data$id
}

#' @title ObtenerIdDriveSite
#' @description Obtiene el ID de una unidad (drive) asociada a un sitio de SharePoint.
#' @param site_id ID del sitio (site id) en Microsoft Graph.
#' @param nombre_drive Nombre del drive (opcional). Si es `NULL`, retorna el primero disponible.
#' @return Cadena con el ID del drive encontrado.
#' @examples
#' \dontrun{
#' drive_id <- ObtenerIdDriveSite("site-id-123")
#' drive_id <- ObtenerIdDriveSite("site-id-123", nombre_drive = "Documentos compartidos")
#' }
#' @references
#' https://learn.microsoft.com/graph/api/site-list-drives
#' @export
ObtenerIdDriveSite <- function(site_id, nombre_drive = NULL) {
  validar_cadena_scalar(site_id, "site_id")
  if (!is.null(nombre_drive)) {
    validar_cadena_scalar(nombre_drive, "nombre_drive")
  }

  headers <- CabecerasGraph()
  url <- paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "/drives")
  resp <- httr::GET(url, headers)

  if (httr::http_error(resp)) {
    stop(
      "Error al obtener drives: ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }

  data <- httr::content(resp, as = "parsed", type = "application/json")
  if (is.null(data$value) || length(data$value) == 0) {
    stop("El sitio no contiene drives disponibles.")
  }

  if (is.null(nombre_drive)) {
    return(data$value[[1]]$id)
  }

  drive_names <- vapply(data$value, `[[`, character(1), "name")
  nombre_drive_lower <- tolower(nombre_drive)
  matches <- tolower(drive_names) == nombre_drive_lower |
    grepl(nombre_drive_lower, tolower(drive_names), fixed = TRUE)

  if (!any(matches)) {
    stop(
      "Drive no encontrado. Drives disponibles: ",
      paste(drive_names, collapse = ", ")
    )
  }

  data$value[[which(matches)[1]]]$id
}

#' @title ListarDriveRecursivo
#' @description Lista recursivamente los archivos de un drive de SharePoint/OneDrive con metadatos.
#' @param drive_id ID del drive.
#' @param item_id ID del item (por defecto "root").
#' @param ruta Ruta relativa acumulada (uso interno).
#' @param fecha_desde Fecha mínima de modificación (Date o POSIXct); si se define,
#'   se omiten archivos con fecha anterior.
#' @return Tibble con metadatos de los archivos encontrados.
#' @examples
#' \dontrun{
#' archivos <- ListarDriveRecursivo("drive-id-123")
#' archivos <- ListarDriveRecursivo("drive-id-123", fecha_desde = Sys.Date() - 30)
#' }
#' @references
#' https://learn.microsoft.com/graph/api/driveitem-list-children
#' @export
ListarDriveRecursivo <- function(drive_id, item_id = "root", ruta = "", fecha_desde = NULL) {
  validar_cadena_scalar(drive_id, "drive_id")
  validar_cadena_scalar(item_id, "item_id")

  if (!is.null(fecha_desde)) {
    if (inherits(fecha_desde, "Date")) {
      fecha_desde <- as.POSIXct(fecha_desde, tz = "UTC")
    }
    if (!inherits(fecha_desde, "POSIXct")) {
      stop("`fecha_desde` debe ser Date o POSIXct.")
    }
  }

  headers <- CabecerasGraph()
  base_url <- if (item_id == "root") {
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/root/children")
  } else {
    paste0("https://graph.microsoft.com/v1.0/drives/", drive_id, "/items/", item_id, "/children")
  }

  obtener_items <- function(url_actual) {
    items <- list()
    while (!is.null(url_actual)) {
      resp <- httr::GET(url_actual, headers)
      if (httr::http_error(resp)) {
        stop(
          "Error al listar contenido: ",
          httr::content(resp, as = "text", encoding = "UTF-8")
        )
      }
      data <- httr::content(resp, as = "parsed", type = "application/json")
      if (!is.null(data$value) && length(data$value) > 0) {
        items <- c(items, data$value)
      }
      url_actual <- data$`@odata.nextLink`
    }
    items
  }

  items <- obtener_items(base_url)
  if (length(items) == 0) {
    return(tibble::tibble())
  }

  extraer_nombre_usuario <- function(x) {
    if (is.null(x$user$displayName)) NA_character_ else x$user$displayName
  }

  salida <- list()
  for (it in items) {
    ruta_actual <- if (ruta == "") it$name else file.path(ruta, it$name)

    if (!is.null(it$folder)) {
      hijos <- ListarDriveRecursivo(
        drive_id = drive_id,
        item_id = it$id,
        ruta = ruta_actual,
        fecha_desde = fecha_desde
      )
      if (nrow(hijos) > 0) {
        salida[[length(salida) + 1]] <- hijos
      }
      next
    }

    fecha_mod <- as.POSIXct(it$lastModifiedDateTime, tz = "UTC")
    if (!is.null(fecha_desde) && !is.na(fecha_mod) && fecha_mod < fecha_desde) {
      next
    }

    extension <- tools::file_ext(it$name)
    extension <- ifelse(extension == "", NA_character_, extension)

    salida[[length(salida) + 1]] <- tibble::tibble(
      id = it$id,
      nombre = it$name,
      extension = extension,
      ruta = ruta_actual,
      tamaño_bytes = if (!is.null(it$size)) as.numeric(it$size) else NA_real_,
      fecha_creacion = as.POSIXct(it$createdDateTime, tz = "UTC"),
      fecha_modificacion = fecha_mod,
      creado_por = extraer_nombre_usuario(it$createdBy),
      modificado_por = extraer_nombre_usuario(it$lastModifiedBy)
    )
  }

  if (length(salida) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(salida)
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

#' @title CargarExcelSite
#' @description Descarga un archivo Excel desde SharePoint/OneDrive (por drive/item)
#'   y lo carga con readxl sin persistir el archivo.
#' @param drive_id ID del drive en Microsoft Graph.
#' @param item_id ID del archivo (item) en Microsoft Graph.
#' @param hoja Hoja a leer (nombre o índice). Si es `NULL`, se usa la hoja por defecto.
#' @param ... Argumentos adicionales para readxl::read_excel.
#' @return Un tibble/data.frame con los datos leídos.
#' @examples
#' \dontrun{
#' datos <- CargarExcelSite(
#'   drive_id = "drive-id-123",
#'   item_id = "item-id-456",
#'   hoja = "Datos",
#'   skip = 1
#' )
#' }
#' @references
#' https://learn.microsoft.com/graph/api/driveitem-get-content
#' @export
CargarExcelSite <- function(drive_id, item_id, hoja = NULL, ...) {
  validar_cadena_scalar(drive_id, "drive_id")
  validar_cadena_scalar(item_id, "item_id")
  if (!is.null(hoja) &&
    !(is.character(hoja) && length(hoja) == 1 && !is.na(hoja) && hoja != "") &&
    !(is.numeric(hoja) && length(hoja) == 1 && !is.na(hoja))) {
    stop("`hoja` debe ser un nombre (carácter) o índice (numérico) de hoja válido.")
  }

  headers <- CabecerasGraph()
  url <- paste0(
    "https://graph.microsoft.com/v1.0/drives/",
    drive_id,
    "/items/",
    item_id,
    "/content"
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit({ if (file.exists(tmp)) unlink(tmp) }, add = TRUE)

  resp <- httr::GET(url, headers, httr::write_disk(tmp, overwrite = TRUE))

  if (httr::http_error(resp)) {
    stop(
      "Error al descargar archivo: ",
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  }

  if (is.null(hoja)) {
    readxl::read_excel(tmp, ...)
  } else {
    readxl::read_excel(tmp, sheet = hoja, ...)
  }
}

TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, nom_var, lab_recodificar = "OTROS"){
  # Descripción: Recodifica las categorías menos frecuentes de una variable según su valor absoluto o una función de resumen y las agrupa en una nueva categoría.
  # Parámetros:
  #   - data: El conjunto de datos en el cual se encuentra la variable a recodificar.
  #   - var_recode: El nombre de la variable que se desea recodificar.
  #   - var_top: El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
  #   - fun_Top: La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
  #   - n: (Opcional) El número máximo de categorías principales a conservar (predeterminado: 10).
  #   - nom_var: El nombre para la nueva variable recodificada.
  #   - lab_recodificar: (Opcional) El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
  # Valor de retorno:
  #   - El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
  
  require(rlang)
  require(forcats)
  datos = data
  
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n(),
                Pct = Var/unique(Tot))
  } 
  else{
    aux1 <- datos %>% 
      mutate(Tot= !!parse_expr(paste(fun_Top, "(", var_top,", na.rm = T)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= !!parse_expr(paste0(fun_Top, "(", var_top,", na.rm = T)")),
                Pct = Var/unique(Tot))
  }

  aux2 <- aux1 %>%
    arrange(desc(Var)) %>%
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Seq<=n, as.character(",
                                            var_recode, "), '",
                                            lab_recodificar, "'", ")"))) %>%
    select(all_of(var_recode), all_of(nom_var))

  data <- datos %>%
    left_join(aux2, by = var_recode) %>% 
    mutate(!!nom_var := !!parse_expr(paste0("factor(", nom_var, ", levels = unique(aux2$",nom_var ,"), ordered = T)")),
           !!nom_var := !!parse_expr(paste0("fct_relevel(", nom_var,", 'OTROS', after = Inf)"))
           )
  return(data)
}
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, nom_var, lab_recodificar = "OTROS"){
  # Descripción: Recodifica las categorías menos frecuentes de una variable según su valor relativo o una función de resumen y las agrupa en una nueva categoría.
  # Parámetros:
  #   - data: El conjunto de datos en el cual se encuentra la variable a recodificar.
  #   - var_recode: El nombre de la variable que se desea recodificar.
  #   - var_top: El nombre de la variable a partir de la cual se calcularán las frecuencias o la función de resumen.
  #   - fun_Top: La función de resumen a aplicar en caso de no utilizar las frecuencias absolutas (por ejemplo, "mean", "sum", etc.).
  #   - pct_min: (Opcional) El porcentaje mínimo necesario para considerar una categoría principal (predeterminado: 0.05).
  #   - nom_var: El nombre para la nueva variable recodificada.
  #   - lab_recodificar: (Opcional) El nombre o etiqueta para las categorías recodificadas (predeterminado: "OTROS").
  # Valor de retorno:
  #   - El conjunto de datos con la variable recodificada según las categorías principales y las categorías recodificadas.
  
  datos = data
  if (fun_Top == "n"){
    aux1 <- datos %>% 
      mutate(Tot = n()) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= n(),
                Pct = Var/unique(Tot))
  } 
  else{
    aux1 <- datos %>% 
      mutate(Tot= !!parse_expr(paste(fun_Top, "(", var_top,", na.rm = T)"))) %>% 
      group_by_at(var_recode) %>% 
      summarise(Var= !!parse_expr(paste0(fun_Top, "(", var_top,", na.rm = T)")),
                Pct = Var/unique(Tot))
  }
  
  aux2 <- aux1 %>% 
    arrange(desc(Var)) %>% 
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Pct>pct_min, as.character(", 
                                            var_recode, "), '", 
                                            lab_recodificar, "'", ")"))) %>% 
    select(all_of(var_recode), all_of(nom_var))
  
  data <- datos %>%
    left_join(aux2, by = var_recode) %>%
    mutate(!!nom_var := !!parse_expr(paste0("factor(", nom_var, ", levels = unique(aux2$",nom_var ,"), ordered = T)")),
           !!nom_var := !!parse_expr(paste0("fct_relevel(", nom_var,", 'OTROS', after = Inf)"))
           )
  return(data)
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
