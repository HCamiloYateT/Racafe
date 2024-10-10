#' Consulta a una base de datos SQL Server
#'
#' Esta función realiza una consulta a una base de datos SQL Server utilizando ODBC.
#' Se conecta a diferentes bases de datos dependiendo del valor del parámetro `base`,
#' ejecuta una consulta SQL y limpia los nombres de las columnas en el dataframe resultante.
#'
#' @param base Una cadena de texto que especifica el nombre de la base de datos.
#'            Puede ser uno de los siguientes valores: "syscafe", "cafesys" o "estad".
#'            Se asignará el nombre correspondiente en la conexión.
#' @param uid El nombre de usuario para la conexión a la base de datos.
#' @param pwd La contraseña del usuario para la conexión.
#' @param query La consulta SQL a ejecutar en la base de datos.
#'
#' @return Un dataframe con los resultados de la consulta, con los nombres de las columnas limpiados.
#' @export
#' Consulta a una base de datos SQL Server y limpia los nombres de las columnas
#'
#' Esta función realiza una consulta a una base de datos SQL Server utilizando ODBC.
#' Dependiendo del valor del parámetro `base`, se conecta a una base de datos específica,
#' ejecuta una consulta SQL, y limpia los nombres de las columnas en el dataframe resultante.
#'
#' @param base Una cadena que especifica la base de datos a la que conectarse. Puede ser
#'             uno de los siguientes valores: "syscafe", "cafesys", o "estad".
#'             El valor correspondiente se traduce al nombre real de la base de datos.
#' @param uid El nombre de usuario para la conexión a la base de datos.
#' @param pwd La contraseña del usuario para la conexión.
#' @param query La consulta SQL que se ejecutará en la base de datos.
#'
#' @return Un dataframe con los resultados de la consulta y los nombres de las columnas limpiados.
#' @export
ConsultaSistema <- function(base, uid, pwd, query) {

  # Cargar las librerías necesarias
  require(DBI)       # Para la conexión y manejo de bases de datos
  require(tidyverse) # Para manipulación de datos (dplyr, tidyr, etc.)

  # Asigna el nombre de la base de datos en función del valor de `base`
  base <- case_when(
    base == "syscafe" ~ "ContabRacafe",
    base == "cafesys" ~ "Cafesys",
    base == "estad" ~ "EstadRacafe",
    TRUE ~ stop("Base de datos no válida") # Agregar control de errores si el valor de `base` es incorrecto
  )

  # Establece la conexión con la base de datos SQL Server
  con <- dbConnect(odbc::odbc(),
                   Driver = "ODBC Driver 18 for SQL Server",
                   Server = "172.16.19.21",
                   Database = base,
                   uid = uid,
                   pwd = pwd,
                   port = 1433,
                   TrustServerCertificate = "yes")

  # Ejecuta la consulta SQL y limpia los nombres de las columnas usando la función LimpiarNombres
  df <- dbGetQuery(con, query) %>%
    mutate(across(where(is.character), racafe::LimpiarNombres))

  # Cierra la conexión con la base de datos
  dbDisconnect(con)

  # Retorna el dataframe con los resultados
  return(df)
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
#' @import dplyr forcats rlang
#' @export
TopAbsoluto <- function(data, var_recode, var_top, fun_Top, n=10, nom_var, lab_recodificar = "OTROS"){

  require(rlang)
  require(forcats)
  datos = data

  # Calcula las frecuencias o las estadísticas según la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos %>%
      mutate(Tot = n()) %>%
      group_by_at(var_recode) %>%
      summarise(Var = n(),
                Pct = Var/unique(Tot))
  } else {
    aux1 <- datos %>%
      mutate(Tot = !!parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) %>%
      group_by_at(var_recode) %>%
      summarise(Var = !!parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                Pct = Var/unique(Tot))
  }

  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 %>%
    arrange(desc(Var)) %>%
    mutate(Seq = row_number(),
           !!nom_var := ifelse(Seq <= n, as.character(!!parse_expr(var_recode)), lab_recodificar)
    ) %>%
    select(all_of(var_recode), all_of(nom_var))

  # Recodifica las categorías en el dataset original
  data <- datos %>%
    left_join(aux2, by = var_recode) %>%
    mutate(!!nom_var := factor(!!sym(nom_var), levels = c(unique(aux2[[nom_var]])), ordered = TRUE),
           !!nom_var := fct_relevel(!!sym(nom_var), lab_recodificar, after = Inf))  # Asegura que 'lab_recodificar' está al final

  return(data)
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
#' @import dplyr forcats rlang
#' @export
TopRelativo <- function(data, var_recode, var_top, fun_Top, pct_min=0.05, nom_var, lab_recodificar = "OTROS") {

  datos = data

  # Calcula las frecuencias o estadísticas relativas de acuerdo a la función proporcionada
  if (fun_Top == "n"){
    aux1 <- datos %>%
      mutate(Tot = n()) %>%
      group_by_at(var_recode) %>%
      summarise(Var = n(),
                Pct = Var / unique(Tot))
  } else {
    aux1 <- datos %>%
      mutate(Tot = !!parse_expr(paste(fun_Top, "(", var_top, ", na.rm = TRUE)"))) %>%
      group_by_at(var_recode) %>%
      summarise(Var = !!parse_expr(paste0(fun_Top, "(", var_top, ", na.rm = TRUE)")),
                Pct = Var / unique(Tot))
  }

  # Organiza los datos, recodifica las categorías menos frecuentes
  aux2 <- aux1 %>%
    arrange(desc(Var)) %>%
    mutate(Seq = row_number(),
           !!nom_var := !!parse_expr(paste0("ifelse(Pct > pct_min, as.character(",
                                            var_recode, "), '",
                                            lab_recodificar, "')"))) %>%
    select(all_of(var_recode), all_of(nom_var))

  # Recodifica las categorías en el dataset original
  data <- datos %>%
    left_join(aux2, by = var_recode) %>%
    mutate(!!nom_var := factor(!!sym(nom_var), levels = c(unique(aux2[[nom_var]])), ordered = TRUE),
           !!nom_var := fct_relevel(!!sym(nom_var), lab_recodificar, after = Inf))  # Asegura que 'lab_recodificar' está al final

  return(data)
}

#' AdicionarBotonDetalle
#'
#' Añade un botón con un ícono de lupa a una tabla para abrir un detalle interactivo.
#' @param tabla Un data frame o tibble. La tabla a la cual se le añadirá el botón de detalle.
#' @return La misma tabla con una nueva columna `Detalle` que contiene un botón interactivo en HTML.
#' @details La función agrega una columna con un ícono de lupa (`&#128270;`) como botón, que puede ser usado para desplegar más información o detalles de un registro específico de la tabla. La función solo realiza modificaciones si la tabla tiene más de una fila.
#' @examples
#' \dontrun{
#' # Crear una tabla de ejemplo
#' tabla <- data.frame(ID = 1:3, Nombre = c("Juan", "Ana", "Luis"))
#'
#' # Adicionar el botón de detalle
#' tabla_con_boton <- AdicionarBotonDetalle(tabla)
#' print(tabla_con_boton)
#' }
#' @import dplyr
#' @importFrom htmltools HTML
#' @export
AdicionarBotonDetalle <- function(tabla) {
  # Requiere que la tabla no sea nula y tenga filas
  req(tabla)

  # Solo añadir el botón si la tabla tiene filas
  if (nrow(tabla) > 0) {
    # Añadir la columna 'Detalle' con un ícono de lupa (HTML)
    tabla %>%
      mutate(Detalle = htmltools::HTML(paste0(
        "<span title='Abrir Detalle' style='cursor:pointer'>&#128270;</span>"
      )))
  } else {
    # Retornar la tabla sin modificar si no tiene filas
    return(tabla)
  }
}
