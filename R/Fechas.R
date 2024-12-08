#' Obtener el Primer Día de una Unidad de Tiempo en una Fecha
#'
#' Esta función calcula el primer día de la unidad temporal especificada (mes, año, etc.) de una fecha dada.
#' Si no se especifica, el valor predeterminado es calcular el primer día del mes.
#'
#' @param x La fecha de la cual se desea obtener el primer día. Puede ser un objeto de clase \code{Date} o \code{POSIXt}.
#' @param uni La unidad de tiempo que se desea usar para el cálculo. Puede ser "month", "year", entre otras. El valor predeterminado es "month".
#' @return Un objeto de clase \code{Date} que representa el primer día de la unidad temporal de la fecha dada.
#' @import lubridate
#' @examples
#' PrimerDia("2023-10-15")  # Devuelve "2023-10-01"
#' PrimerDia(as.Date("2023-05-22"))  # Devuelve "2023-05-01"
#' PrimerDia("2023-10-15", uni = "year")  # Devuelve "2023-01-01"
#' @export
PrimerDia <- function(x, uni = "month") {
  # Verificar si x es una fecha válida y convertir a tipo Date
  require(lubridate)
  x <- lubridate::floor_date(as.Date(x), unit = uni)
  return(x)
}

#' Convertir una Fecha en un Formato de Texto Personalizado
#'
#' Esta función convierte una fecha en un formato de texto personalizado, permitiendo seleccionar qué partes de la fecha mostrar (día, mes, año).
#'
#' @param x Un vector de fechas que se desea convertir.
#' @param dia Un valor lógico que indica si se debe incluir el día (predeterminado: TRUE).
#' @param dia_nombre Un valor lógico que indica si se debe incluir el nombre completo del día (predeterminado: TRUE).
#' @param dia_nom_abr Un valor lógico que indica si se debe incluir el nombre abreviado del día (predeterminado: TRUE).
#' @param mes Un valor lógico que indica si se debe incluir el mes (predeterminado: TRUE).
#' @param mes_abr Un valor lógico que indica si se debe incluir el nombre abreviado del mes (predeterminado: TRUE).
#' @param anho Un valor lógico que indica si se debe incluir el año (predeterminado: TRUE).
#' @param anho_abr Un valor lógico que indica si se debe incluir el año abreviado (predeterminado: TRUE).
#' @param sep_texto Un valor lógico que indica si se debe incluir un separador de texto entre las partes de la fecha (predeterminado: TRUE).
#' @return Un vector de cadenas de texto que representa las fechas en el formato especificado.
#' @import lubridate
#' @import stringr
#' @examples
#' FechaTexto(as.Date(c("2023-10-15", "2022-05-22")))
#' FechaTexto(as.Date(c("2023-10-15", "2022-05-22")), dia_nombre = FALSE)
#' @export
FechaTexto <- function(x, dia = TRUE, dia_nombre = TRUE, dia_nom_abr = TRUE,
                       mes = TRUE, mes_abr = TRUE, anho = TRUE,
                       anho_abr = TRUE, sep_texto = TRUE) {

  # Requiere las librerías necesarias
  require(lubridate)
  require(stringr)

  # Definición de los días de la semana
  dia_l <- c('Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo')
  dia_c <- c('lun', 'mar', 'mié', 'jue', 'vie', 'sáb', 'dom')

  # Definición de los meses
  mes_l <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto',
             'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  mes_c <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')

  # Convertir x a formato de fecha en caso que no lo sea
  x <- as.Date(x)

  # Día de la semana
  dn <- ifelse(dia_nombre,
               ifelse(dia_nom_abr, dia_c[wday(x, week_start = 1)], dia_l[wday(x, week_start = 1)]),
               NA)

  # Mes
  m <- ifelse(mes,
              ifelse(mes_abr, mes_c[month(x)], mes_l[month(x)]),
              NA)

  # Año
  y <- ifelse(anho,
              ifelse(anho_abr, format(x, "%y"), format(x, "%Y")),
              NA)

  # Día
  d <- ifelse(dia, day(x), NA)

  # Construcción del resultado
  res <- paste(dn, paste(m, y, sep = ifelse(sep_texto, " de ", "")), sep = ifelse(sep_texto, ", ", ""))

  # Retornar el resultado
  return(res)
}



#' Calcular Edad en Años Entre Dos Fechas
#'
#' Esta función calcula la edad en años completos entre dos fechas, considerando los años bisiestos.
#'
#' @param from La fecha de nacimiento (fecha inicial).
#' @param to La fecha actual (fecha final).
#' @return La edad en años entre las dos fechas.
#' @import lubridate
#' @examples
#' EdadCumplida(as.Date("1990-05-25"), Sys.Date())  # Devuelve la edad actual
#' EdadCumplida(as.Date("1985-10-10"), as.Date("2023-01-01"))  # Devuelve 37
#' @export
EdadCumplida <- function(from, to) {
  # Convertir a objetos de clase Date
  from <- as.Date(from)
  to <- as.Date(to)

  # Calcular la diferencia en años
  age <- as.numeric(difftime(to, from, units = "days")) / 365.25

  # Redondear a la cantidad entera de años
  return(floor(age))
}
