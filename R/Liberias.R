# Paquetes ----
#' Cargar e Instalar Paquetes Requeridos
#'
#' Esta función verifica si los paquetes de R especificados están instalados.
#' Si algún paquete no está instalado, lo instala. Posteriormente, carga todos los paquetes solicitados.
#'
#' @param pkg Un vector de caracteres con los nombres de los paquetes que se desean verificar, instalar (si es necesario) y cargar.
#' @return Un vector lógico que indica si cada paquete fue cargado con éxito (\code{TRUE} si se cargó, \code{FALSE} en caso contrario).
#' @examples
#' Loadpkg(c("ggplot2", "dplyr"))  # Instala y carga ggplot2 y dplyr si es necesario
#' @export
Loadpkg <- function(pkg) {
  # Identificar los paquetes que no están instalados
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

  # Instalar paquetes faltantes
  if (length(new.pkg) > 0) {
    install.packages(new.pkg, dependencies = TRUE)
  }

  # Cargar los paquetes requeridos y devolver vector lógico de éxito/fracaso
  sapply(pkg, require, character.only = TRUE)
}
