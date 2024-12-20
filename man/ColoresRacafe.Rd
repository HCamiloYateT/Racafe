% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ElementosGraficos.R
\name{ColoresRacafe}
\alias{ColoresRacafe}
\title{Generar Colores para Gráficos según Valores}
\usage{
ColoresRacafe(input_values)
}
\arguments{
\item{input_values}{Un vector de valores para los cuales se desea asignar colores.
Los valores que contengan la cadena "RACAFE & CIA S C A" serán asignados
el color 'firebrick'. El resto de los valores recibirá colores graduales de gris.}
}
\value{
Un vector de colores asignados a cada valor en el vector de entrada.
        Los valores que coincidan con "RACAFE & CIA S C A" (usando `grepl`) tendrán el color 'firebrick',
        y el resto de los valores se asignarán colores graduales de gris (desde gris claro a gris oscuro).
}
\description{
Esta función asigna colores a un conjunto de valores, con una excepción específica
para los valores que contienen la cadena "RACAFE & CIA S C A", que siempre se asigna un color particular
(firebrick). Para los otros valores, se asignan colores graduados desde el gris claro
hasta el gris oscuro, dependiendo de la cantidad de valores.
}
\examples{
# Asignar colores a un vector de valores
ColoresRacafe(c("RACAFE & CIA S C A", "Otro Valor", "Valor Importante"))

}
