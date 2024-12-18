% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Formatos.R
\name{chr_kpi}
\alias{chr_kpi}
\title{Obtener un carácter que represente la dirección de un indicador}
\usage{
chr_kpi(x)
}
\arguments{
\item{x}{El número del cual se desea obtener el carácter representativo.}
}
\value{
Un carácter: '▲' para valores positivos, '▼' para negativos y '▬' para valores iguales a 0.
}
\description{
Esta función devuelve un carácter que representa el crecimiento o decrecimiento de un indicador numérico.
}
\examples{
chr_kpi(1)  # Devuelve "▲" para crecimiento positivo.
chr_kpi(-1)  # Devuelve "▼" para decrecimiento negativo.
}
