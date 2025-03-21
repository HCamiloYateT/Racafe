% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Numericos.R
\name{SiError_0}
\alias{SiError_0}
\title{Reemplazar Valores NaN, Inf y -Inf por 0}
\usage{
SiError_0(x)
}
\arguments{
\item{x}{Un vector numérico en el que se buscarán los valores `NaN`, `Inf` y `-Inf`.}
}
\value{
Un nuevo vector numérico con los valores `NaN`, `Inf` y `-Inf` reemplazados por 0.
}
\description{
Esta función reemplaza los valores `NaN`, `Inf` y `-Inf` en un vector numérico por 0.
}
\examples{
SiError_0(c(1, NaN, 2, Inf, -Inf))  # Devuelve: 1 0 2 0 0
}
