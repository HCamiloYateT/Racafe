% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Html.R
\name{Espacios}
\alias{Espacios}
\title{Insertar Espacios en HTML}
\usage{
Espacios(n = 1)
}
\arguments{
\item{n}{Un valor entero que indica el número de espacios a generar. El valor predeterminado es \code{1}.}
}
\value{
Una cadena de texto que contiene \code{&emsp;} repetido \code{n} veces.
}
\description{
Esta función genera una cadena con un número especificado de espacios en formato HTML (\code{&emsp;}).
}
\examples{
Espacios(4)  # Devuelve "&emsp;&emsp;&emsp;&emsp;"
}
