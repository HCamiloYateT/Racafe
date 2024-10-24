% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Numericos.R
\name{Variacion}
\alias{Variacion}
\title{Calcular Variación Porcentual}
\usage{
Variacion(ini, fin)
}
\arguments{
\item{ini}{El número inicial (denominador).}

\item{fin}{El número final (numerador).}
}
\value{
Un valor numérico que representa la variación porcentual.
}
\description{
Esta función calcula la variación porcentual entre un número inicial y uno final.
La variación porcentual se calcula de acuerdo con las siguientes reglas:
 - Si ambos valores son 0, la variación es 0.
 - Si el valor inicial es 0 y el final es distinto de 0, la variación es 0.
 - Si ambos valores tienen el mismo signo (o son ambos positivos o ambos negativos), se calcula como \code{(fin - ini) / ini}.
 - Si los valores tienen signos opuestos, la variación se calcula como \code{(fin - ini) / abs(ini)}.
}
\examples{
Variacion(10, 15)   # Devuelve 0.5 (50\% de aumento)
Variacion(0, 10)    # Devuelve 0 (por convención en este caso)
Variacion(-10, 5)   # Devuelve 1.5 (150\% de aumento, en valor absoluto del denominador)
}
