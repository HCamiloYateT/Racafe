% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Numericos.R
\name{Variacion}
\alias{Variacion}
\title{Calcula la variación porcentual entre dos valores.}
\usage{
Variacion(ini, fin)
}
\arguments{
\item{ini}{Valor inicial. Puede ser un número positivo, negativo o cero.}

\item{fin}{Valor final. Puede ser un número positivo, negativo o cero.}
}
\value{
Un número que representa la variación porcentual entre `ini` y `fin`.
Si `ini` es cero y `fin` también es cero, devuelve 0. Si `ini` es cero y `fin`
es diferente de cero, devuelve el signo de `fin` (1 o -1).
En caso de que ambos sean distintos de cero, calcula la variación como
\eqn{(fin - ini) / |ini|}.
}
\description{
Esta función calcula la variación porcentual entre un valor inicial (`ini`) y un valor final (`fin`).
La variación se calcula en relación con el valor absoluto de `ini`, y maneja correctamente
los casos en que `ini` o `fin` pueden ser cero, positivos o negativos.
}
\examples{
Variacion(10, 15)   # Devuelve 0.5, lo que representa un aumento del 50\%.
Variacion(-10, -5)  # Devuelve 0.5, lo que representa un aumento del 50\%.
Variacion(0, 5)     # Devuelve 1, porque pasa de 0 a un valor positivo.
Variacion(0, -5)    # Devuelve -1, porque pasa de 0 a un valor negativo.
Variacion(10, -5)   # Devuelve -1.5, representa una disminución significativa.

}
