% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Datos.R
\name{bind_rows_na}
\alias{bind_rows_na}
\title{Combina múltiples data frames en uno solo, ignorando aquellos que están vacíos.}
\usage{
bind_rows_na(...)
}
\arguments{
\item{...}{Data frames a combinar. Puede recibir uno o más data frames.}
}
\value{
Un data frame que resulta de la combinación de los data frames
        proporcionados. Si no hay data frames no vacíos, se devuelve un
        data frame vacío.
}
\description{
Esta función toma varios data frames como entrada y los combina en un solo
data frame. Los data frames vacíos se ignoran para evitar errores al
realizar la unión.
}
\examples{
df1 <- data.frame(a = 1:3, b = letters[1:3])
df2 <- data.frame(a = numeric(0), b = character(0))  # Data frame vacío
df3 <- data.frame(a = 4:5, b = letters[4:5])

# Combina df1 y df3, ignorando df2
result <- bind_rows_na(df1, df2, df3)

}
