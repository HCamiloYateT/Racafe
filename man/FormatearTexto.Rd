% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Formatos.R
\name{FormatearTexto}
\alias{FormatearTexto}
\title{Formatear Texto con Opciones de Estilo}
\usage{
FormatearTexto(
  x,
  negrita = TRUE,
  color = "#000000",
  tamano_pct = 1,
  alineacion = "left",
  transform = "none"
)
}
\arguments{
\item{x}{El texto a formatear.}

\item{negrita}{Un valor lógico (`TRUE` o `FALSE`) que indica si el texto debe aparecer en negrita (por defecto es `TRUE`).}

\item{color}{El color en formato hexadecimal (por defecto es "#000000").}

\item{tamano_pct}{El tamaño de la fuente en porcentaje (por defecto es 1, es decir, tamaño normal).}

\item{alineacion}{La alineación del texto. Puede ser "left", "center" o "right" (por defecto es "left").}

\item{transform}{La transformación del texto. Opciones: "none", "capitalize", "uppercase", "lowercase" (por defecto es "capitalize").}
}
\value{
El texto formateado en HTML con las opciones seleccionadas.
}
\description{
Esta función permite formatear un texto con diversas opciones de estilo, como
resaltar en negrita, cambiar el color, ajustar el tamaño de la fuente, alinear el texto
y transformar el texto a mayúsculas, minúsculas o capitalización de la primera letra.
}
\examples{
# Formatear texto en negrita, color azul y tamaño 120\%
FormatearTexto("Texto formateado", negrita = TRUE, color = "#0000FF", tamano_pct = 1.2)

# Texto en mayúsculas y alineado al centro
FormatearTexto("Texto en mayúsculas", transform = "uppercase", alineacion = "center")

}
