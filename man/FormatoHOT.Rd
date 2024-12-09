% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Formatos.R
\name{FormatoHOT}
\alias{FormatoHOT}
\title{Definir el Formato de Números para Handsontable (HOT)}
\usage{
FormatoHOT(formato)
}
\arguments{
\item{formato}{Un string que especifica el formato deseado para los números. Puede ser uno de los siguientes:
- "coma": Números con comas como separadores de miles.
- "numero": Números con dos decimales.
- "dinero": Números con símbolo "$" y comas como separadores de miles.
- "porcentaje": Números expresados como porcentaje con dos decimales.}
}
\value{
Un string que representa el formato correspondiente para usar en Handsontable (HOT).
}
\description{
Esta función asigna un formato de número adecuado para ser usado en Handsontable, una biblioteca JavaScript.
Los formatos disponibles son: "coma" (números con comas como separadores de miles), "numero" (números con dos decimales),
"dinero" (números con símbolo de dólar y separadores de miles), y "porcentaje" (números expresados como porcentaje).
}
\examples{
# Formato con comas
FormatoHOT("coma")

# Formato de dinero
FormatoHOT("dinero")

}
