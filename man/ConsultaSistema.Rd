% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Datos.R
\name{ConsultaSistema}
\alias{ConsultaSistema}
\title{Consulta a una base de datos SQL Server.}
\usage{
ConsultaSistema(bd, uid, pwd, query)
}
\arguments{
\item{bd}{Una cadena de texto que especifica el nombre de la base de datos a la que conectarse.
Puede ser uno de los siguientes valores: "syscafe", "cafesys" o "estad".}

\item{uid}{El nombre de usuario para la conexión a la base de datos.}

\item{pwd}{La contraseña del usuario para la conexión.}

\item{query}{La consulta SQL que se ejecutará en la base de datos.}
}
\value{
Un dataframe con los resultados de la consulta, con los nombres de las columnas limpiados.
}
\description{
Esta función se conecta a una base de datos SQL Server y ejecuta una consulta SQL.
Dependiendo del valor del parámetro `bd`, se selecciona la base de datos correspondiente.
Después de ejecutar la consulta, limpia los nombres de las columnas en el dataframe resultante.
}
