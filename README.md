# racafe

Paquete de R con funciones utilitarias usadas en proyectos de análisis y reporting de Racafé.

## Estado real del repositorio

Este repositorio **sí es un paquete de R** (no una app), con la siguiente estructura principal:

- `DESCRIPTION`, `NAMESPACE`, `LICENSE`: metadatos y configuración del paquete.
- `R/`: implementación de funciones por módulos (`Datos`, `Formatos`, `GTStyles`, `Inputs`, `Outputs`, `Pronosticos`, etc.).
- `man/`: documentación `.Rd` generada con roxygen2.
- `tests/testthat/`: pruebas unitarias.
- `renv/settings.json`: configuración de entorno reproducible.

Actualmente el paquete exporta **92 funciones** (ver `NAMESPACE`).

## Instalación

### Instalación local (recomendada para desarrollo)

```r
install.packages(c("devtools", "remotes"))
devtools::install(".")
```

### Instalación desde GitHub

```r
remotes::install_github("racafe/racafe")
```

## Uso rápido

```r
library(racafe)

# Ver funciones exportadas
getNamespaceExports("racafe")

# Abrir ayuda general del paquete
help(package = "racafe")
```

## Módulos del paquete (resumen)

- **Conectividad y datos (`R/Datos.R`)**:
  - conexión y escritura a BD: `ConectarBD()`, `EscribirDatos()`, `AgregarDatos()`, `ReemplazarDatos()`, `CargarDatos()`, `Consulta()`.
  - SQL Server: `ConsultaSistema()`.
  - Microsoft Graph / OneDrive / SharePoint: `ObtenerTokenAcceso()`, `CabecerasGraph()`, `ObtenerIdSite()`, `ObtenerIdDriveSite()`, `ObtenerIdDrive()`, `Listar*`, `CargarExcelDesdeOneDrive()`, `CargarExcelSite()`, `LeerExcelDesdeOneDrive()`, `Descargar*()`.
  - transformación: `TopAbsoluto()`, `TopRelativo()`, `bind_rows_na()`, `left_join_all()`, `RevisarDuplicados()`.

- **Formatos y utilidades visuales**:
  - `R/Formatos.R`: `DefinirFormato()`, `FormatoD3()`, `FormatoJS()`, `FormatoHOT()`, `FormatearNumero()`, `FormatearTabla()`, `FormatearTexto()`.
  - `R/GTStyles.R`: `gt_minimal_style()`, `gt_mensaje_vacio()`, `gt_pct_style()`, `gt_var_style()`, `gt_sign_style()`, `gt_color_columns()`, `col_kpi()`, `chr_kpi()`, `col_num()`.
  - `R/ElementosGraficos.R`: `vline()`, `hline()`, `ImprimirDensidad()`, `ImprimirAnillo()`, `ImprimeSankey()`, `ColoresRacafe()`, `ColoresGreenBlue()`.

- **Componentes Shiny**:
  - `R/Inputs.R`: `InputNumerico()`, `ListaDesplegable()`, `pick_opt()`, `BotonesRadiales()`, `BotonEstado()`, `BotonGuardar()`.
  - `R/Outputs.R`: `BotonDescarga()`, `CajaIco()`, `CajaValor()`.

- **Funciones de soporte**:
  - `R/Numericos.R`: `SiError_0()`, `Variacion()`, `Moda()`, `RedondearMultiplo()`.
  - `R/Fechas.R`: `PrimerDia()`, `FechaTexto()`, `EdadCumplida()`.
  - `R/Texto.R`: `LimpiarNombres()`, `LimpiarCadena()`, `UnirCadenas()`, `Unicos()`, `EsVacio()`, `EsEnteroPositivo()`, `EsNumero()`, `EsNumTelefono()`, `EsEmail()`.
  - `R/Html.R`: `Saltos()`, `Espacios()`, `Obligatorio()`.
  - `R/Liberias.R`: `Loadpkg()`.

- **Pronósticos (`R/Pronosticos.R`)**:
  - `aplicar_imputacion()`, `extraer_intervalos()`, `ejecutar_pronosticos()`, `Pronosticar()`, `PronMetricas()`, `PronSeleccionar()`, `PronSerie()`, `PronMensual()`, `PronPatronMes()`.

## Ejecutar pruebas

```r
testthat::test_dir("tests/testthat")
```

O desde terminal:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Nueva funcionalidad: `FormatearTabla()`

`FormatearTabla()` aplica `FormatearNumero()` por fila sobre todas las columnas numéricas de una tabla, usando defaults globales y permitiendo overrides por nombre del `Item` (primera columna) o por posición de fila.

```r
library(racafe)

datos <- data.frame(
  Item = c("Ventas", "Margen", "Meta"),
  Ene = c(1000, 0.34, 1200),
  Feb = c(950, 0.31, 1200)
)

tabla_fmt <- FormatearTabla(
  data = datos,
  formato_def = "numero",
  filas_fmt = list(
    Ventas = list(formato = "dinero", negrita = TRUE, meta = 980, prop = TRUE),
    Margen = list(formato = "porcentaje", color = "#0B5345"),
    "3"    = list(formato = "dinero", color = "#1F618D")
  )
)
```

## Notas

- La documentación de detalle por función está en `man/*.Rd` y se consulta con `?NombreFuncion`.
- Si agregas/modificas funciones, actualiza la documentación con roxygen2 y regenera `NAMESPACE`/`man`.
