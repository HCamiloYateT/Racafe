# Funciones Utilitarias

Este repositorio contiene un paquete de R creado específicamente para **Racafé**, el cual incluye una serie de funciones misceláneas diseñadas para facilitar y optimizar la creación de reportes. Las funciones incluidas permiten la generación de gráficos, cálculos de KPIs, manipulación de datos y otros procesos útiles en el contexto de análisis y reportes corporativos.

## Instalación

```r
# Instalar la librería devtools si aún no está disponible
install.packages("devtools")

# Clonar el repositorio y, una vez dentro de la carpeta, ejecutar:
devtools::install(".")

# Alternativamente, puede instalar directamente desde GitHub
install.packages("remotes")
remotes::install_github("racafe/racafe")
```

## Funciones del paquete

### Conexión y escritura de bases
- `ConectarBD()`: Establece una conexión a la base de datos usando variables de entorno.
```r
con <- ConectarBD()
DBI::dbDisconnect(con)
```
- `EscribirDatos(df, tabla)`: Sobrescribe una tabla con el contenido de un `data.frame`.
```r
df <- data.frame(x = 1:3, y = letters[1:3])
EscribirDatos(df, "mi_tabla")
```
- `AgregarDatos(df, tabla)`: Agrega filas de un `data.frame` a una tabla existente.
```r
df_nuevos <- data.frame(x = 4:6, y = letters[4:6])
AgregarDatos(df_nuevos, "mi_tabla")
```
- `CargarDatos(tabla, condicion = NULL)`: Recupera datos de una tabla, opcionalmente filtrando con una condición.
```r
df <- CargarDatos("mi_tabla")
df_filtrado <- CargarDatos("mi_tabla", "x > 10")
```
- `Consulta(consulta)`: Ejecuta una consulta SQL arbitraria y retorna un `data.frame`.
```r
resultado <- Consulta("SELECT COUNT(*) AS n FROM mi_tabla")
```

### Integración con OneDrive/Microsoft Graph
- `ObtenerTokenAcceso()`: Obtiene un token de acceso para la API de Microsoft Graph.
```r
token <- ObtenerTokenAcceso()
```
- `CabecerasGraph()`: Construye las cabeceras HTTP con el token Bearer.
```r
headers <- CabecerasGraph()
```
- `ObtenerIdDrive(usuario)`: Devuelve el identificador de OneDrive del usuario.
```r
drive_id <- ObtenerIdDrive("juan.perez")
```
- `CargarExcelDesdeOneDrive(usuario, ruta, archivo)`: Descarga un Excel y lo abre como `openxlsx2::wb_load`.
```r
wb <- CargarExcelDesdeOneDrive("juan.perez", "Carpeta/Reportes", "informe.xlsx")
```
- `DescargarExcelDesdeOneDrive(usuario, ruta, archivo, nombre_salida)`: Guarda localmente un Excel desde OneDrive.
```r
DescargarExcelDesdeOneDrive("juan.perez", "Carpeta/Reportes", "informe.xlsx", "informe_local")
```
- `ListarCarpetas(usuario)`: Lista las carpetas en la raíz del OneDrive del usuario.
```r
carpetas <- ListarCarpetas("juan.perez")
```
- `ObtenerIdCarpeta(usuario, nombre_carpeta)`: Obtiene el ID de una carpeta por nombre.
```r
carpeta_id <- ObtenerIdCarpeta("juan.perez", "Reportes")
```
- `ListarContenidoCarpetaNombre(usuario, nombre_carpeta)`: Lista archivos y carpetas dentro de una carpeta por nombre.
```r
df <- ListarContenidoCarpetaNombre("juan.perez", "Reportes")
```
- `ListarContenidoCarpetaId(usuario, carpeta_id)`: Lista archivos y carpetas dentro de una carpeta por ID.
```r
df <- ListarContenidoCarpetaId("juan.perez", "0123ABC...")
```
- `ListarContenidoCarpetaRecursivo(usuario, carpeta_id)`: Recorre una carpeta por ID de forma recursiva devolviendo una lista.
```r
lst <- ListarContenidoCarpetaRecursivo("juan.perez", "0123ABC...")
```
- `ListarTodoContenidoCarpeta(usuario, carpeta_id)`: Devuelve un tibble con todos los archivos de una carpeta de manera recursiva.
```r
df <- ListarTodoContenidoCarpeta("juan.perez", "0123ABC...")
```
- `DescargarArchivoId(archivo_id, usuario)`: Descarga un archivo de OneDrive a un temporal y retorna su ruta.
```r
tmp <- DescargarArchivoId("ABC123...", "juan.perez")
```
- `ListarHojasExcelOneDrive(archivo_id, usuario)`: Lista las hojas de un Excel alojado en OneDrive.
```r
hojas <- ListarHojasExcelOneDrive("ABC123...", "juan.perez")
```
- `LeerExcelDesdeOneDrive(archivo_id, usuario, ...)`: Lee un Excel de OneDrive con `readxl::read_excel`.
```r
df <- LeerExcelDesdeOneDrive("ABC123...", "juan.perez", sheet = "Datos", skip = 1)
```

### Datos y transformación
- `ConsultaSistema(bd, uid, pwd, query, server = "172.16.19.21", port = 1433)`: Ejecuta una consulta en SQL Server y devuelve un `data.frame` con nombres de columnas limpios.
```r
# df <- ConsultaSistema("syscafe", "usuario", "clave", "SELECT TOP 10 * FROM tabla")
```
- `TopAbsoluto(data, var_recode, var_top, fun_Top, n = 10, nom_var, lab_recodificar = "OTROS")`: Recodifica categorías menos frecuentes según valores absolutos o una función de resumen.
```r
# df <- TopAbsoluto(df, Categoria, Valor, fun_Top = "sum", n = 5, nom_var = "CategoriaTop")
```
- `TopRelativo(data, var_recode, var_top, fun_Top, pct_min = 0.05, nom_var, lab_recodificar = "OTROS")`: Recodifica categorías menos frecuentes según porcentaje mínimo.
```r
# df <- TopRelativo(df, Categoria, Valor, fun_Top = "sum", pct_min = 0.05, nom_var = "CategoriaTop")
```
- `AdicionarBotones(tabla, botones)`: Agrega columnas con botones HTML interactivos a una tabla.
```r
# tabla <- AdicionarBotones(tabla, c("Detalle", "Editar"))
```
- `bind_rows_na(...)`: Combina múltiples `data.frame` ignorando aquellos vacíos.
```r
# res <- bind_rows_na(df1, df2, df3)
```
- `left_join_all(x, y_list, by, type = "left")`: Realiza uniones iterativas utilizando las funciones `*_join()` de `dplyr`.
```r
# resultado <- left_join_all(df_base, list(df_extra1, df_extra2), by = c("id" = "id"))
```
- `%||%(a, b)`: Operador infijo que retorna `a` si no es nulo ni vacío; de lo contrario, retorna `b`.
```r
valor <- entrada %||% "valor_por_defecto"
```
- `Loadpkg(pkg)`: Carga un paquete si está instalado o lo instala antes de cargarlo.
```r
Loadpkg("dplyr")
```

### Formatos y estilos
- `DefinirFormato(formato, ...)`: Registra un formato personalizado reutilizable.
```r
DefinirFormato("porcentaje", scales::percent_format())
```
- `FormatoD3(formato)`: Convierte un formato registrado a la sintaxis usada por D3.js.
```r
FormatoD3("porcentaje")
```
- `FormatoJS(formato)`: Obtiene la representación JavaScript de un formato.
```r
FormatoJS("porcentaje")
```
- `FormatoHOT(formato)`: Traduce un formato a la sintaxis de Handsontable.
```r
FormatoHOT("porcentaje")
```
- `FormatearNumero(x, formato, negrita = TRUE, color = "#000000", meta = NA, prop = TRUE)`: Aplica formato numérico con estilos condicionales.
```r
FormatearNumero(0.25, "porcentaje", meta = 0.2)
```
- `FormatearTexto(x, negrita = TRUE, color = "#000000", tamano_pct = 1, alineacion = "left", transform = "none")`: Aplica estilo a texto plano.
```r
FormatearTexto("Meta alcanzada", color = "#28B78D")
```
- `gt_minimal_style(gt_table)`: Aplica un estilo mínimo a objetos `gt`.
```r
tabla <- gt_minimal_style(gt::gt(head(mtcars)))
```
- `col_kpi(x, prop = TRUE)`: Devuelve estilos de color para columnas KPI según cumplimiento.
```r
col_kpi(c(0.8, 1.1))
```
- `chr_kpi(x)`: Genera indicadores textuales de desempeño.
```r
chr_kpi(c(0.8, 1.1))
```
- `col_num(x)`: Define la paleta numérica estándar del paquete.
```r
col_num(1:5)
```
- `gt_pct_style(gt_table, ...)`: Configura porcentajes en tablas `gt`.
```r
gt_pct_style(gt::gt(head(mtcars)), columns = mpg)
```
- `gt_var_style(gt_table, ...)`: Da formato variacional a columnas de `gt`.
```r
gt_var_style(gt::gt(head(mtcars)), columns = cyl)
```
- `gt_color_columns(gt_table, columns, color)`: Colorea columnas específicas en `gt`.
```r
gt_color_columns(gt::gt(head(mtcars)), columns = hp, color = "#28B78D")
```

### Elementos gráficos
- `vline(x = 0, color = "red")`: Crea una línea vertical en gráficos `plotly`.
```r
vline(10, "#28B78D")
```
- `hline(y = 0, color = "#ff3a21")`: Crea una línea horizontal en `plotly`.
```r
hline(0.5)
```
- `ColoresRacafe(input_values)`: Genera una paleta de colores corporativos.
```r
ColoresRacafe(5)
```
- `ColoresGreenBlue(value)`: Devuelve gradientes verde-azul según valores numéricos.
```r
ColoresGreenBlue(seq(0, 1, length.out = 5))
```
- `ImprimirAnillo(data, var_label, var_medida = NULL, funcion = c("sum", "n"), colores = NULL)`: Construye gráficos de anillo en `plotly`.
```r
ImprimirAnillo(df, var_label = "categoria", var_medida = "valor")
```

### Componentes de entrada (Shiny)
- `InputNumerico(id, label, value, dec = 2, max = NULL, min = NULL, type = "numero", label_col = 6, input_col = 6, width = "100%")`: Genera un input numérico personalizado.
```r
InputNumerico("ventas", "Ventas", 1000, dec = 0)
```
- `ListaDesplegable(inputId, label = NULL, choices, selected = choices, multiple = TRUE, fem = FALSE, ns = NULL)`: Crea un `pickerInput` con estilo.
```r
ListaDesplegable("region", "Región", choices = c("Norte", "Sur"))
```
- `pick_opt(cho, fem = TRUE)`: Construye opciones con género en `pickerInput`.
```r
pick_opt(c("Seleccionar", "Todas"))
```
- `BotonesRadiales(inputId, label = NULL, choices, selected = NULL, ...)`: Genera botones radiales estilizados.
```r
BotonesRadiales("estado", "Estado", choices = c("Activo", "Inactivo"))
```
- `BotonEstado(...)`: Construye un botón tipo interruptor para activar/desactivar estados.
```r
BotonEstado("toggle", "Activar filtro")
```

### Componentes de salida (Shiny)
- `BotonDescarga(button_id, icon_name = "file-excel", color = "#28b78d", ns = NULL, ...)`: Crea un botón de descarga personalizado.
```r
BotonDescarga("descargar")
```
- `CajaIco(texto, icono, col_fondo = "#FDFEFE", alto = 120, col_letra = "#17202A", col_icono = "#000000")`: Diseña cajas informativas con íconos.
```r
CajaIco("Ingresos", "chart-line")
```
- `ImprimeSankey(data, vars, fun, var = NULL, colores)`: Genera diagramas Sankey con `plotly`.
```r
ImprimeSankey(df, vars = c("origen", "destino"), fun = "sum", var = "valor")
```

### Funciones numéricas
- `SiError_0(x)`: Reemplaza errores por cero.
```r
SiError_0(tryCatch(log(-1), error = function(e) e))
```
- `Variacion(ini, fin)`: Calcula la variación porcentual entre dos valores.
```r
Variacion(100, 120)
```
- `Moda(x, na.rm = TRUE)`: Determina la moda de un vector.
```r
Moda(c(1, 2, 2, 3))
```
- `RedondearMultiplo(x, multiple)`: Redondea al múltiplo más cercano.
```r
RedondearMultiplo(17, 5)
```

### Manejo de fechas
- `PrimerDia(x, uni = "month")`: Retorna el primer día de la unidad temporal de una fecha.
```r
PrimerDia("2023-10-15")
```
- `FechaTexto(x, ...)`: Convierte fechas en un texto personalizado.
```r
FechaTexto(as.Date("2023-10-15"))
```
- `EdadCumplida(from, to)`: Calcula la edad en años entre dos fechas.
```r
EdadCumplida(as.Date("1990-05-25"), Sys.Date())
```

### Manipulación de texto
- `LimpiarNombres(s)`: Normaliza cadenas eliminando espacios repetidos y convirtiéndolas a mayúsculas.
```r
LimpiarNombres("  Camilo    Yate  ")
```
- `LimpiarCadena(x, rem_espacios = FALSE, rem_numeros = TRUE, rem_caresp = TRUE, rem_acentos = TRUE)`: Limpia caracteres no deseados de un texto.
```r
LimpiarCadena("¡Hola, mundo 123!")
```
- `UnirCadenas(..., sep = " ", collapse = NULL, na.rm = FALSE)`: Une textos omitiendo `NA` si se indica.
```r
UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
```
- `Unicos(x)`: Devuelve los valores únicos ordenados de un vector.
```r
Unicos(c("b", "a", "a"))
```
- `EsVacio(x)`: Verifica si un valor es `NULL`, `NA` o una cadena vacía.
```r
EsVacio("")
```
- `EsEnteroPositivo(s)`: Comprueba si una cadena representa un entero positivo.
```r
EsEnteroPositivo("123")
```
- `EsNumero(cadena)`: Comprueba si la cadena es un número positivo. Los valores `NA` y las cadenas vacías retornan `FALSE`.
```r
EsNumero("12.3")
EsNumero("")
EsNumero(NA)
```
- `EsNumTelefono(tel)`: Valida el formato de un número telefónico.
```r
EsNumTelefono("3123456789")
```
- `EsEmail(email)`: Valida direcciones de correo electrónico.
```r
EsEmail("usuario@racafe.com")
```

### Utilidades HTML
- `Saltos(n = 1)`: Genera saltos de línea en HTML.
```r
Saltos(2)
```
- `Espacios(n = 1)`: Inserta espacios no separables.
```r
Espacios(3)
```
- `Obligatorio(s)`: Marca texto como obligatorio con estilo HTML.
```r
Obligatorio("Campo requerido")
```

### Pronósticos
- `aplicar_imputacion(ts_data, metodo_imputacion, valor_constante = NULL, prob_percentil = 0.25)`: Imputa valores faltantes en series de tiempo usando distintos métodos.
```r
serie_imputada <- aplicar_imputacion(serie, "promedio")
```
- `extraer_intervalos(forecast_obj, nivel_conf)`: Obtiene intervalos de confianza de un objeto de pronóstico.
```r
intervalos <- extraer_intervalos(pronostico, 0.95)
```
- `ejecutar_pronosticos(train_data, test_data, h_periods, fechas_futuras, ...)`: Ajusta múltiples modelos y devuelve resultados comparativos.
```r
resultado <- ejecutar_pronosticos(train, test, h_periods = 12, fechas_futuras = seq_len(12))
```
- `Pronosticar(df, fecha_col = "fecha", valor_cols = NULL, nivel_confianza = 0.95, ...)`: Orquesta el flujo completo de pronósticos desde datos crudos.
```r
pron <- Pronosticar(df, fecha_col = "fecha", valor_cols = c("ventas"))
```
- `PronMetricas(resultado_pronostico, columna = NULL)`: Calcula métricas de precisión para las columnas seleccionadas.
```r
PronMetricas(pron)
```
- `PronSeleccionar(resultado_pronostico, columna = NULL, ...)`: Elige el mejor modelo por columna o segmento.
```r
seleccion <- PronSeleccionar(pron)
```
- `PronSerie(seleccion, columna = NULL)`: Construye tablas detalladas de series pronosticadas.
```r
PronSerie(seleccion)
```
- `PronMensual(seleccion, columna = NULL, incluir_pronosticos = TRUE)`: Resume pronósticos por mes.
```r
PronMensual(seleccion)
```
- `PronPatronMes(seleccion, columna = NULL)`: Analiza patrones de comportamiento mensual.
```r
PronPatronMes(seleccion)
```

