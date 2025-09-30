# Funciones Utilitarias

Este repositorio contiene un paquete de R creado específicamente para **Racafé**, el cual incluye una serie de funciones misceláneas diseñadas para facilitar y optimizar la creación de reportes. Las funciones incluidas permiten la generación de gráficos, cálculos de KPIs, manipulación de datos y otros procesos útiles en el contexto de análisis y reportes corporativos.
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
- `CargarDatos(tabla, condicion = NULL)`: Recupera datos de una tabla opcionalmente filtrando con una condición.
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

### Datos
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

### Fechas
- `PrimerDia(x, uni = "month")`: Retorna el primer día de la unidad temporal de una fecha.
```r
# PrimerDia("2023-10-15")
```
- `FechaTexto(x, ...)`: Convierte fechas en un texto personalizado.
```r
# FechaTexto(as.Date("2023-10-15"))
```
- `EdadCumplida(from, to)`: Calcula la edad en años entre dos fechas.
```r
# EdadCumplida(as.Date("1990-05-25"), Sys.Date())
```

### Texto
- `LimpiarNombres(s)`: Normaliza cadenas eliminando espacios repetidos y convirtiéndolas a mayúsculas.
```r
# LimpiarNombres("  Camilo    Yate  ")
```
- `LimpiarCadena(x, rem_espacios = FALSE, rem_numeros = TRUE, rem_caresp = TRUE, rem_acentos = TRUE)`: Limpia caracteres no deseados de un texto.
```r
# LimpiarCadena("¡Hola, mundo 123!")
```
- `UnirCadenas(..., sep = " ", collapse = NULL, na.rm = FALSE)`: Une textos omitendo `NA` si se indica.
```r
# UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
```
- `Unicos(x)`: Devuelve los valores únicos ordenados de un vector.
```r
# Unicos(c("b", "a", "a"))
```
- `EsVacio(x)`: Verifica si un valor es `NULL`, `NA` o una cadena vacía.
```r
# EsVacio("")
```
- `EsEnteroPositivo(s)`: Comprueba si una cadena representa un entero positivo.
```r
# EsEnteroPositivo("123")
```
- `EsNumero(cadena)`: Comprueba si la cadena es un número positivo. Los valores `NA` y las cadenas vacías retornan `FALSE`.
```r
# EsNumero("12.3")
# EsNumero("")
# EsNumero(NA)
```
- `EsNumTelefono(tel)`: Verifica si una cadena corresponde a un número de teléfono válido.
```r
# EsNumTelefono("3001234567")
```
- `EsEmail(email)`: Valida si una cadena corresponde a un correo electrónico.
```r
# EsEmail("test@example.com")
```

### Elementos gráficos
- `vline(x = 0, color = "red")`: Genera una línea vertical para gráficos interactivos.
```r
# plotly::layout(shapes = list(vline(2)))
```
- `hline(y = 0, color = "#ff3a21")`: Genera una línea horizontal para gráficos interactivos.
```r
# plotly::layout(shapes = list(hline(3)))
```
- `ColoresRacafe(input_values)`: Asigna colores personalizados resaltando "RACAFE".
```r
# ColoresRacafe(c("RACAFE", "Otro"))
```
- `ColoresGreenBlue(value)`: Genera una paleta de colores del verde al azul.
```r
# ColoresGreenBlue(1:5)
```

### Formatos
- `DefinirFormato(formato)`: Devuelve un formateador numérico predefinido.
```r
# f <- DefinirFormato("dinero"); f(1234.5)
```
- `FormatoD3(formato)`: Retorna una cadena de formato compatible con D3.js.
```r
# FormatoD3("porcentaje")
```
- `FormatoJS(formato)`: Devuelve una función de formato en JavaScript como cadena.
```r
# FormatoJS("coma")
```
- `FormatoHOT(formato)`: Entrega una cadena de formato para Handsontable.
```r
# FormatoHOT("dinero")
```
- `FormatearNumero(x, formato, ...)`: Envuelve números en HTML con estilo.
```r
# FormatearNumero(2500, "dinero", meta = 2000)
```
- `FormatearTexto(x, ...)`: Aplica estilos HTML a un texto.
```r
# FormatearTexto("Hola", color = "#0000FF")
```
- `gt_minimal_style(gt_table)`: Aplica un estilo minimalista a tablas `gt`.
```r
# gt::gt(head(mtcars)) |> gt_minimal_style()
```
- `col_kpi(x, prop = TRUE)`: Asigna un color según el valor de un KPI.
```r
# col_kpi(1)
```
- `chr_kpi(x)`: Devuelve un símbolo representativo del KPI.
```r
# chr_kpi(-1)
```
- `col_num(x)`: Define color según si el número es positivo o negativo.
```r
# col_num(c(1, -1))
```

### HTML
- `Saltos(n = 1)`: Inserta saltos de línea `<br/>` en HTML.
```r
# Saltos(2)
```
- `Espacios(n = 1)`: Inserta espacios `&emsp;` en HTML.
```r
# Espacios(3)
```
- `Obligatorio(s)`: Añade un asterisco rojo a un campo obligatorio.
```r
# Obligatorio("Nombre")
```

### Inputs
- `InputNumerico(id, label, value, ...)`: Crea un campo numérico estilizado para `shiny`.
```r
# InputNumerico("monto", "Monto:", 1000, type = "dinero")
```
- `ListaDesplegable(id, label, choices, ...)`: Genera un menú desplegable compatible con `shiny` utilizando estilos corporativos.
```r
# ListaDesplegable("producto", "Producto:", choices = c("Café", "Azúcar", "Cacao"))
```
- `pick_opt(cho, fem = TRUE)`: Genera opciones personalizadas para `selectPicker`.
```r
# pick_opt(letters[1:5])
```

### Numéricos
- `SiError_0(x)`: Reemplaza valores `NaN` e infinitos por 0.
```r
# SiError_0(c(1, NaN, Inf))
```
- `Variacion(ini, fin)`: Calcula la variación relativa entre valores iniciales y finales.
```r
# Variacion(c(10, 0), c(15, 5))
```
- `Moda(x, na.rm = TRUE)`: Obtiene la moda de un vector.
```r
# Moda(c(1, 2, 2, 3))
```
- `RedondearMultiplo(x, multiple)`: Redondea al múltiplo más cercano.
```r
# RedondearMultiplo(453, 100)
```

### Outputs
- `CajaIco(texto, icono, ...)`: Crea una caja con ícono y texto.
```r
# CajaIco("Mensaje", "info-circle")
```
- `ImprimeSankey(data, vars, fun, var = NULL, colores)`: Genera un diagrama de Sankey.
```r
# ImprimeSankey(df, c("Var1", "Var2"), "n", colores = c("blue", "green"))
```

### Librerías
- `Loadpkg(pkg)`: Instala (si es necesario) y carga paquetes de R.
```r
# Loadpkg(c("ggplot2", "dplyr"))
```

