#' MATfexcel: Importar datos desde una hoja de Excel
#'
#' Esta función importa datos desde una hoja de cálculo Excel, convierte la primera columna
#' en nombres de fila, y muestra un resumen de las variables.
#'
#' @param file_path Ruta al archivo Excel.
#' @param sheet_name Nombre o índice de la hoja de Excel a importar.
#' @param na_values Un vector de caracteres que representa valores que deben tratarse como `NA`. Por defecto, las celdas vacías se consideran `NA`.
#' @return Un dataframe con los datos importados.
#' @import readxl
#' @examples
#' \dontrun{
#' # Importar datos con celdas vacías como NA
#' my_data <- MATfexcel("datos.xlsx", "Datos")
#'
#' # Importar datos con valores adicionales considerados como NA
#' my_data <- MATfexcel("datos.xlsx", "Datos", na_values = c("n.d.", "s.d."))
#' }
#' @export
MATfexcel <- function(file_path, sheet_name, na_values = NULL) {
  # Verificar si el paquete readxl está instalado y cargarlo
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  library(readxl)

  # Leer los datos desde la hoja de Excel
  data <- read_excel(file_path, sheet = sheet_name, na = na_values)

  # Convertir a dataframe y establecer la primera columna como nombres de fila
  data <- data.frame(data, row.names = 1)

  # Mostrar un resumen de las variables
  print(summary(data))

  # Devolver el dataframe
  return(data)
}
