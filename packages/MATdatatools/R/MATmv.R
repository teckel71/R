#' MATmv: Manejo de datos con casos completos y NA
#'
#' Esta función selecciona variables de un dataframe, identifica casos completos y crea un resumen
#' con información sobre los datos faltantes. Ahora también permite seleccionar una sola variable
#' sin necesidad de usar `c(...)`.
#'
#' @param dataframe Un dataframe que contiene las variables.
#' @param columnas Las columnas que se desean seleccionar. Puede ser una sola variable (sin comillas)
#'   o un vector de variables.
#' @return El nombre del nuevo dataframe con los casos completos.
#' @details La función genera un nuevo dataframe con los casos completos de las columnas seleccionadas
#'   y una lista de información en el entorno global. La lista contiene una tabla con los datos faltantes
#'   y un gráfico que visualiza los valores faltantes.
#' @examples
#' # Usar una sola variable:
#' MATmv(dataframe = mtcars, columnas = mpg)
#'
#' # Usar varias variables:
#' MATmv(dataframe = mtcars, columnas = c(mpg, hp))
#' @export
MATmv <- function(dataframe, columnas) {

  # Verificar si los paquetes necesarios están instalados y cargarlos
  if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")
  if (!requireNamespace("visdat", quietly = TRUE)) install.packages("visdat")
  if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
  if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

  library(magrittr)
  library(visdat)
  library(knitr)
  library(kableExtra)

  # Convertir nombres de columnas a texto si no lo son
  columnas <- substitute(columnas)
  if (length(columnas) == 1) {
    columnas <- as.character(columnas)
  } else {
    columnas <- sapply(columnas[-1], deparse)
  }

  # Verificar que el dataframe sea válido
  if (!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  # Verificar que las columnas existen en el dataframe
  columnas_no_validas <- setdiff(columnas, colnames(dataframe))
  if (length(columnas_no_validas) > 0) {
    stop(paste("Las siguientes columnas no existen en el dataframe:", paste(columnas_no_validas, collapse = ", ")))
  }

  # Seleccionar las variables deseadas y asegurar que el resultado sea un dataframe
  df_seleccion <- dataframe[, columnas, drop = FALSE]

  # Conservar los nombres de las filas originales
  rownames(df_seleccion) <- rownames(dataframe)

  # Crear el nuevo dataframe con casos completos
  df_sm <- dataframe[complete.cases(df_seleccion), ]

  # Nombrar el nuevo dataframe
  nombre_df_sm <- paste0(deparse(substitute(dataframe)), "_sm")
  assign(nombre_df_sm, df_sm, envir = .GlobalEnv)

  # Identificar los casos con datos faltantes
  df_na <- df_seleccion[!complete.cases(df_seleccion), , drop = FALSE]

  # Crear la lista de información
  lista_info <- list()

  if (nrow(df_na) > 0) {
    # Crear la tabla en formato HTML sin duplicar los nombres de los casos
    tabla_na <- kableExtra::kable(df_na, format = "html",
                                  caption = "Casos con datos faltantes") %>%
      kableExtra::kable_styling(full_width = FALSE)

    # Agregar la tabla a la lista
    lista_info$tabla_na <- tabla_na
    message("Se han encontrado casos con datos faltantes. La información está almacenada en la lista.")
  } else {
    message("No hay casos con datos faltantes en las variables seleccionadas.")
    lista_info$tabla_na <- NULL
  }

  # Crear el gráfico de vis_miss
  grafico_vis_miss <- visdat::vis_miss(df_seleccion)
  lista_info$grafico_vis_miss <- grafico_vis_miss

  # Nombrar la lista y guardarla en el entorno global
  nombre_lista <- paste0(nombre_df_sm, "_info")
  assign(nombre_lista, lista_info, envir = .GlobalEnv)

  # Devolver el nombre del nuevo dataframe
  return(nombre_df_sm)
}
