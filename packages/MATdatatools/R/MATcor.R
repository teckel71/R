#' MATcor: Matriz de correlación gráfica
#'
#' Esta función crea una matriz de correlación gráfica entre las variables métricas de un dataframe.
#' Por defecto, incluye todas las variables métricas del dataframe, pero también permite seleccionar
#' un subconjunto específico de variables.
#'
#' @param data Dataframe que contiene los datos.
#' @param ... Variables específicas (sin entrecomillar) para incluir en la matriz de correlación. Si no se especifica, se usan todas las variables métricas.
#' @return Una lista con el gráfico de correlación, guardada en el entorno global con el nombre "<nombre_df>_correlaciones_info".
#' @import GGally dplyr
#' @examples
#' \u0026dontrun{
#' # Correlaciones entre todas las variables métricas del dataframe mtcars
#' MATcor(data = mtcars)
#'
#' # Correlaciones entre un subconjunto específico de variables
#' MATcor(data = mtcars, mpg, hp, wt)
#' }
#' @export
MATcor <- function(data, ...) {
  # Verificar e instalar paquetes necesarios
  packages <- c("GGally", "dplyr")
  lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  })

  # Cargar paquetes
  library(GGally)
  library(dplyr)

  # Obtener el nombre del dataframe
  df_name <- deparse(substitute(data))

  # Seleccionar variables específicas si se proporcionan
  variables <- quos(...)
  if (length(variables) > 0) {
    data <- data %>% select(!!!variables)
  } else {
    # Seleccionar automáticamente las variables métricas
    data <- data %>% select(where(is.numeric))
  }

  # Verificar que queden variables métricas después de la selección
  if (ncol(data) < 2) {
    stop("El dataframe debe contener al menos dos variables métricas para calcular la correlación.")
  }

  # Crear el gráfico de correlación
  corr_plot <- ggpairs(
    data,
    lower = list(continuous = wrap("cor", size = 4.5, method = "pearson", stars = TRUE)),
    title = "Matriz de Correlación"
  )

  # Guardar el gráfico en el entorno global
  nombre_lista <- paste0(df_name, "_correlaciones_info")
  assign(nombre_lista, list(correlaciones = corr_plot), envir = .GlobalEnv)

  # Mensaje de confirmación
  message(paste("La matriz de correlación gráfica se guardó en el entorno global con el nombre:", nombre_lista))
}
