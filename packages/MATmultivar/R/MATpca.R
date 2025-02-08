# Archivo: R/MATpca.R

#' MATpca: Análisis de Componentes Principales
#'
#' Realiza un Análisis de Componentes Principales (PCA) sobre variables métricas.
#' Devuelve tablas de resumen y gráficos de autovalores y varianza acumulada.
#'
#' @param data Dataframe con los datos.
#' @param ... Variables específicas (sin comillas) a incluir en el análisis.
#' @return Lista con tablas de resumen y gráficos.
#' @import dplyr ggplot2 patchwork knitr kableExtra rlang
#' @export
MATpca <- function(data, ...) {
  required_packages <- c("dplyr", "ggplot2", "patchwork", "knitr", "kableExtra", "rlang")
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  })
  
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(rlang)
  
  variables <- rlang::quos(...)
  selected_data <- if (length(variables) == 0) data %>% select(where(is.numeric)) else data %>% select(!!!variables)
  
  if (ncol(selected_data) < 2) stop("Se necesitan al menos dos variables numéricas para realizar el PCA.")
  
  pca_result <- prcomp(selected_data, scale = TRUE)
  summary_df <- as.data.frame(t(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100))
  colnames(summary_df) <- paste0("PC", 1:ncol(summary_df))
  summary_df <- rbind("Desviación típica" = pca_result$sdev, "Proporción de varianza" = summary_df[1, ], "Varianza acumulada" = cumsum(summary_df[1, ]))
  
  tabla_resumen <- summary_df %>%
    kable(caption = "Resumen de Componentes", format.args = list(decimal.mark = ".", digits = 4)) %>%
    kable_styling(bootstrap_options = c("striped", "bordered"), full_width = FALSE)
  
  resultado_info <- list(
    resumen_componentes = tabla_resumen,
    pca = pca_result
  )
  return(resultado_info)
}

