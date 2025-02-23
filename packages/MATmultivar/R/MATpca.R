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
  # Comprobar e instalar paquetes necesarios dentro de la función
  paquetes_necesarios <- c("dplyr", "ggplot2", "patchwork", "knitr", "kableExtra", "rlang")
  
  for (paquete in paquetes_necesarios) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete)
    }
  }
  
  # Cargar librerías
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(rlang)
  
  # Capturar las variables seleccionadas sin necesidad de comillas
  variables <- rlang::quos(...)
  
  if (length(variables) == 0) {
    # Si no se especifican variables, seleccionar solo las métricas
    selected_data <- data %>% select(where(is.numeric))
  } else {
    # Seleccionar las variables especificadas
    selected_data <- data %>% select(!!!variables)
  }
  
  # Verificar si hay suficientes variables métricas para el análisis
  if (ncol(selected_data) < 2) {
    stop("Se necesitan al menos dos variables numéricas para realizar el análisis de componentes principales.")
  }
  
  # Análisis de Componentes Principales con escalado
  pca_result <- prcomp(selected_data, scale = TRUE)
  
  # Calcular proporción de varianza explicada y varianza acumulada
  proporcion_varianza <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
  
  # Crear tabla de resumen de componentes con la corrección integrada
  summary_df <- data.frame(
    `Desviación típica` = pca_result$sdev,
    `Proporción de varianza` = proporcion_varianza,
    `Varianza acumulada` = cumsum(proporcion_varianza)
  )
  
  # Renombrar columnas acorde a los componentes principales
  colnames(summary_df) <- paste0("PC", 1:ncol(summary_df))
  
  # Crear tabla con kable
  tabla_resumen <- summary_df %>%
    kable(caption = "Resumen de Componentes",
          format.args = list(decimal.mark = ".", digits = 4)) %>%
    kable_styling(bootstrap_options = c("striped", "bordered", "condensed"),
                  full_width = FALSE, position = "center") %>%
    row_spec(0, bold= TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, extra_css = "text-align: left;")
  
  # Cargas de componentes
  cargas <- as.data.frame(pca_result$rotation)
  tabla_cargas <- cargas %>%
    kable(caption = "Cargas de las Componentes",
          format.args = list(decimal.mark = ".", digits = 4)) %>%
    kable_styling(bootstrap_options = c("striped", "bordered", "condensed"),
                  full_width = FALSE, position = "center") %>%
    row_spec(0, bold= TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, extra_css = "text-align: left;")
  
  # Autovalores
  autovalores <- data.frame(
    Componente = 1:length(pca_result$sdev),
    Autovalor = pca_result$sdev^2
  )
  
  autograph <- ggplot(autovalores, aes(x = Componente, y = Autovalor)) +
    geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
    scale_x_continuous(breaks = 1:nrow(autovalores)) +
    geom_hline(yintercept = 1, colour = "darkblue") +
    geom_text(aes(label = round(Autovalor, 2)), vjust = -0.5, colour = "darkblue", size = 3) +
    ggtitle("Autovalores de las Componentes") +
    xlab("Número de Componente") +
    ylab("Autovalor")
  
  # Varianza acumulada
  autovalores <- autovalores %>%
    mutate(Varianza_Acumulada = cumsum(Autovalor) / sum(Autovalor) * 100)
  
  vacumgraph <- ggplot(autovalores, aes(x = Componente, y = Varianza_Acumulada)) +
    geom_bar(stat = "identity", colour = "red", fill = "lightblue", alpha = 0.7) +
    scale_x_continuous(breaks = 1:nrow(autovalores)) +
    geom_text(aes(label = round(Varianza_Acumulada, 2)), vjust = -0.5, colour = "darkblue", size = 3) +
    ggtitle("Varianza Acumulada por Componentes") +
    xlab("Número de Componente") +
    ylab("Varianza Acumulada (%)")
  
  # Combinar gráficos con patchwork
  plot_combinado <- autograph / vacumgraph
  
  # Crear lista de salida con tablas y gráficos
  resultado_info <- list(
    resumen_componentes = tabla_resumen,
    cargas_componentes = tabla_cargas,
    graficos = plot_combinado
  )
  
  # Retornar una lista que contiene el PCA y la información
  return(list(pca = pca_result, info = resultado_info))
}
