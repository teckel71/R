# Archivo: R/MATpcascores.R

#' MATpcascores: Obtención de puntuaciones de componentes principales
#'
#' Calcula y devuelve puntuaciones de componentes principales de un PCA previamente realizado.
#'
#' @param data Dataframe original.
#' @param pca_object Objeto resultante de prcomp().
#' @param num_comp Número de componentes a extraer.
#' @param num_cases Número de casos a mostrar.
#' @return Lista con el dataframe de puntuaciones y tabla formateada.
#' @import dplyr knitr kableExtra rlang
#' @export
MATpcascores <- function(data, pca_object, num_comp, num_cases = 0, ...) {
  if (!"prcomp" %in% class(pca_object)) stop("El objeto PCA no es válido.")
  
  pca_scores <- as.data.frame(pca_object$x[, 1:num_comp, drop = FALSE])
  scores_df <- cbind(data, pca_scores)
  
  if (num_cases > 0) scores_df <- scores_df %>% slice(1:num_cases)
  
  tabla_scores <- scores_df %>%
    kable(caption = "Puntuaciones de Componentes", format.args = list(decimal.mark = ".", digits = 4)) %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered"))
  
  return(list(scores_df = scores_df, tabla_scores = tabla_scores))
}
