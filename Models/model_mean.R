# Models/model_mean.R

#' Imputación de datos faltantes usando la media
#'
#' @param data data.frame con NA's
#' @param cols vector opcional de columnas a imputar
#' @return data.frame con NA imputados
impute_mean <- function(data, cols = NULL) {
  df <- data
  
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  
  for (col in cols) {
    if (!is.numeric(df[[col]])) next
    
    media  <- mean(df[[col]], na.rm = TRUE)
    na_idx <- is.na(df[[col]])
    df[na_idx, col] <- media
  }
  
  df
}
