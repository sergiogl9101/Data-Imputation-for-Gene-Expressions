# Models/model_median.R
# Imputación de datos faltantes usando la mediana por columna

impute_median <- function(data, cols = NULL) {
  df <- data
  
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  
  for (col in cols) {
    if (!is.numeric(df[[col]])) next
    
    mediana <- median(df[[col]], na.rm = TRUE)
    na_idx  <- is.na(df[[col]])
    df[na_idx, col] <- mediana
  }
  
  df
}