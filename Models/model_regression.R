# ==========================================================
#  impute_regression() — versión RIDGE (stable for proteomics)
#  Misma firma, mismo nombre, misma estructura general
# ==========================================================

impute_regression <- function(data, lambda = 1) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("The glmnet package is required for Ridge regression.")
  }
  
  df <- data
  cols <- names(df)[sapply(df, is.numeric)]
  
  # -------------------------------
  # 1) Imputación preliminar simple
  # -------------------------------
  # (necesaria porque glmnet NO admite NA)
  for (j in cols) {
    vec <- df[[j]]
    df[[j]][is.na(vec)] <- mean(vec, na.rm = TRUE)
  }
  
  # -------------------------------
  # 2) Ridge para cada columna
  # -------------------------------
  for (target in cols) {
    
    y <- df[[target]]
    
    # No intentar imputar si no hay NA en el dataset original
    original_y <- data[[target]]
    na_idx <- which(is.na(original_y))
    if (length(na_idx) == 0) next
    
    predictors <- setdiff(cols, target)
    X <- as.matrix(df[, predictors, drop = FALSE])
    
    # Entrenamiento Ridge (alpha = 0)
    fit <- glmnet::glmnet(
      x = X,
      y = y,
      alpha = 0,       # === RIDGE ===
      lambda = lambda
    )
    
    # Predicción sobre todo X
    preds <- predict(fit, newx = X, s = lambda)
    
    # Solo imputamos los NA verdaderos
    df[[target]][na_idx] <- preds[na_idx]
  }
  
  df
}
