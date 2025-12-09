# Models/model_knn.R
# Imputación por KNN (single imputation, sin MICE)
# Implementación basada en distancia euclidiana usando
# las demás columnas numéricas como features.

impute_knn <- function(data, cols = NULL, k = 5) {
  df <- data
  
  # Selección de columnas numéricas si no se especifican
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  if (length(cols) == 0) return(df)
  
  X <- as.matrix(df[, cols, drop = FALSE])
  n <- nrow(X)
  p <- ncol(X)
  
  # ---- Saneamos y fijamos k global ----
  k_raw <- k
  k <- suppressWarnings(as.integer(k))
  if (!is.finite(k) || k < 1L) k <- 5L
  
  cat(
    sprintf(
      "[impute_knn] Using KNN (single imputation) with k = %d (raw input: %s) on %d rows x %d numeric columns.\n",
      k,
      as.character(k_raw),
      n,
      p
    )
  )
  
  # Para cada columna numérica con NA
  for (j in seq_len(p)) {
    col_j   <- X[, j]
    miss_idx <- which(is.na(col_j))
    obs_idx  <- which(!is.na(col_j))
    
    if (length(miss_idx) == 0 || length(obs_idx) == 0) next
    
    # Si solo hay una columna numérica, no hay features;
    # usamos simplemente la media de la columna.
    if (p == 1) {
      X[miss_idx, j] <- mean(col_j, na.rm = TRUE)
      next
    }
    
    # Features: todas las demás columnas numéricas
    feat_mat <- X[, -j, drop = FALSE]
    
    for (i in miss_idx) {
      xi <- feat_mat[i, ]
      
      # Distancias a las filas observadas
      dists <- sapply(obs_idx, function(r) {
        xr <- feat_mat[r, ]
        common <- !(is.na(xi) | is.na(xr))
        if (!any(common)) return(Inf)
        sqrt(sum((xi[common] - xr[common])^2))
      })
      
      finite <- is.finite(dists)
      if (!any(finite)) {
        # Si no hay vecinos válidos, usar la media
        X[i, j] <- mean(col_j, na.rm = TRUE)
        next
      }
      
      # Ajustamos k a la cantidad de vecinos válidos disponibles
      ord      <- order(dists[finite])
      k_eff    <- min(k, length(ord))
      idx_k    <- obs_idx[finite][ord[seq_len(k_eff)]]
      
      X[i, j] <- mean(col_j[idx_k], na.rm = TRUE)
    }
  }
  
  df[, cols] <- X
  
  cat(
    sprintf(
      "[impute_knn] Imputation finished. Final k used (per row) was <= %d depending on available neighbors.\n",
      k
    )
  )
  
  df
}
