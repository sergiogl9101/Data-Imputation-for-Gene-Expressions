# Models/model_ls.R
# LS-Impute aproximado mediante reconstrucción de bajo rango (SVD)

impute_ls <- function(data, cols = NULL, rank = 2, maxit = 20, tol = 1e-4) {
  df <- data
  
  # --- Selección de columnas numéricas ---
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  if (length(cols) == 0) return(df)
  
  X <- as.matrix(df[, cols, drop = FALSE])
  miss <- is.na(X)
  if (!any(miss)) {
    cat("[impute_ls] No missing values detected; returning original data.\n")
    return(df)
  }
  
  n <- nrow(X)
  p <- ncol(X)
  
  # --- Saneamos hiperparámetros ---
  rank_raw <- rank
  maxit_raw <- maxit
  tol_raw <- tol
  
  rank <- suppressWarnings(as.integer(rank))
  if (!is.finite(rank) || rank < 1L) rank <- 2L
  
  maxit <- suppressWarnings(as.integer(maxit))
  if (!is.finite(maxit) || maxit < 1L) maxit <- 20L
  
  tol <- suppressWarnings(as.numeric(tol))
  if (!is.finite(tol) || tol <= 0) tol <- 1e-4
  
  # rank no puede ser mayor que min(n, p)
  k_max <- min(n, p)
  if (!is.finite(k_max) || k_max < 1L) k_max <- 1L
  if (rank > k_max) rank <- k_max
  
  cat(
    sprintf(
      "[impute_ls] Using LS low-rank SVD imputation on %d x %d matrix.\n",
      n, p
    )
  )
  cat(
    sprintf(
      "[impute_ls] rank = %d (raw: %s), maxit = %d (raw: %s), tol = %g (raw: %s)\n",
      rank, as.character(rank_raw),
      maxit, as.character(maxit_raw),
      tol,  as.character(tol_raw)
    )
  )
  
  # --- Inicializamos NA con la media de la columna ---
  col_means <- colMeans(X, na.rm = TRUE)
  for (j in seq_len(ncol(X))) {
    idx <- which(is.na(X[, j]))
    if (length(idx) > 0)
      X[idx, j] <- col_means[j]
  }
  
  last_diff  <- NA_real_
  iter_used  <- 0L
  
  # --- Iteraciones de reconstrucción de bajo rango ---
  for (iter in seq_len(maxit)) {
    X_old <- X
    
    sv <- svd(X)
    d  <- sv$d
    
    if (rank < length(d)) {
      d[(rank + 1):length(d)] <- 0
    }
    
    X_hat <- sv$u %*% diag(d, nrow = length(d), ncol = length(d)) %*% t(sv$v)
    
    # Solo actualizamos las celdas originalmente faltantes
    X[miss] <- X_hat[miss]
    
    diff_max <- max(abs(X[miss] - X_old[miss]))
    last_diff <- diff_max
    iter_used <- iter
    
    if (!is.finite(diff_max) || diff_max < tol) {
      cat(
        sprintf(
          "[impute_ls] Converged at iteration %d with max change on missing entries = %.4e\n",
          iter, diff_max
        )
      )
      break
    }
  }
  
  if (iter_used == maxit && is.finite(last_diff) && last_diff >= tol) {
    cat(
      sprintf(
        "[impute_ls] Reached maxit = %d without strict convergence (last max change = %.4e).\n",
        maxit, last_diff
      )
    )
  }
  
  df[, cols] <- X
  df
}
