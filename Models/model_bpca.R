# Models/model_bpca.R
impute_bpca <- function(data, cols = NULL, ncomp = 2, center = TRUE, scale = "uv") {
  if (!requireNamespace("pcaMethods", quietly = TRUE)) {
    stop(
      "The 'pcaMethods' package is required for BPCA.\n",
      "Please install it with:\n",
      "  install.packages('BiocManager')\n",
      "  BiocManager::install('pcaMethods')"
    )
  }
  
  df <- data
  
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  if (length(cols) == 0) return(df)
  
  X <- as.matrix(df[, cols, drop = FALSE])
  
  # Saneamos ncomp
  ncomp <- suppressWarnings(as.integer(ncomp))
  if (!is.finite(ncomp) || ncomp < 1) ncomp <- 2L
  
  k_max <- min(nrow(X) - 1L, ncol(X))
  if (!is.finite(k_max) || k_max < 1L) k_max <- 1L
  if (ncomp > k_max) ncomp <- k_max
  
  # 👉 Mensaje informativo en consola
  message(
    sprintf(
      "[Imputation] Método: BPCA (pcaMethods::pca, method = 'bpca') | ncomp usado = %d | n = %d, p = %d",
      ncomp, nrow(X), ncol(X)
    )
  )
  
  bp <- pcaMethods::pca(
    X,
    method = "bpca",
    nPcs   = ncomp,
    center = center,
    scale  = scale
  )
  
  X_imp <- pcaMethods::completeObs(bp)
  df[, cols] <- X_imp
  df
}
