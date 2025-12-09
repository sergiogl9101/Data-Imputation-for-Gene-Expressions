# R/hp_suggest.R
# Helpers to suggest hyperparameters based on the current numeric dataset

# ---- 1) Suggest K for KNN (heuristic + tamaño de muestra) ----
suggest_knn_k <- function(df_num, max_k = 20L) {
  df_num <- as.data.frame(df_num, check.names = FALSE, stringsAsFactors = FALSE)
  n <- nrow(df_num)
  if (n <= 5) return(3L)
  
  # Usar filas con pocas NAs para estimar "n efectivo"
  row_na <- rowMeans(is.na(df_num))
  idx    <- which(row_na <= 0.10)   # hasta 10% NA en la fila
  n_eff  <- length(idx)
  if (n_eff < 10) n_eff <- n        # si hay pocas, usar n completo
  
  # Regla tipo sqrt(n) acotada
  k <- floor(sqrt(n_eff))
  k <- max(3L, min(as.integer(k), max_k))
  k
}

# ---- 2) Suggest #components for BPCA (por varianza explicada) ----
suggest_bpca_ncomp <- function(df_num, var_threshold = 0.90, max_comp = 10L) {
  df_num <- as.data.frame(df_num, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Usar solo casos completos para estimar la estructura
  cc <- stats::complete.cases(df_num)
  if (sum(cc) < 5) {
    # Muy pocos casos completos → algo conservador
    return(2L)
  }
  
  x <- scale(df_num[cc, , drop = FALSE])
  
  # Matriz de covarianza y eigenvalues
  cov_mat <- stats::cov(x, use = "pairwise.complete.obs")
  ev      <- tryCatch(eigen(cov_mat, only.values = TRUE)$values,
                      error = function(e) NA_real_)
  ev      <- ev[is.finite(ev) & ev > 0]
  if (length(ev) == 0) {
    return(2L)
  }
  
  var_explained <- cumsum(ev) / sum(ev)
  k <- which(var_explained >= var_threshold)[1]
  if (is.na(k)) {
    k <- min(ncol(df_num), max_comp)
  }
  
  k <- max(2L, min(as.integer(k), max_comp, ncol(df_num)))
  k
}

# ---- 3) Suggest tolerance for LS Impute (según % global de NA) ----
suggest_ls_tolerance <- function(df_num) {
  m <- mean(is.na(as.matrix(df_num)))
  if (!is.finite(m)) return(1e-4)
  
  # m es proporción [0,1]
  if (m < 0.05)  return(1e-5)  # pocos NA → tolerancia más estricta
  if (m < 0.15)  return(1e-4)
  if (m < 0.30)  return(5e-4)
  if (m < 0.45)  return(1e-3)
  # Por arriba de esto en teoría ya lo rebotaste en upload, pero por si acaso:
  5e-3
}
