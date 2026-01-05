# Models/model_mice.R
# Imputación múltiple tipo "MICE-like" usando TUS modelos base
# Aplica a:
#   - mean_mice, median_mice, linear_mice   (estadísticos)
#   - knn_mice, bpca_mice, ls_mice         (ML)
#
# Todos usan lógica:
#   X_new^(j) = X_predicho + epsilon
#   epsilon ~ N(0, sigma_col^2) solo en las celdas que originalmente eran NA.

run_mice_imputation <- function(
    data,
    model_code,
    m             = 5L,
    maxit         = 10L,
    knn_neighbors = 5L,
    bpca_ncomp    = 2L,
    ls_tol        = 1e-4
) {
  # 0) Sanitizar data
  df <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Máscara de columnas numéricas
  num_mask <- vapply(df, is.numeric, logical(1))
  num_cols <- names(df)[num_mask]
  
  if (length(num_cols) == 0) {
    stop("No numeric columns available for multiple imputation.")
  }
  
  # --------------------------------------------------------------------------
  # 1) Imputación base con modelos single-imputation
  # --------------------------------------------------------------------------
  impute_base <- switch(
    model_code,
    
    # ===== Estadísticos =====
    "mean_mice" = {
      if (!exists("impute_mean")) {
        stop("La función 'impute_mean' no está disponible en el entorno.")
      }
      impute_mean(df, cols = num_cols)
    },
    
    "median_mice" = {
      if (!exists("impute_median")) {
        stop("La función 'impute_median' no está disponible en el entorno.")
      }
      impute_median(df, cols = num_cols)
    },
    
    "linear_mice" = {
      if (!exists("impute_regression")) {
        stop("La función 'impute_regression' no está disponible en el entorno.")
      }
      impute_regression(df)
    },
    
    # ===== ML =====
    "knn_mice" = {
      if (!exists("impute_knn")) {
        stop("La función 'impute_knn' no está disponible en el entorno.")
      }
      impute_knn(df, cols = num_cols, k = as.integer(knn_neighbors))
    },
    
    "bpca_mice" = {
      if (!exists("impute_bpca")) {
        stop("La función 'impute_bpca' no está disponible en el entorno.")
      }
      impute_bpca(df, cols = num_cols, ncomp = as.integer(bpca_ncomp))
    },
    
    "ls_mice" = {
      if (!exists("impute_ls")) {
        stop("La función 'impute_ls' no está disponible en el entorno.")
      }
      impute_ls(df, cols = num_cols, rank = 2, maxit = 20, tol = ls_tol)
    },
    
    stop(sprintf("Unknown MICE-like model_code: '%s'", model_code))
  )
  
  # --------------------------------------------------------------------------
  # 2) Patrón de NAs + cálculo de sigma de ruido por columna
  # --------------------------------------------------------------------------
  # Patrón de NA solo de las columnas numéricas
  na_pattern <- is.na(df[, num_cols, drop = FALSE])
  
  # Desviación estándar del "ruido" por columna, basada en residuales
  noise_sd_per_col <- numeric(length(num_cols))
  names(noise_sd_per_col) <- num_cols
  
  for (col in num_cols) {
    original_col <- df[[col]]
    pred_col     <- impute_base[[col]]
    
    # Usamos sólo filas con dato original (no NA)
    obs_mask <- !is.na(original_col)
    sd_resid <- NA_real_
    
    if (sum(obs_mask) >= 3) {
      resid <- original_col[obs_mask] - pred_col[obs_mask]
      sd_resid <- stats::sd(resid, na.rm = TRUE)
    }
    
    # Fallback si no hay residuales decentes:
    # sd_resid ≈ 5% de la variabilidad del predicho
    if (!is.finite(sd_resid) || sd_resid <= 0) {
      sd_resid <- 0.05 * stats::sd(pred_col, na.rm = TRUE)
    }
    
    if (!is.finite(sd_resid) || sd_resid <= 0) {
      # Último fallback por si la columna es constante
      sd_resid <- 0
    }
    
    noise_sd_per_col[col] <- sd_resid
  }
  
  # --------------------------------------------------------------------------
  # 3) Generar m datasets imputados:
  #    X_new = X_predicho + epsilon
  #    epsilon ~ N(0, sigma_col^2) SOLO en NAs originales
  # --------------------------------------------------------------------------
  imputed_list <- vector("list", length = m)
  
  for (j in seq_len(m)) {
    imp_j <- impute_base
    
    for (col in num_cols) {
      na_mask_col <- na_pattern[, col]
      if (!any(na_mask_col)) next  # no había NAs en esta columna
      
      sd_noise <- noise_sd_per_col[col]
      if (!is.finite(sd_noise) || sd_noise <= 0) next  # sin ruido para esta col
      
      # Valores predichos base (X_predicho)
      col_pred <- imp_j[[col]]
      
      # epsilon ~ N(0, sd_noise^2) para cada NA original
      eps <- stats::rnorm(sum(na_mask_col), mean = 0, sd = sd_noise)
      
      # Aplicamos: X_new = X_predicho + epsilon, SOLO donde había NAs
      col_pred[na_mask_col] <- col_pred[na_mask_col] + eps
      imp_j[[col]] <- col_pred
    }
    
    imputed_list[[j]] <- imp_j
  }
  
  # --------------------------------------------------------------------------
  # 4) Dataset pooled: promedio por fila en columnas numéricas
  # --------------------------------------------------------------------------
  pooled <- imputed_list[[1]]
  
  if (m > 1) {
    for (col in names(pooled)) {
      if (is.numeric(pooled[[col]])) {
        # Construimos matriz (filas = obs, columnas = m)
        mat_col <- sapply(imputed_list, function(dd) dd[[col]])
        pooled[[col]] <- rowMeans(as.matrix(mat_col), na.rm = TRUE)
      }
    }
  }
  
  # --------------------------------------------------------------------------
  # 5) Diagnóstico simple: media global de todas las columnas numéricas por dataset
  # --------------------------------------------------------------------------
  num_indices <- which(num_mask)
  
  diag_vals <- sapply(imputed_list, function(dd) {
    vals <- as.matrix(dd[, num_indices, drop = FALSE])
    mean(vals, na.rm = TRUE)
  })
  
  diag_df <- data.frame(
    iter  = seq_len(m),
    value = as.numeric(diag_vals)
  )
  
  # --------------------------------------------------------------------------
  # 6) Salida
  # --------------------------------------------------------------------------
  list(
    data        = pooled,
    diagnostics = diag_df
  )
}
