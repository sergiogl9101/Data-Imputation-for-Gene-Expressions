# R/utils.R
# Funciones de utilidad para enmascaramiento y evaluación de imputación

# Enmascaramiento aleatorio de un data.frame numérico
mask_random <- function(df, prop = 0.2) {
  df_mat <- as.matrix(df)
  n <- nrow(df_mat)
  p <- ncol(df_mat)
  
  total <- n * p
  m <- floor(total * prop)      # número de celdas a enmascarar
  
  idx  <- sample(total, m)
  rows <- ((idx - 1) %/% p) + 1
  cols <- ((idx - 1) %%  p) + 1
  
  X_true   <- df_mat
  X_masked <- df_mat
  X_masked[cbind(rows, cols)] <- NA
  
  mask <- matrix(FALSE, n, p)
  mask[cbind(rows, cols)] <- TRUE
  
  list(
    X_true   = X_true,
    X_masked = X_masked,
    mask     = mask
  )
}

# Métricas de recuperación (MSE, RMSE, MAE, R2)
metricas_recuperacion <- function(y_true, y_pred) {
  # Quitamos cualquier par con NA en true o pred
  ok <- !(is.na(y_true) | is.na(y_pred))
  y_true <- y_true[ok]
  y_pred <- y_pred[ok]
  
  # Si después de filtrar no queda nada, devolvemos NA en todo
  if (length(y_true) == 0) {
    return(data.frame(
      mse  = NA_real_,
      rmse = NA_real_,
      mae  = NA_real_,
      r2   = NA_real_
    ))
  }
  
  mse  <- mean((y_true - y_pred)^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(y_true - y_pred))
  
  sse <- sum((y_true - y_pred)^2)
  sst <- sum((y_true - mean(y_true))^2)
  r2  <- 1 - sse / sst
  
  data.frame(
    mse  = mse,
    rmse = rmse,
    mae  = mae,
    r2   = r2
  )
}

# Evaluador genérico de modelos de imputación
# Soporta dos modos:
#  - Modo clásico (sin pipeline): impute_fun trabaja en ESCALA ORIGINAL
#  - Modo pipeline (con pipeline != NULL):
#       ✔️ Paso 1: usar df original
#       ✔️ Paso 2: enmascarar en escala original
#       ✔️ Paso 3: aplicar transformaciones solo al df enmascarado
#       ✔️ Paso 4: imputar en escala transformada (impute_fun)
#       ✔️ Paso 5: invertir transformaciones
#       ✔️ Paso 6: evaluar en escala original
evaluate_imputer <- function(df, impute_fun, model_name,
                             props    = c(0.1, 0.2, 0.3),
                             n_rep    = 5,
                             pipeline = NULL,
                             yj_auto  = TRUE,
                             yj_lambda = NULL,
                             ...) {
  # Aseguramos data.frame limpio
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  df_mat <- as.matrix(df)
  
  res_list <- list()
  
  # ¿Usamos pipeline aquí o dejamos que lo haga impute_fun?
  use_pipeline <- !is.null(pipeline) && length(pipeline) > 0
  
  for (prop in props) {
    for (i in seq_len(n_rep)) {
      # ✔️ Paso 1–2: enmascarar en escala ORIGINAL
      masked <- mask_random(df_mat, prop = prop)
      X_true   <- masked$X_true
      X_masked <- masked$X_masked
      mask     <- masked$mask
      
      # df_masked en escala original (con NAs introducidos)
      df_masked <- as.data.frame(
        X_masked,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      # ------ Pasos 3–5: sólo si queremos que utils se encargue del pipeline ------
      if (use_pipeline) {
        # ✔️ Paso 3 — aplicar transformaciones solo al dataset con NAs
        tf <- apply_transform_pipeline(
          df       = df_masked,
          pipeline = pipeline,
          yj_auto  = yj_auto,
          yj_lambda = yj_lambda
        )
        df_trans_masked <- tf$data
        meta_steps      <- tf$meta
        
        # ✔️ Paso 4 — imputar en la escala transformada
        df_imputed_trans <- tryCatch(
          impute_fun(df_trans_masked, ...),
          error = function(e) {
            warning(sprintf(
              "evaluate_imputer: error en impute_fun (prop=%.2f, rep=%d): %s",
              prop, i, e$message
            ))
            return(NULL)
          }
        )
        
        if (is.null(df_imputed_trans)) {
          next
        }
        
        # ✔️ Paso 5 — invertir transformaciones → volver a ESCALA ORIGINAL
        df_imputed <- tryCatch(
          invert_transform_pipeline(
            df_trans = df_imputed_trans,
            pipeline = pipeline,
            meta     = meta_steps
          ),
          error = function(e) {
            warning(sprintf(
              "evaluate_imputer: error al invertir pipeline (prop=%.2f, rep=%d): %s",
              prop, i, e$message
            ))
            return(NULL)
          }
        )
        
        if (is.null(df_imputed)) {
          next
        }
        
      } else {
        # Modo clásico (compatibilidad):
        # impute_fun ya se encarga internamente de log/z/YJ si corresponde.
        df_imputed <- tryCatch(
          impute_fun(df_masked, ...),
          error = function(e) {
            warning(sprintf(
              "evaluate_imputer: error en impute_fun (prop=%.2f, rep=%d): %s",
              prop, i, e$message
            ))
            return(NULL)
          }
        )
        
        if (is.null(df_imputed)) {
          next
        }
      }
      
      X_imputed <- as.matrix(df_imputed)
      
      # Seguridad: si cambian las dimensiones, no intentamos evaluar
      if (!all(dim(X_imputed) == dim(X_true))) {
        warning("evaluate_imputer: dimensiones de X_imputed no coinciden con X_true; se omite esta réplica.")
        next
      }
      
      # ✔️ Paso 6 — Evaluar en escala ORIGINAL (X_true vs X_imputed en esas celdas)
      y_true <- X_true[mask]
      y_pred <- X_imputed[mask]
      
      met <- metricas_recuperacion(y_true, y_pred)
      met$prop  <- prop
      met$rep   <- i
      met$model <- model_name
      
      res_list[[length(res_list) + 1L]] <- met
    }
  }
  
  if (length(res_list) == 0) {
    return(data.frame(
      mse   = NA_real_,
      rmse  = NA_real_,
      mae   = NA_real_,
      r2    = NA_real_,
      prop  = NA_real_,
      rep   = NA_integer_,
      model = model_name
    ))
  }
  
  do.call(rbind, res_list)
}
