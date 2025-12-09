# server/transform_pipeline.R
# Helpers genéricos para aplicar y deshacer el pipeline de transformaciones:
#   "log"        → log10 con offset adaptativo
#   "zscore"     → (x - mean) / sd
#   "yeojohnson" → bestNormalize::yeojohnson

if (!requireNamespace("bestNormalize", quietly = TRUE)) {
  stop("Package 'bestNormalize' is required for Yeo–Johnson. Install it with install.packages('bestNormalize').")
}

tp_num_mask <- function(df) {
  vapply(df, is.numeric, logical(1))
}

tp_auto_offset <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  v <- v[is.finite(v)]
  if (!length(v)) return(1e-6)
  m <- min(v, na.rm = TRUE)
  if (!is.finite(m)) return(1e-6)
  if (m > 0) return(0)
  abs(m) + 1e-6
}

# ---------- LOG10 ----------
tp_apply_log <- function(df) {
  df2 <- df
  nms <- names(df2)
  offsets <- numeric(length(nms))
  names(offsets) <- nms
  
  for (nm in nms) {
    v <- df2[[nm]]
    if (!is.numeric(v)) next
    off <- tp_auto_offset(v)
    offsets[[nm]] <- off
    df2[[nm]] <- suppressWarnings(log10(v + off))
  }
  list(
    data   = df2,
    params = list(offsets = offsets)
  )
}

tp_invert_log <- function(df_log, offsets) {
  df2 <- df_log
  common <- intersect(names(df2), names(offsets))
  for (nm in common) {
    v   <- df2[[nm]]
    if (!is.numeric(v)) next
    off <- offsets[[nm]]
    df2[[nm]] <- (10^v) - off
  }
  df2
}

# ---------- Z-SCORE ----------
tp_apply_z <- function(df) {
  df2 <- df
  nms <- names(df2)
  means <- numeric(length(nms))
  sds   <- numeric(length(nms))
  names(means) <- nms
  names(sds)   <- nms
  
  for (nm in nms) {
    v <- df2[[nm]]
    if (!is.numeric(v)) next
    m <- mean(v, na.rm = TRUE)
    s <- stats::sd(v, na.rm = TRUE)
    means[[nm]] <- m
    sds[[nm]]   <- s
    if (!is.finite(s) || s == 0) {
      df2[[nm]] <- v
    } else {
      df2[[nm]] <- (v - m) / s
    }
  }
  list(
    data   = df2,
    params = list(means = means, sds = sds)
  )
}

tp_invert_z <- function(df_z, means, sds) {
  df2   <- df_z
  nms   <- intersect(names(df2), names(means))
  nms   <- intersect(nms, names(sds))
  for (nm in nms) {
    v <- df2[[nm]]
    if (!is.numeric(v)) next
    m <- means[[nm]]
    s <- sds[[nm]]
    if (!is.finite(m) || !is.finite(s) || s == 0) next
    df2[[nm]] <- v * s + m
  }
  df2
}

# ---------- YEO–JOHNSON ----------
tp_apply_yj <- function(df, auto = TRUE, lambda = NULL) {
  df2  <- df
  nms  <- names(df2)
  fits <- vector("list", length(nms))
  names(fits) <- nms
  
  for (nm in nms) {
    v <- df2[[nm]]
    if (!is.numeric(v)) next
    fit <- if (isTRUE(auto)) {
      bestNormalize::yeojohnson(v, standardize = FALSE)
    } else {
      # compatibilidad lam / lambda
      tryCatch(
        bestNormalize::yeojohnson(v, standardize = FALSE, lam = lambda),
        error = function(e)
          bestNormalize::yeojohnson(v, standardize = FALSE, lambda = lambda)
      )
    }
    df2[[nm]] <- as.numeric(predict(fit, newdata = v))
    fits[[nm]] <- fit
  }
  list(
    data   = df2,
    params = list(fits = fits)
  )
}

tp_invert_yj <- function(df_yj, fits) {
  df2   <- df_yj
  nms   <- intersect(names(df2), names(fits))
  for (nm in nms) {
    fit <- fits[[nm]]
    if (is.null(fit)) next
    v <- df2[[nm]]
    if (!is.numeric(v)) next
    df2[[nm]] <- as.numeric(predict(fit, newdata = v, inverse = TRUE))
  }
  df2
}

# ---------- PIPELINE COMPLETO ----------
# pipeline = c("log", "zscore", "yeojohnson", ...)
# meta = lista de pasos con type + params (para poder invertir en orden inverso)

apply_transform_pipeline <- function(df, pipeline,
                                     yj_auto = TRUE,
                                     yj_lambda = NULL) {
  df_cur <- df
  steps_meta <- vector("list", length(pipeline))
  
  if (length(pipeline) == 0L) {
    return(list(data = df_cur, meta = list()))
  }
  
  for (i in seq_along(pipeline)) {
    step <- pipeline[[i]]
    res <- switch(
      step,
      "log"        = tp_apply_log(df_cur),
      "zscore"     = tp_apply_z(df_cur),
      "yeojohnson" = tp_apply_yj(df_cur, auto = yj_auto, lambda = yj_lambda),
      # cualquier paso desconocido: no hacer nada, pero guardar tipo
      list(data = df_cur, params = list())
    )
    df_cur <- res$data
    steps_meta[[i]] <- list(
      type   = step,
      params = res$params
    )
  }
  
  list(
    data = df_cur,
    meta = steps_meta
  )
}

invert_transform_pipeline <- function(df_trans, pipeline, meta) {
  df_cur <- df_trans
  
  if (length(pipeline) == 0L) return(df_cur)
  if (length(meta) != length(pipeline)) {
    stop("Length of 'meta' does not match length of 'pipeline'.")
  }
  
  # Recorremos el pipeline al revés para deshacer las transformaciones
  for (i in rev(seq_along(pipeline))) {
    step <- pipeline[[i]]
    pars <- meta[[i]]$params %||% list()
    
    df_cur <- switch(
      step,
      "log"        = tp_invert_log(df_cur, offsets = pars$offsets),
      "zscore"     = tp_invert_z(df_cur,  means   = pars$means, sds = pars$sds),
      "yeojohnson" = tp_invert_yj(df_cur, fits    = pars$fits),
      df_cur
    )
  }
  
  df_cur
}
