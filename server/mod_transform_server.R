# Server/mod_transform_server.R
library(shiny)
library(shinyjs)
library(DT)
library(echarts4r)
library(e1071)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# NOTA: usaremos bestNormalize para Yeo-Johnson
suppressWarnings({
  if (!requireNamespace("bestNormalize", quietly = TRUE)) {
    stop("Falta el paquete 'bestNormalize'. Instálalo: install.packages('bestNormalize')")
  }
})

transform_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- HOW TO (Transform) | same pattern as Upload ----------
    
    # 1) Helper: YouTube iframe (embed)
    tutorial_iframe <- function(embed_url) {
      tags$div(
        style = "position:relative;padding-bottom:56.25%;height:0;overflow:hidden;border-radius:12px;",
        tags$iframe(
          src = embed_url,
          style = "position:absolute;top:0;left:0;width:100%;height:100%;",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = NA
        )
      )
    }
    
    # 2) Map: tab -> label + embed url
    howto_map <- list(
      before_after = list(
        label = "How to use: Before vs After",
        url   = "https://www.youtube.com/embed/Q3-cChcOjpI"
      ),
      summary_after = list(
        label = "How to use: Summary (after)",
        url   = "https://www.youtube.com/embed/m2RF7QfIYYY"
      )
    )
    
    # 3) Track current section (tab)
    howto_current_key <- reactiveVal("before_after")
    
    observeEvent(input$tfm_tabs, {
      key <- input$tfm_tabs
      if (!is.null(key) && nzchar(key) && !is.null(howto_map[[key]])) {
        howto_current_key(key)
      }
    }, ignoreInit = FALSE)
    
    # 4) Render ONE button, but with dynamic label
    output$howto_btn_ui <- renderUI({
      key <- howto_current_key()
      item <- howto_map[[key]] %||% howto_map[["before_after"]]
      
      actionButton(
        inputId = ns("howto_btn"),
        label   = item$label,
        icon    = icon("circle-play"),
        class   = "btn btn-info w-100",
        style   = "white-space: normal; text-align: left;"
      )
    })
    
    # 5) On click: open the modal with the correct video
    observeEvent(input$howto_btn, {
      key <- howto_current_key()
      item <- howto_map[[key]] %||% howto_map[["before_after"]]
      
      showModal(modalDialog(
        title = item$label,
        tutorial_iframe(item$url),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    })
    
    
    # ------------------ Helpers ------------------
    .deep_copy <- function(d) {
      if (is.null(d)) return(NULL)
      as.data.frame(lapply(d, function(x) { if (is.factor(x)) as.character(x) else x }),
                    check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    .only_numeric <- function(d) {
      if (is.null(d) || !is.data.frame(d)) return(data.frame())
      d2 <- d[, sapply(d, is.numeric), drop = FALSE]
      if (!ncol(d2)) d2 <- data.frame()
      d2
    }
    
    .auto_offset <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      v <- v[is.finite(v)]
      if (!length(v)) return(1e-6)
      m <- min(v, na.rm = TRUE)
      if (!is.finite(m)) return(1e-6)
      if (m > 0) return(0)
      abs(m) + 1e-6
    }
    
    .scale_cols <- function(df, center = TRUE, scale = TRUE) {
      if (is.null(df) || !ncol(df)) return(df)
      out <- lapply(df, function(col) {
        col <- suppressWarnings(as.numeric(col))
        if (!any(is.finite(col))) return(col)
        as.numeric(scale(col, center = center, scale = scale))
      })
      as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    # Función para calcular la curva normal basada en la media y desviación estándar
    .normal_curve <- function(mean, sd, x) {
      y <- dnorm(x, mean = mean, sd = sd)
      return(data.frame(x = x, y = y))
    }
    
    .yeojohnson_cols <- function(df) {
      if (is.null(df) || !ncol(df)) return(df)
      out <- list()
      for (nm in colnames(df)) {
        v <- suppressWarnings(as.numeric(df[[nm]]))
        if (all(!is.finite(v))) { out[[nm]] <- v; next }
        tr <- try(bestNormalize::yeojohnson(v, standardize = FALSE), silent = TRUE)
        out[[nm]] <- if (inherits(tr, "try-error")) v else tr$x.t
      }
      as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    .hist_df_fixed_bins <- function(x, bins = 30) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      h <- hist(x, breaks = bins, plot = FALSE)
      data.frame(x = h$mids, y = as.numeric(h$counts))
    }
    
    .density_df <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      d <- density(x, na.rm = TRUE)
      data.frame(x = d$x, y = d$y)
    }
    
    .flatten_all_numeric <- function(df) {
      if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(numeric())
      v <- suppressWarnings(as.numeric(as.matrix(df)))
      v[is.finite(v)]
    }
    
    .qq_df <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      x <- sort(x); n <- length(x)
      p <- (1:n - 0.5) / n
      theo <- qnorm(p)
      data.frame(theoretical = theo, sample = scale(x)[,1])
    }
    
    # --- NUEVO: helper para registrar pasos en el pipeline ---
    .add_step <- function(step_name) {
      rv$transform_pipeline <- c(rv$transform_pipeline, step_name)
    }
    
    # ------------------ State ------------------
    rv <- reactiveValues(
      base_df    = NULL,   # lo que llega desde upload (processed_df)
      current_df = NULL,   # lo que se va transformando
      z_df       = NULL,   # snapshot z-score
      yj_df      = NULL,   # snapshot YJ
      paired_zyj = FALSE,  # TRUE solo cuando se aplican juntas (Auto)
      # --- NUEVO: aquí guardamos el orden de transformaciones ---
      transform_pipeline = character(0)
    )
    rv$z_ready  <- FALSE
    rv$yj_ready <- FALSE
    
    # ================== INGESTA DESDE upload ==================
    observe({
      tfm_src <- session$userData$tfm_base
      if (is.null(tfm_src)) return()
      d <- tryCatch(
        if (is.reactive(tfm_src)) tfm_src() else tfm_src,
        error = function(e) NULL
      )
      if (is.null(d) || !is.data.frame(d) || !ncol(d)) return()
      
      d <- .deep_copy(d) |> .only_numeric()
      if (!ncol(d)) showNotification("No numeric columns in dataset", type = "warning")
      rv$base_df    <- d
      rv$current_df <- .deep_copy(d)
      rv$z_df <- NULL; rv$yj_df <- NULL; rv$paired_zyj <- FALSE
      rv$z_ready <- FALSE; rv$yj_ready <- FALSE
      
      # --- NUEVO: dataset nuevo → pipeline limpio ---
      rv$transform_pipeline <- character(0)
    })
    
    observeEvent(session$input[["tfm_reload_trigger"]], {
      tfm_src <- session$userData$tfm_base
      if (is.null(tfm_src)) return()
      d <- tryCatch(if (is.reactive(tfm_src)) tfm_src() else tfm_src, error = function(e) NULL)
      if (is.null(d) || !is.data.frame(d)) return()
      d <- .deep_copy(d) |> .only_numeric()
      rv$base_df    <- d
      rv$current_df <- .deep_copy(d)
      rv$z_df <- NULL; rv$yj_df <- NULL; rv$paired_zyj <- FALSE
      rv$z_ready <- FALSE; rv$yj_ready <- FALSE
      
      # --- NUEVO: recarga → pipeline limpio ---
      rv$transform_pipeline <- character(0)
    }, ignoreInit = TRUE)
    
    # ================== BOTONES ==================
    observeEvent(input$tfm_reset_btn, {
      validate(need(!is.null(rv$base_df) && ncol(rv$base_df), "No base to reset"))
      rv$current_df <- .deep_copy(rv$base_df)
      rv$z_df <- NULL; rv$yj_df <- NULL; rv$paired_zyj <- FALSE
      showNotification("Returning to original dataset", type = "message")
      rv$z_df <- NULL; rv$yj_df <- NULL
      rv$z_ready <- FALSE; rv$yj_ready <- FALSE
      
      # --- NUEVO: reset → pipeline vacío ---
      rv$transform_pipeline <- character(0)
    })
    
    observeEvent(input$tfm_log_apply, {
      validate(need(!is.null(rv$current_df) && ncol(rv$current_df), "No data to transform."))
      df <- .deep_copy(rv$current_df)
      offs <- vapply(df, .auto_offset, numeric(1))
      for (j in seq_along(df)) {
        v <- suppressWarnings(as.numeric(df[[j]]))
        df[[j]] <- suppressWarnings(log10(v + offs[[j]]))
      }
      rv$current_df <- df
      rv$paired_zyj <- FALSE
      showNotification("Log10 applied with adaptative offset.", type = "message")
      
      # --- NUEVO: registrar paso ---
      .add_step("log")
    })
    
    observeEvent(input$tfm_norm_apply, {
      validate(need(!is.null(rv$current_df) && ncol(rv$current_df), "No data to transform."))
      df <- .scale_cols(.deep_copy(rv$current_df), TRUE, TRUE)
      rv$current_df <- df
      rv$z_df <- .deep_copy(df)
      rv$paired_zyj <- FALSE
      showNotification("Z-score applied", type = "message")
      validate(need(is.data.frame(rv$z_df), "Can't generate Z-score."))
      
      rv$z_ready  <- TRUE
      rv$yj_ready <- FALSE  # invalidar YJ previo si existía
      
      # --- NUEVO ---
      .add_step("zscore")
    })
    
    .apply_yj_df <- function(d, auto = TRUE, lambda = 0) {
      stopifnot(is.data.frame(d))
      num <- vapply(d, is.numeric, logical(1))
      out <- d
      if (!any(num)) return(out)
      
      for (nm in names(d)[num]) {
        v <- d[[nm]]
        fit <- if (isTRUE(auto)) {
          bestNormalize::yeojohnson(v, standardize = FALSE)
        } else {
          tryCatch(
            bestNormalize::yeojohnson(v, standardize = FALSE, lam = lambda),
            error = function(e) bestNormalize::yeojohnson(v, standardize = FALSE, lambda = lambda)
          )
        }
        out[[nm]] <- as.numeric(predict(fit, newdata = v))
      }
      out
    }
    
    
    observeEvent(input$tfm_yj_apply, {
      # Asegurar que ya se aplicó Z-score (flujo manual)
      if (!isTRUE(rv$paired_zyj) && !isTRUE(rv$z_ready)) {
        shiny::showNotification("First apply Z-score and then Yeo–Johnson.", type = "error")
        return(invisible(NULL))
      }
      
      req(rv$z_df)  # YJ SIEMPRE sobre Z-score
      
      # Calcular Yeo–Johnson sobre el Z-score
      rv$yj_df <- .apply_yj_df(
        rv$z_df,
        auto   = isTRUE(input$tfm_yj_auto),
        lambda = input$tfm_yj_lambda
      )
      validate(need(is.data.frame(rv$yj_df), "Could not generate Yeo–Johnson."))
      
      # Marcar que viene del Z-score (para el plot comparativo)
      attr(rv$yj_df, "from") <- "z"
      rv$yj_ready <- TRUE
      
      rv$current_df <- .deep_copy(rv$yj_df)
      
      shiny::showNotification(
        "Yeo–Johnson applied on top of Z-score. Current dataset updated.",
        type = "message"
      )
      
      # --- NUEVO ---
      .add_step("yeojohnson")
    })
    
    
    observeEvent(input$tfm_auto_btn, {
      validate(need(!is.null(rv$current_df) && ncol(rv$current_df), "No hay data para transformar."))
      df <- .deep_copy(rv$current_df)
      
      # 1) Log
      offs <- vapply(df, .auto_offset, numeric(1))
      for (j in seq_along(df)) {
        v <- suppressWarnings(as.numeric(df[[j]]))
        df[[j]] <- suppressWarnings(log10(v + offs[[j]]))
      }
      
      # 2) Z-score
      df <- .scale_cols(df, TRUE, TRUE)
      rv$z_df <- .deep_copy(df)
      
      # 3) Yeo-Johnson (sobre Z)
      df <- .yeojohnson_cols(df)
      rv$yj_df <- .deep_copy(df)
      attr(rv$yj_df, "from") <- "z"
      
      rv$current_df <- df
      
      rv$z_ready    <- TRUE
      rv$yj_ready   <- TRUE
      rv$paired_zyj <- FALSE
      shinyjs::enable(ns("tfm_yj_apply"))
      
      # --- NUEVO: auto aplica las tres en este orden ---
      rv$transform_pipeline <- c(rv$transform_pipeline, "log", "zscore", "yeojohnson")
      
      showNotification("Auto transform applied: Log → Z-score → Yeo-Johnson.", type = "message")
    })
    
    
    # ================== CONTROLES ==================
    .plot_type <- reactive({ input$plot_type %||% "Histogram" })
    
    # ================== GRÁFICAS BEFORE/AFTER (ALL DATA) ==================
    output$tfm_ba_plot_before <- echarts4r::renderEcharts4r({
      req(rv$base_df); validate(need(ncol(rv$base_df) > 0, "No data available."))
      ptype <- .plot_type()
      x <- .flatten_all_numeric(rv$base_df); validate(need(length(x) > 0, "No numeric data in 'Before'."))
      
      if (ptype == "Histogram") {
        hb <- .hist_df_fixed_bins(x, bins = 30); validate(need(!is.null(hb), "Could not generate histogram (Before)."))
        echarts4r::e_charts(x, data = hb) |>
          echarts4r::e_bar(y, name = "Before") |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Count") |>
          echarts4r::e_x_axis(name = "All values (Before)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
      } else if (ptype == "Density") {
        db <- .density_df(x)
        validate(need(!is.null(db), "Could not generate density (Before)."))
        
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        
        if (is.finite(s) && s > 0) {
          db$normal_y <- stats::dnorm(db$x, mean = m, sd = s)
        } else {
          db$normal_y <- NA_real_
        }
        
        p <- echarts4r::e_charts(x, data = db) |>
          echarts4r::e_line(y, name = "Before", smooth = TRUE, showSymbol = FALSE) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Density") |>
          echarts4r::e_x_axis(name = "All values (Before)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
        
        if (any(is.finite(db$normal_y))) {
          p <- p |>
            echarts4r::e_line(normal_y, name = "Normal Curve",
                              smooth = TRUE,
                              lineStyle = list(type = "dashed"))
        }
        
        p
      } else if (ptype == "Boxplot") {
        df <- data.frame(group = "Before", value = x)
        echarts4r::e_charts(group, data = df) |>
          echarts4r::e_boxplot(value) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_y_axis(name = "All values (Before)") |>
          echarts4r::e_x_axis(name = NULL) |>
          echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%")
      } else { # QQ-plot (full dataset)
        dfq <- .qq_df(x); 
        validate(need(!is.null(dfq), "Could not generate QQ plot (Before)."))
        
        theoretical <- qnorm(ppoints(length(x)), mean = mean(x), sd = sd(x))
        
        echarts4r::e_charts(theoretical, data = dfq) |>
          echarts4r::e_scatter(sample, name = "Before") |>
          echarts4r::e_line(theoretical, name = "Normal Reference", smooth = TRUE,
                            lineStyle = list(type = "dashed", color = "red")) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_x_axis(name = "Theoretical Quantiles (Normal)") |>
          echarts4r::e_y_axis(name = "Sample Quantiles (Scaled)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
      }
    })
    
    output$tfm_ba_plot_after <- echarts4r::renderEcharts4r({
      req(rv$current_df); validate(need(ncol(rv$current_df) > 0, "No data available."))
      ptype <- .plot_type()
      x <- .flatten_all_numeric(rv$current_df); validate(need(length(x) > 0, "No numeric data in 'After'."))
      
      if (ptype == "Histogram") {
        ha <- .hist_df_fixed_bins(x, bins = 30); validate(need(!is.null(ha), "Could not generate histogram (After)."))
        echarts4r::e_charts(x, data = ha) |>
          echarts4r::e_bar(y, name = "After") |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Count") |>
          echarts4r::e_x_axis(name = "All values (After)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
      } else if (ptype == "Density") {
        da <- .density_df(x)
        validate(need(!is.null(da), "Could not generate density (After)."))
        
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        
        if (is.finite(s) && s > 0) {
          da$normal_y <- stats::dnorm(da$x, mean = m, sd = s)
        } else {
          da$normal_y <- NA_real_
        }
        
        p <- echarts4r::e_charts(x, data = da) |>
          echarts4r::e_line(y, name = "After", smooth = TRUE, showSymbol = FALSE) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Density") |>
          echarts4r::e_x_axis(name = "All values (After)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
        
        if (any(is.finite(da$normal_y))) {
          p <- p |>
            echarts4r::e_line(normal_y,
                              name = "Normal Curve",
                              smooth = TRUE,
                              lineStyle = list(type = "dashed"))
        }
        
        p
      } else if (ptype == "Boxplot") {
        df <- data.frame(group = "After", value = x)
        echarts4r::e_charts(group, data = df) |>
          echarts4r::e_boxplot(value) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_y_axis(name = "All values (After)") |>
          echarts4r::e_x_axis(name = NULL) |>
          echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%")
      } else { # QQ-plot (full dataset)
        dfq <- .qq_df(x)
        validate(need(!is.null(dfq), "Could not generate QQ plot (After)."))
        
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        n <- nrow(dfq) %||% length(x)
        
        theoretical <- if (is.finite(s) && s > 0 && n > 1) {
          stats::qnorm(stats::ppoints(n), mean = m, sd = s)
        } else {
          rep(NA_real_, n)
        }
        
        p <- echarts4r::e_charts(theoretical, data = dfq) |>
          echarts4r::e_scatter(sample, name = "After") |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_x_axis(name = "Theoretical Quantiles (Normal)") |>
          echarts4r::e_y_axis(name = "Sample Quantiles (Scaled)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
        
        if (any(is.finite(theoretical))) {
          p <- p |>
            echarts4r::e_line(theoretical,
                              name = "Normal Reference",
                              smooth = TRUE,
                              lineStyle = list(type = "dashed"))
        }
        
        p
      }
    })
    
    # ================== Z -SCORE VS YEO JOHNSON ==================
    
    .apply_yj_df <- function(d, auto = TRUE, lambda = 0) {
      stopifnot(is.data.frame(d))
      num <- vapply(d, is.numeric, logical(1))
      out <- d
      if (!any(num)) return(out)
      
      for (nm in names(d)[num]) {
        v <- d[[nm]]
        
        fit <- if (isTRUE(auto)) {
          bestNormalize::yeojohnson(v, standardize = FALSE)
        } else {
          tryCatch(
            bestNormalize::yeojohnson(v, standardize = FALSE, lam = lambda),
            error = function(e)
              bestNormalize::yeojohnson(v, standardize = FALSE, lambda = lambda)
          )
        }
        
        out[[nm]] <- as.numeric(predict(fit, newdata = v))
      }
      out
    }
    
    output$tfm_z_vs_yj_plot <- echarts4r::renderEcharts4r({
      req( isTRUE(rv$paired_zyj) || (isTRUE(rv$z_ready) && isTRUE(rv$yj_ready)) )
      
      req(rv$z_df, rv$yj_df)
      validate(need(identical(attr(rv$yj_df, "from"), "z"),
                    "Yeo–Johnson debe aplicarse sobre Z-score. Reaplica YJ."))
      
      vz <- .flatten_all_numeric(rv$z_df)   # Z-score
      vy <- .flatten_all_numeric(rv$yj_df)  # YJ sobre Z-score
      validate(need(length(vz) > 1 && length(vy) > 1, "Insufficient data to compare Z vs YJ."))
      
      dz <- .density_df(vz); dy <- .density_df(vy)
      validate(need(!is.null(dz) && !is.null(dy), "Could not generate density comparison."))
      
      echarts4r::e_charts(x, data = dz) |>
        echarts4r::e_line(y, name = "Z-score", smooth = TRUE, showSymbol = FALSE) |>
        echarts4r::e_line(y, data = dy, name = "Yeo–Johnson (on Z-score)", smooth = TRUE, showSymbol = FALSE) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_y_axis(name = "Density") |>
        echarts4r::e_x_axis(name = "All values") |>
        echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
    })
    
    
    # ================== SUMMARY (AFTER) ==================
    output$tfm_summary_after_tbl <- DT::renderDataTable({
      d_before <- rv$base_df
      d_after  <- rv$current_df
      
      validate(need(!is.null(d_before) && !is.null(d_after) && ncol(d_before) && ncol(d_after),
                    "No transformed data available."))
      
      common <- intersect(colnames(d_before), colnames(d_after))
      validate(need(length(common) > 0, "No common columns between before and after."))
      
      d_before <- as.data.frame(d_before[, common, drop = FALSE], check.names = FALSE)
      d_after  <- as.data.frame(d_after[,  common, drop = FALSE], check.names = FALSE)
      
      is_num_before <- vapply(d_before, function(x) suppressWarnings(all(is.finite(as.numeric(x[!is.na(x)])) | is.na(x))), logical(1))
      is_num_after  <- vapply(d_after,  function(x) suppressWarnings(all(is.finite(as.numeric(x[!is.na(x)])) | is.na(x))), logical(1))
      keep <- common[is_num_before & is_num_after]
      validate(need(length(keep) > 0, "No numeric columns to summarize."))
      
      d_before <- lapply(d_before[keep], function(x) suppressWarnings(as.numeric(x)))
      d_after  <- lapply(d_after[keep],  function(x) suppressWarnings(as.numeric(x)))
      
      metrics_order <- c(
        "Mean","Standard Deviation (SD)","Min","Q1","Median","Q3","Max",
        "Skewness","Kurtosis","Coefficient of Variation (CV)","IQR"
      )
      summary_tbl <- data.frame(Metric = metrics_order, check.names = FALSE)
      
      .calc_metrics <- function(v) {
        v <- v[is.finite(v)]
        if (!length(v)) return(rep(NA_real_, length(metrics_order)))
        qs <- stats::quantile(v, probs = c(0, .25, .5, .75, 1), na.rm = TRUE, names = FALSE)
        m  <- mean(v, na.rm = TRUE)
        s  <- stats::sd(v, na.rm = TRUE)
        cv <- if (isTRUE(is.finite(m)) && abs(m) > .Machine$double.eps) (s / m) * 100 else NA_real_
        c(
          Mean                          = m,
          `Standard Deviation (SD)`     = s,
          Min                           = qs[1],
          Q1                            = qs[2],
          Median                        = qs[3],
          Q3                            = qs[4],
          Max                           = qs[5],
          Skewness                      = e1071::skewness(v, na.rm = TRUE),
          Kurtosis                      = e1071::kurtosis(v, na.rm = TRUE),
          `Coefficient of Variation (CV)` = cv,
          IQR                           = qs[4] - qs[2]
        )[metrics_order]
      }
      
      for (nm in keep) {
        before_vec <- .calc_metrics(d_before[[nm]])
        after_vec  <- .calc_metrics(d_after[[nm]])
        summary_tbl[[paste0(nm, " (Before)")]] <- before_vec
        summary_tbl[[paste0(nm, " (After)")]]  <- after_vec
      }
      
      summary_tbl_t <- summary_tbl
      rownames(summary_tbl_t) <- summary_tbl_t$Metric
      summary_tbl_t$Metric <- NULL
      summary_tbl_t <- as.data.frame(t(summary_tbl_t), check.names = FALSE)
      summary_tbl_t <- data.frame(Variable = rownames(summary_tbl_t),
                                  summary_tbl_t, check.names = FALSE, row.names = NULL)
      
      DT::datatable(
        summary_tbl_t,
        rownames = FALSE,
        filter = "top",
        extensions = c("FixedHeader", "FixedColumns"),
        options = list(
          scrollX = "70vh",
          scrollY = "180vh",
          scrollCollapse = TRUE,
          dom = "lftip",
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1)
        ),
        class = "stripe hover nowrap"
      )
    })
    
    
    # ================== SHAPIRO-WILK TEST ==================
    output$tfm_shapiro_tbl <- DT::renderDataTable({
      d <- rv$current_df
      validate(need(!is.null(d) && ncol(d), "No transformed data available for Shapiro–Wilk test."))
      
      num_cols <- lapply(d, function(x) suppressWarnings(as.numeric(x)))
      col_nms  <- names(num_cols)
      
      res <- lapply(seq_along(num_cols), function(i) {
        nm <- col_nms[i]
        v  <- num_cols[[i]]
        v  <- v[is.finite(v)]
        
        method <- "Shapiro–Wilk"
        n_used <- length(v)
        pval   <- NA_real_
        
        if (n_used >= 3 && n_used <= 5000) {
          sw <- try(stats::shapiro.test(v), silent = TRUE)
          if (!inherits(sw, "try-error")) pval <- sw$p.value
        } else if (n_used > 5000) {
          set.seed(abs(as.integer(stats::runif(1, min = 1, max = .Machine$integer.max))) + i)
          idx <- seq_len(n_used)
          step <- floor(n_used / 5000)
          take <- idx[seq(1, min(1 + step * 4999, n_used), by = step)]
          v2   <- v[take]
          n_used <- length(v2)
          sw <- try(stats::shapiro.test(v2), silent = TRUE)
          if (!inherits(sw, "try-error")) pval <- sw$p.value
          method <- "Shapiro–Wilk (subsampled 5000)"
        } else {
          method <- "N/A (<3 valid values)"
        }
        
        data.frame(
          Variable = nm,
          `Shapiro–Wilk p-value` = pval,
          Method = method,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      
      tbl <- do.call(rbind, res)
      
      tbl$Normality <- ifelse(is.na(tbl$`Shapiro–Wilk p-value`),
                              "N/A",
                              ifelse(tbl$`Shapiro–Wilk p-value` < 0.05, "Not normal", "Normal"))
      
      denom    <- sum(!is.na(tbl$`Shapiro–Wilk p-value`))
      n_normal <- sum(tbl$Normality == "Normal",    na.rm = TRUE)
      n_abnorm <- sum(tbl$Normality == "Not normal", na.rm = TRUE)
      pct_norm <- if (denom > 0) round(100 * n_normal / denom, 1) else NA_real_
      pct_abn  <- if (denom > 0) round(100 * n_abnorm / denom, 1) else NA_real_
      
      caption_html <- htmltools::tags$div(
        style = "caption-side: bottom; text-align: left; padding-top: .5rem;",
        htmltools::HTML(
          sprintf("<strong>Summary:</strong> Normal = %d (%.1f%%) &nbsp;&nbsp;·&nbsp;&nbsp; Not normal = %d (%.1f%%)",
                  n_normal, pct_norm, n_abnorm, pct_abn)
        )
      )
      
      DT::datatable(
        tbl,
        rownames   = FALSE,
        filter     = "top",
        caption    = caption_html,
        extensions = c("FixedHeader", "FixedColumns"),
        options    = list(
          scrollX        = TRUE,
          scrollY        = "50vh",
          scrollCollapse = TRUE,
          dom            = "lftip",
          pageLength     = 25,
          lengthMenu     = list(c(10,25,50,-1), c("10","25","50","All")),
          fixedHeader    = TRUE,
          fixedColumns   = list(leftColumns = 1),
          rowCallback = htmlwidgets::JS(
            "function(row, data, displayNum, displayIndex, dataIndex){",
            "  if(displayIndex === 0){",
            "    $(row).addClass('dt-sticky-first-row');",
            "  }",
            "}"
          )
        ),
        class = "stripe hover nowrap"
      ) |>
        DT::formatRound(columns = "Shapiro–Wilk p-value", digits = 4) |>
        DT::formatStyle(
          "Shapiro–Wilk p-value",
          backgroundColor = DT::styleInterval(0.05, c("rgba(248,199,195,.8)", "rgba(209,247,214,.8)")),
          color           = DT::styleInterval(0.05, c("#842029", "#0f5132"))
        ) |>
        DT::formatStyle(
          "Normality",
          backgroundColor = DT::styleEqual(
            c("Normal", "Not normal", "N/A"),
            c("rgba(209,247,214,.8)", "rgba(248,199,195,.8)", "#f1f3f5")
          ),
          color = DT::styleEqual(
            c("Normal", "Not normal", "N/A"),
            c("#0f5132", "#842029", "#495057")
          ),
          fontWeight = DT::styleEqual(c("Normal", "Not normal"), c("600", "600"))
        )
    })
    
    # ================== GLOBAL SUMMARY (BEFORE vs AFTER, full dataset) ==================
    output$tfm_summary_var_tbl <- DT::renderDataTable({
      d_before <- rv$base_df
      d_after  <- rv$current_df
      
      validate(need(!is.null(d_before) && !is.null(d_after) && ncol(d_before) > 0 && ncol(d_after) > 0,
                    "No data for global comparison."))
      
      v_before <- suppressWarnings(as.numeric(unlist(d_before, use.names = FALSE)))
      v_after  <- suppressWarnings(as.numeric(unlist(d_after,  use.names = FALSE)))
      v_before <- v_before[is.finite(v_before)]
      v_after  <- v_after[is.finite(v_after)]
      
      validate(need(length(v_before) > 0 && length(v_after) > 0,
                    "No numeric values to summarize."))
      
      .metrics <- function(v) {
        qs <- stats::quantile(v, probs = c(0, .25, .5, .75, 1), na.rm = TRUE, names = FALSE)
        m  <- mean(v, na.rm = TRUE)
        s  <- stats::sd(v,   na.rm = TRUE)
        cv <- if (isTRUE(is.finite(m)) && abs(m) > .Machine$double.eps) (s / m) * 100 else NA_real_
        c(
          N        = length(v),
          Mean     = m,
          `Standard Deviation (SD)` = s,
          Min      = qs[1],
          Q1       = qs[2],
          Median   = qs[3],
          Q3       = qs[4],
          Max      = qs[5],
          Skewness = e1071::skewness(v, na.rm = TRUE),
          Kurtosis = e1071::kurtosis(v, na.rm = TRUE),
          `Coefficient of Variation (CV)` = cv,
          IQR      = qs[4] - qs[2]
        )
      }
      
      mb <- .metrics(v_before)
      ma <- .metrics(v_after)
      
      metrics_order <- c(
        "N","Mean","Standard Deviation (SD)","Min","Q1","Median","Q3","Max",
        "Skewness","Kurtosis","Coefficient of Variation (CV)","IQR"
      )
      
      summary_global <- data.frame(
        Metric = metrics_order,
        Before = unname(mb[metrics_order]),
        After  = unname(ma[metrics_order]),
        check.names = FALSE
      )
      
      DT::datatable(
        summary_global,
        rownames   = FALSE,
        filter     = "top",
        extensions = c("FixedHeader","FixedColumns"),
        options    = list(
          scrollX        = TRUE,
          scrollCollapse = TRUE,
          dom            = "lftip",
          pageLength     = 12,
          lengthMenu     = list(c(12,24,-1), c("12","24","All")),
          fixedHeader    = TRUE,
          fixedColumns   = list(leftColumns = 1),
          rowCallback = htmlwidgets::JS(
            "function(row, data){",
            "  var metric = data[0];",
            "  function colorCell(td, val, redCond, greenCond){",
            "    var x = parseFloat(val);",
            "    if(isNaN(x)) return;",
            "    if(redCond(x)) { $(td).css({'background-color':'rgba(248,199,195,.8)','color':'#842029','font-weight':'600'}); }",
            "    else if(greenCond && greenCond(x)) { $(td).css({'background-color':'rgba(209,247,214,.8)','color':'#0f5132','font-weight':'600'}); }",
            "  }",
            "  if(metric === 'Coefficient of Variation (CV)'){",
            "    colorCell($('td:eq(1)', row), data[1], function(x){return x>20;}, function(x){return x<=20;});",
            "    colorCell($('td:eq(2)', row), data[2], function(x){return x>20;}, function(x){return x<=20;});",
            "  }",
            "  if(metric === 'Skewness' || metric === 'Kurtosis'){",
            "    colorCell($('td:eq(1)', row), data[1], function(x){return Math.abs(x)>1;});",
            "    colorCell($('td:eq(2)', row), data[2], function(x){return Math.abs(x)>1;});",
            "  }",
            "}"
          )
        ),
        class = "stripe hover nowrap"
      ) |>
        DT::formatRound(columns = c("Before","After"), digits = 4)
    })
    
    # ================== ENVIAR A IMPUTATION (con pipeline) ==================
    observeEvent(input$go_impute_btn, {
      validate(need(!is.null(rv$current_df) && ncol(rv$current_df),
                    "No data to send to Imputation."))
      
      rv_global <- tryCatch(
        get("rv", envir = .GlobalEnv),
        error = function(e) NULL
      )
      
      if (is.null(rv_global)) {
        showNotification(
          "Global 'rv' not found. Cannot send payload to Imputation.",
          type = "error"
        )
        return()
      }
      
      rv_global$impute_payload <- list(
        base_df     = if (!is.null(rv$base_df)) rv$base_df else NULL,
        current_df  = rv$current_df,
        z_df        = if (!is.null(rv$z_df))  rv$z_df  else NULL,
        yj_df       = if (!is.null(rv$yj_df)) rv$yj_df else NULL,
        
        # --- NUEVO: orden de transformaciones que se aplicaron a current_df ---
        transform_pipeline = rv$transform_pipeline,
        
        selection   = list(
          default_dataset = "current",
          columns         = colnames(rv$current_df)
        ),
        meta = list(
          columns_before = if (!is.null(rv$base_df)) colnames(rv$base_df) else NULL,
          columns_after  = colnames(rv$current_df),
          n_before       = if (!is.null(rv$base_df)) nrow(rv$base_df) else NA_integer_,
          n_after        = nrow(rv$current_df),
          generated_at   = Sys.time()
        )
      )
      
      session$onFlushed(function() {
        shinyjs::runjs("
      (function(){
        var navId   = 'main_nav';
        var value   = 'imputation_tab';
        var titleTx = 'Imputation';

        function log(msg){ try{ console.log('[go_impute]', msg); }catch(e){} }

        function clickEl(el){
          if (!el) return false;
          try {
            var ev = new MouseEvent('click', {bubbles:true, cancelable:true, view:window});
            var ok = el.dispatchEvent(ev);
            if (ok) return true;
            if (typeof el.click === 'function') el.click();
            return true;
          } catch(e){ return false; }
        }

        var byVal = document.querySelector('[data-value=\"'+value+'\"]');
        if (clickEl(byVal)) { log('click por data-value (global)'); return; }

        var byValScoped = document.querySelector('#'+navId+' [data-value=\"'+value+'\"]');
        if (clickEl(byValScoped)) { log('click por data-value (scoped)'); return; }

        var links = document.querySelectorAll('#'+navId+' .nav-link, #'+navId+' a.nav-link, #'+navId+' button.nav-link');
        for (var i=0;i<links.length;i++){
          if ((links[i].textContent||'').trim() === titleTx){ 
            if (clickEl(links[i])) { log('click por texto exacto'); return; }
          }
        }

        for (var j=0;j<links.length;j++){
          var txt = (links[j].textContent||'').replace(/\\s+/g,' ').trim();
          if (txt.indexOf(titleTx) !== -1){
            if (clickEl(links[j])) { log('click por texto (incluye)'); return; }
          }
        }

        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('main_nav', value, {priority:'event'});
          log('setInputValue fallback');
        } else {
          log('no se encontró target del tab :(');
        }
      })();
    ")
      }, once = TRUE)
      
      showNotification("Datasets sent to 'Imputation' panel.", type = "message")
    })
    
  })
}
