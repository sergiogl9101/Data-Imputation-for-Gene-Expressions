# Server/mod_viz_server.R

viz_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # ====== GLOBALS DESDE QUICK PROFILING (IDs + tmp base) ======
    # Wrappers seguros alrededor de session$userData$qp_row_ids / qp_tmp_base
    get_qp_row_ids <- reactive({
      f <- session$userData$qp_row_ids
      
      if (is.null(f)) {
        cat(">>> [viz] qp_row_ids todavía es NULL en session$userData\n")
        return(NULL)
      }
      
      ids <- f()
      cat(">>> [viz] qp_row_ids() length:", length(ids), "\n")
      if (length(ids)) {
        cat("    head(ids):", paste(head(ids, 3), collapse = ", "), "\n")
      }
      ids
    })
    
    get_qp_tmp_base <- reactive({
      f <- session$userData$qp_tmp_base
      
      if (is.null(f)) {
        cat(">>> [viz] qp_tmp_base todavía es NULL en session$userData\n")
        return(NULL)
      }
      
      base <- f()
      if (is.null(base)) {
        cat(">>> [viz] qp_tmp_base() devolvió NULL\n")
        return(NULL)
      }
      
      if (is.data.frame(base) || is.matrix(base)) {
        cat(">>> [viz] qp_tmp_base() dims:", paste(dim(base), collapse = " x "), "\n")
        cat("    colnames:", paste(utils::head(colnames(base), 5), collapse = ", "), "\n")
      } else {
        cat(">>> [viz] qp_tmp_base() no es data.frame/matrix, class:", class(base), "\n")
      }
      base
    })
    
    # ---------- Estado interno ----------
    rv_viz <- reactiveValues(
      base_df            = NULL,
      current_df         = NULL,
      z_df               = NULL,
      yj_df              = NULL,
      imputed_df         = NULL,   # imputado + destransformado
      imputed_df_scaled  = NULL,   # imputado pero SIN destransformar
      mice_diag          = NULL,
      transform_pipeline = NULL,
      transform_meta     = NULL,
      eval_tbl           = NULL
    )
    
    # ---------- Helper: solo numéricas ----------
    get_num_df <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      mask <- vapply(df, is.numeric, logical(1))
      if (!any(mask)) return(NULL)
      df[, mask, drop = FALSE]
    }
    
    # ---------- Helpers tipo transform_server ----------
    viz_flatten_all_numeric <- function(df) {
      num_df <- get_num_df(df)
      if (is.null(num_df) || !ncol(num_df)) return(numeric())
      v <- suppressWarnings(as.numeric(as.matrix(num_df)))
      v[is.finite(v)]
    }
    
    viz_hist_df_fixed_bins <- function(x, bins = 30) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      h <- hist(x, breaks = bins, plot = FALSE)
      data.frame(x = h$mids, y = as.numeric(h$counts))
    }
    
    viz_density_df <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      d <- density(x, na.rm = TRUE)
      data.frame(x = d$x, y = d$y)
    }
    
    viz_qq_df <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (!length(x)) return(NULL)
      x <- sort(x); n <- length(x)
      p <- (1:n - 0.5) / n
      theo <- stats::qnorm(p)
      data.frame(theoretical = theo, sample = scale(x)[, 1])
    }
    
    # ---------- Recibir payload desde impute_server ----------
    observeEvent(rv$viz_payload, {
      cat(">>> [viz] observeEvent(rv$viz_payload) disparado\n")
      payload <- rv$viz_payload
      req(payload)
      
      if (!is.null(payload$imputed_df)) {
        cat("    payload$imputed_df dims:", paste(dim(payload$imputed_df), collapse = " x "), "\n")
      } else {
        cat("    payload$imputed_df es NULL\n")
      }
      
      rv_viz$base_df            <- payload$base_df
      rv_viz$current_df         <- payload$current_df
      rv_viz$z_df               <- payload$z_df
      rv_viz$yj_df              <- payload$yj_df
      rv_viz$imputed_df         <- payload$imputed_df          # ORIGINAL (destransformado)
      rv_viz$imputed_df_scaled  <- payload$imputed_df_scaled   # TRANSFORMADO (sin destransformar)
      rv_viz$mice_diag          <- payload$mice_diag
      rv_viz$transform_pipeline <- payload$transform_pipeline
      rv_viz$transform_meta     <- payload$transform_meta
      rv_viz$eval_tbl           <- payload$eval_results
      
      showNotification(
        "Visualization payload received (original, transformed and imputed datasets).",
        type = "message"
      )
    }, ignoreInit = FALSE)
    
    # ---------- Tabla de métricas de evaluación ----------
    output$viz_eval_tbl <- DT::renderDataTable({
      req(rv_viz$eval_tbl)
      df <- rv_viz$eval_tbl
      
      # Redondear columnas numéricas
      num_mask <- vapply(df, is.numeric, logical(1))
      df[num_mask] <- lapply(df[num_mask], function(x) round(x, 6))
      
      DT::datatable(
        df,
        options  = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    # ================== TIPO DE GRÁFICA (Histogram/Density/Boxplot/QQ) ==================
    viz_plot_type <- reactive({
      input$viz_plot_type %||% "Histogram"
    })
    
    # ================== PLOTS: TRANSFORMED (NO IMPUTATION) ==================
    output$viz_plot_transformed_untransformed <- echarts4r::renderEcharts4r({
      req(rv_viz$current_df)
      validate(need(ncol(rv_viz$current_df) > 0, "No data available in transformed dataset."))
      
      ptype <- viz_plot_type()
      x <- viz_flatten_all_numeric(rv_viz$current_df)
      validate(need(length(x) > 0, "No numeric data in transformed dataset (no imputation)."))
      
      if (ptype == "Histogram") {
        hb <- viz_hist_df_fixed_bins(x, bins = 30)
        validate(need(!is.null(hb), "Could not generate histogram (Transformed)."))
        
        echarts4r::e_charts(x, data = hb) |>
          echarts4r::e_bar(y, name = "Transformed (no imputation)") |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Count") |>
          echarts4r::e_x_axis(name = "All values (Transformed, no imputation)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
        
      } else if (ptype == "Density") {
        db <- viz_density_df(x)
        validate(need(!is.null(db), "Could not generate density (Transformed)."))
        
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        
        if (is.finite(s) && s > 0) {
          db$normal_y <- stats::dnorm(db$x, mean = m, sd = s)
        } else {
          db$normal_y <- NA_real_
        }
        
        p <- echarts4r::e_charts(x, data = db) |>
          echarts4r::e_line(y, name = "Transformed (no imputation)",
                            smooth = TRUE, showSymbol = FALSE) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Density") |>
          echarts4r::e_x_axis(name = "All values (Transformed, no imputation)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
        
        if (any(is.finite(db$normal_y))) {
          p <- p |>
            echarts4r::e_line(
              normal_y,
              name = "Normal curve",
              smooth = TRUE,
              lineStyle = list(type = "dashed")
            )
        }
        p
        
      } else if (ptype == "Boxplot") {
        df_box <- data.frame(
          group = "Transformed (no imputation)",
          value = x
        )
        echarts4r::e_charts(group, data = df_box) |>
          echarts4r::e_boxplot(value) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_y_axis(name = "All values (Transformed, no imputation)") |>
          echarts4r::e_x_axis(name = NULL) |>
          echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%")
        
      } else { # QQ-plot (full dataset)
        dfq <- viz_qq_df(x)
        validate(need(!is.null(dfq), "Could not generate QQ plot (Transformed)."))
        
        theoretical <- stats::qnorm(
          stats::ppoints(length(x)),
          mean = mean(x),
          sd   = stats::sd(x)
        )
        
        echarts4r::e_charts(theoretical, data = dfq) |>
          echarts4r::e_scatter(sample, name = "Transformed (no imputation)") |>
          echarts4r::e_line(
            theoretical,
            name = "Normal reference",
            smooth = TRUE,
            lineStyle = list(type = "dashed")
          ) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_x_axis(name = "Theoretical quantiles (Normal)") |>
          echarts4r::e_y_axis(name = "Sample quantiles (scaled)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
      }
    })
    
    # ================== PLOTS: TRANSFORMED + IMPUTED (SIN DESTRANSFORMAR) ==================
    output$viz_plot_imputed_untransformed <- echarts4r::renderEcharts4r({
      req(rv_viz$imputed_df_scaled)
      validate(need(ncol(rv_viz$imputed_df_scaled) > 0, "No data available in imputed dataset."))
      
      ptype <- viz_plot_type()
      x <- viz_flatten_all_numeric(rv_viz$imputed_df_scaled)
      validate(need(length(x) > 0, "No numeric data in transformed + imputed dataset."))
      
      if (ptype == "Histogram") {
        ha <- viz_hist_df_fixed_bins(x, bins = 30)
        validate(need(!is.null(ha), "Could not generate histogram (Imputed)."))
        
        echarts4r::e_charts(x, data = ha) |>
          echarts4r::e_bar(y, name = "Imputed (transformed scale)") |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Count") |>
          echarts4r::e_x_axis(name = "All values (Transformed + imputed)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
        
      } else if (ptype == "Density") {
        da <- viz_density_df(x)
        validate(need(!is.null(da), "Could not generate density (Imputed)."))
        
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        
        if (is.finite(s) && s > 0) {
          da$normal_y <- stats::dnorm(da$x, mean = m, sd = s)
        } else {
          da$normal_y <- NA_real_
        }
        
        p <- echarts4r::e_charts(x, data = da) |>
          echarts4r::e_line(y, name = "Imputed (transformed scale)",
                            smooth = TRUE, showSymbol = FALSE) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_y_axis(name = "Density") |>
          echarts4r::e_x_axis(name = "All values (Transformed + imputed)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%") |>
          echarts4r::e_datazoom(type = "inside", x_index = 0) |>
          echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
        
        if (any(is.finite(da$normal_y))) {
          p <- p |>
            echarts4r::e_line(
              normal_y,
              name = "Normal curve",
              smooth = TRUE,
              lineStyle = list(type = "dashed")
            )
        }
        p
        
      } else if (ptype == "Boxplot") {
        df_box <- data.frame(
          group = "Imputed (transformed scale)",
          value = x
        )
        echarts4r::e_charts(group, data = df_box) |>
          echarts4r::e_boxplot(value) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_y_axis(name = "All values (Transformed + imputed)") |>
          echarts4r::e_x_axis(name = NULL) |>
          echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%")
        
      } else { # QQ-plot (full dataset)
        dfq <- viz_qq_df(x)
        validate(need(!is.null(dfq), "Could not generate QQ plot (Imputed)."))
        
        theoretical <- stats::qnorm(
          stats::ppoints(length(x)),
          mean = mean(x),
          sd   = stats::sd(x)
        )
        
        echarts4r::e_charts(theoretical, data = dfq) |>
          echarts4r::e_scatter(sample, name = "Imputed (transformed scale)") |>
          echarts4r::e_line(
            theoretical,
            name = "Normal reference",
            smooth = TRUE,
            lineStyle = list(type = "dashed")
          ) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_x_axis(name = "Theoretical quantiles (Normal)") |>
          echarts4r::e_y_axis(name = "Sample quantiles (scaled)") |>
          echarts4r::e_grid(left = "8%", right = "3%", bottom = "12%")
      }
    })
    
    # ---------- Heatmap 1: ORIGINAL (qp_tmp_base, sin imputar ni transformar) ----------
    output$viz_heatmap_original <- echarts4r::renderEcharts4r({
      cat(">>> [viz_heatmap_original] rendering\n")
      
      base_ids <- get_qp_row_ids()
      base_num <- get_qp_tmp_base()
      
      req(base_ids, base_num)
      
      cat("    base_ids length:", if (is.null(base_ids)) "NULL" else length(base_ids), "\n")
      if (!is.null(base_num) && is.data.frame(base_num)) {
        cat("    base_num dim:", paste(dim(base_num), collapse = " x "), "\n")
      } else {
        cat("    base_num is NULL or not data.frame\n")
      }
      
      validate(need(!is.null(base_num) && is.data.frame(base_num),
                    "No Quick Profiling base matrix available (run it first)."))
      validate(need(!is.null(base_ids) && length(base_ids) == nrow(base_num),
                    "Row IDs and tmp base have inconsistent dimensions."))
      
      # Aseguramos que todo sea numérico
      base_num[] <- lapply(base_num, function(v) {
        if (is.numeric(v)) return(v)
        suppressWarnings(as.numeric(v))
      })
      
      nr <- nrow(base_num); nc <- ncol(base_num)
      validate(need(nr > 0 && nc > 0, "No data to build the heatmap."))
      
      RowLabel <- rep(base_ids,              times = nc)
      ColLabel <- rep(colnames(base_num),    each  = nr)
      value    <- as.numeric(as.matrix(base_num))
      
      df <- data.frame(
        RowLabel = RowLabel,
        ColLabel = ColLabel,
        value    = value,
        stringsAsFactors = FALSE
      )
      df <- df[is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No numeric values to plot."))
      
      # Mantener el orden de los IDs
      row_levels    <- unique(df$RowLabel)
      df$RowLabel   <- factor(df$RowLabel, levels = row_levels)
      
      vmin <- suppressWarnings(min(df$value, na.rm = TRUE)); if (!is.finite(vmin)) vmin <- 0
      vmax <- suppressWarnings(max(df$value, na.rm = TRUE)); if (!is.finite(vmax)) vmax <- 1
      
      echarts4r::e_charts(df, ColLabel) |>
        echarts4r::e_heatmap(RowLabel, value, name = "Original") |>
        echarts4r::e_visual_map(
          min        = vmin,
          max        = vmax,
          calculable = TRUE,
          right      = "8%",
          top        = "middle",
          inRange    = list(color = c("#fee0d2", "#de2d26"))  # rojito
        ) |>
        echarts4r::e_tooltip(
          trigger   = "item",
          formatter = htmlwidgets::JS(
            "function(p){
               var col = p.name;
               var row = p.value[1];
               var val = p.value[2];
               return 'Row: ' + row + '<br/>Col: ' + col + '<br/>Value: ' + val + ' (original)';
             }"
          )
        ) |>
        echarts4r::e_y_axis(type = 'category', axisLabel = list(interval = 0)) |>
        echarts4r::e_x_axis(axisLabel = list(interval = 0)) |>
        echarts4r::e_grid(left = '18%', right = '12%', bottom = '22%', top = '6%') |>
        echarts4r::e_datazoom(type = 'inside', x_index = 0) |>
        echarts4r::e_datazoom(type = 'inside', y_index = 0) |>
        echarts4r::e_datazoom(type = 'slider', x_index = 0, bottom = 8) |>
        echarts4r::e_datazoom(type = 'slider', y_index = 0, right = 6)
    })
    
    
    # ---------- Heatmap 2: IMPUTADO (imputed_df destransformado) ----------
    output$viz_heatmap_imputed <- echarts4r::renderEcharts4r({
      cat(">>> [viz_heatmap_imputed] rendering\n")
      req(rv_viz$imputed_df)
      
      base_ids <- get_qp_row_ids()
      base_num <- get_qp_tmp_base()
      imp_full <- rv_viz$imputed_df
      
      req(base_ids, base_num)
      
      cat("    imputed_df dim:", paste(dim(imp_full), collapse = " x "), "\n")
      cat("    base_ids length:", if (is.null(base_ids)) "NULL" else length(base_ids), "\n")
      if (!is.null(base_num) && is.data.frame(base_num)) {
        cat("    base_num dim:", paste(dim(base_num), collapse = " x "), "\n")
      } else {
        cat("    base_num is NULL or not data.frame\n")
      }
      
      validate(need(!is.null(base_num) && is.data.frame(base_num),
                    "Quick Profiling base matrix not available."))
      validate(need(!is.null(base_ids) && length(base_ids) == nrow(base_num),
                    "Row IDs and tmp base have inconsistent dimensions."))
      
      # Subset numérico del imputado
      get_num_df_local <- function(df) {
        if (is.null(df) || !is.data.frame(df)) return(NULL)
        mask <- vapply(df, is.numeric, logical(1))
        if (!any(mask)) return(NULL)
        df[, mask, drop = FALSE]
      }
      
      imp_num <- get_num_df_local(imp_full)
      validate(need(!is.null(imp_num) && ncol(imp_num) > 0,
                    "No numeric columns in imputed dataset."))
      
      # Alinear columnas entre tmp base y imputado
      common_cols <- intersect(colnames(imp_num), colnames(base_num))
      cat("    common_cols:", paste(common_cols, collapse = ", "), "\n")
      validate(need(length(common_cols) > 0,
                    "No common numeric columns between tmp base and imputed dataset."))
      
      imp_num_aligned <- imp_num[, common_cols, drop = FALSE]
      
      validate(need(nrow(imp_num_aligned) == nrow(base_num),
                    "Row counts differ between tmp base and imputed dataset."))
      
      nr <- nrow(imp_num_aligned); nc <- ncol(imp_num_aligned)
      validate(need(nr > 0 && nc > 0, "No data to build the heatmap."))
      
      RowLabel <- rep(base_ids,                      times = nc)
      ColLabel <- rep(colnames(imp_num_aligned),     each  = nr)
      value    <- as.numeric(as.matrix(imp_num_aligned))
      
      df <- data.frame(
        RowLabel = RowLabel,
        ColLabel = ColLabel,
        value    = value,
        stringsAsFactors = FALSE
      )
      df <- df[is.finite(df$value), , drop = FALSE]
      validate(need(nrow(df) > 0, "No numeric values to plot."))
      
      # Mantener orden de IDs
      row_levels  <- unique(df$RowLabel)
      df$RowLabel <- factor(df$RowLabel, levels = row_levels)
      
      vmin <- suppressWarnings(min(df$value, na.rm = TRUE)); if (!is.finite(vmin)) vmin <- 0
      vmax <- suppressWarnings(max(df$value, na.rm = TRUE)); if (!is.finite(vmax)) vmax <- 1
      
      echarts4r::e_charts(df, ColLabel) |>
        echarts4r::e_heatmap(RowLabel, value, name = "Imputed") |>
        echarts4r::e_visual_map(
          min        = vmin,
          max        = vmax,
          calculable = TRUE,
          right      = "8%",
          top        = "middle",
          inRange    = list(color = c("#deebf7", "#3182bd"))  # azul
        ) |>
        echarts4r::e_tooltip(
          trigger   = "item",
          formatter = htmlwidgets::JS(
            "function(p){
               var col = p.name;
               var row = p.value[1];
               var val = p.value[2];
               return 'Row: ' + row + '<br/>Col: ' + col + '<br/>Value: ' + val + ' (imputed)';
             }"
          )
        ) |>
        echarts4r::e_y_axis(type = 'category', axisLabel = list(interval = 0)) |>
        echarts4r::e_x_axis(axisLabel = list(interval = 0)) |>
        echarts4r::e_grid(left = '18%', right = '12%', bottom = '22%', top = '6%') |>
        echarts4r::e_datazoom(type = "inside", x_index = 0) |>
        echarts4r::e_datazoom(type = "inside", y_index = 0) |>
        echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8) |>
        echarts4r::e_datazoom(type = "slider", y_index = 0, right = 6)
    })
    
    
    # ---------- Tabla final (imputed + destransformado) ----------
    output$viz_imputed_tbl <- DT::renderDataTable({
      req(rv_viz$imputed_df)
      
      # Data imputada en escala ORIGINAL
      df_imp  <- rv_viz$imputed_df
      base_df <- rv_viz$base_df
      
      n_rows <- nrow(df_imp)
      
      # ----- Máscara de celdas imputadas -----
      # TRUE donde base_df tenía NA y ahora imputed_df tiene un valor
      imp_flags <- as.data.frame(
        matrix(FALSE, nrow = n_rows, ncol = ncol(df_imp),
               dimnames = list(NULL, colnames(df_imp)))
      )
      
      if (!is.null(base_df) && nrow(base_df) == n_rows) {
        common_cols <- intersect(colnames(base_df), colnames(df_imp))
        
        for (col in common_cols) {
          b  <- base_df[[col]]
          im <- df_imp[[col]]
          
          # Aseguramos longitudes iguales
          if (length(b) == length(im)) {
            imp_flags[[col]] <- is.na(b) & !is.na(im)
          }
        }
      }
      
      # ----- Construir versión "pintada" de la tabla (sin ID todavía) -----
      disp_noid <- df_imp
      
      for (col in colnames(disp_noid)) {
        vals  <- disp_noid[[col]]
        
        # Si no hay máscara para esta columna, solo la pasamos a character
        if (is.null(imp_flags[[col]])) {
          vals_chr <- as.character(vals)
          vals_chr[is.na(vals)] <- NA_character_
          disp_noid[[col]] <- vals_chr
        } else {
          flags <- imp_flags[[col]]
          vals_chr <- as.character(vals)
          vals_chr[is.na(vals)] <- NA_character_
          
          idx_imp <- which(flags)
          if (length(idx_imp) > 0) {
            vals_chr[idx_imp] <- paste0(
              "<span style='background-color:#fff3cd;' title='Imputed value'>",
              vals_chr[idx_imp],
              "</span>"
            )
          }
          disp_noid[[col]] <- vals_chr
        }
      }
      
      # ----- Agregar columna ID al inicio -----
      ids <- get_qp_row_ids()
      if (!is.null(ids) && length(ids) == n_rows) {
        df_display <- data.frame(
          ID = ids,
          disp_noid,
          check.names = FALSE
        )
      } else {
        df_display <- data.frame(
          ID = seq_len(n_rows),
          disp_noid,
          check.names = FALSE
        )
      }
      
      DT::datatable(
        df_display,
        options = list(
          scrollX      = TRUE,
          pageLength   = 20,
          fixedColumns = list(leftColumns = 1)  #  congela la primera columna
        ),
        extensions = "FixedColumns",             #  habilita la extensión
        rownames  = FALSE,
        escape    = FALSE
      )
    })
    
    
    # ---------- Download .csv del dataset imputado (destransformado) ----------
    output$viz_download_imputed <- downloadHandler(
      filename = function() {
        paste0("imputed_dataset_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv_viz$imputed_df)
        df_out <- rv_viz$imputed_df
        
        # Agregar la misma columna ID que en la tabla
        ids <- get_qp_row_ids()
        if (!is.null(ids) && length(ids) == nrow(df_out)) {
          df_out <- data.frame(
            ID = ids,
            df_out,
            check.names = TRUE
          )
        } else {
          df_out <- data.frame(
            ID = seq_len(nrow(df_out)),
            df_out,
            check.names = TRUE
          )
        }
        
        utils::write.csv(df_out, file, row.names = FALSE)
      }
    )
    
    
    
  })
}
