# Server/stat_summary_bindings.R
# Acepta tanto la firma nueva (raw_react, processed_react)
# como la vieja (data_react, num_df, miss_pct) vía ...

bind_stat_summary <- function(output, raw_react = NULL, processed_react = NULL, session, ...) {
  dots <- list(...)
  
  # Back-compat: mapear argumentos antiguos si vinieron
  if (is.null(raw_react)) {
    if (!is.null(dots$data_react)) raw_react <- dots$data_react
  }
  if (is.null(processed_react)) {
    if (!is.null(dots$processed_react)) processed_react <- dots$processed_react
    if (is.null(processed_react) && !is.null(dots$num_df)) processed_react <- dots$num_df
    if (is.null(processed_react) && !is.null(dots$data_react)) processed_react <- dots$data_react
  }
  
  # Helpers
  .numeric_vector <- function(d) {
    if (is.null(d) || !is.data.frame(d) || ncol(d) == 0) return(numeric())
    v <- suppressWarnings(as.numeric(as.matrix(d)))
    v[is.finite(v)]
  }
  .validate_processed <- function(d) {
    validate(need(!is.null(d) && is.data.frame(d) && nrow(d) > 0 && ncol(d) > 0,
                  "Run Quick Profiling first (no processed data yet)."))
  }
  .validate_raw <- function(d) {
    validate(need(!is.null(d) && is.data.frame(d) && ncol(d) >= 2 && nrow(d) >= 2,
                  "Upload a CSV with headers and a first text column."))
  }
  
  # ================= 1) Donut (sobre procesado) =================
  output[["upload_missing_plot"]] <- echarts4r::renderEcharts4r({
    d <- processed_react()
    .validate_processed(d)
    
    total <- nrow(d) * ncol(d)
    n_missing <- sum(is.na(d))
    n_present <- total - n_missing
    
    data.frame(category = c("Missing","Present"), value = c(n_missing, n_present)) |>
      echarts4r::e_charts(category) |>
      echarts4r::e_pie(value, radius = c("40%","70%"),
                       label = list(formatter = "{b}: {d}%")) |>
      echarts4r::e_tooltip(trigger = "item") |>
      echarts4r::e_legend(orient = "vertical", left = "left")
  })
  
  # ================= 2) Distribución (procesado) =================
  output[["upload_dist_plot"]] <- echarts4r::renderEcharts4r({
    d <- processed_react()
    .validate_processed(d)
    
    vals <- .numeric_vector(d)
    validate(need(length(vals) > 0, "No numeric values to plot."))
    
    # Histograma (barras) con FD
    h <- hist(vals, breaks = "FD", plot = FALSE)
    df_h <- data.frame(x = h$mids, y = as.numeric(h$counts))
    
    # Curva suavizada (densidad -> escala 'counts')
    bw_hist <- if (length(h$breaks) > 1) mean(diff(h$breaks)) else 1
    dens <- density(vals, na.rm = TRUE, bw = "nrd0", adjust = 1.8)
    df_curve <- data.frame(
      x = dens$x,
      y = dens$y * length(vals) * bw_hist
    )
    
    df_h |>
      echarts4r::e_charts(x) |>
      # Barras del histograma
      echarts4r::e_bar(y, name = "Count", barWidth = "85%", z = 1) |>
      # Área bajo la curva (suave)
      echarts4r::e_area(
        y, data = df_curve, name = "Smoothed area",
        smooth = TRUE,
        areaStyle = list(opacity = 0.22),
        lineStyle = list(width = 0),
        showSymbol = FALSE,
        z = 5
      ) |>
      # Línea suavizada encima del área
      echarts4r::e_line(
        y, data = df_curve, name = "Smoothed frequency",
        smooth = TRUE,
        showSymbol = FALSE,
        lineStyle = list(width = 2),
        z = 10
      ) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(right = 10) |>
      echarts4r::e_y_axis(name = "Count") |>
      echarts4r::e_x_axis(type = "value", name = "Value") |>
      echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
      echarts4r::e_datazoom(type = "inside", x_index = 0) |>
      echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
  })
  
  
  
  # ================= 3) Boxplot (procesado) =================
  output[["upload_boxplot"]] <- echarts4r::renderEcharts4r({
    d <- processed_react()
    .validate_processed(d)
    
    vals <- .numeric_vector(d)
    validate(need(length(vals) > 0, "No numeric values to plot."))
    
    data.frame(group = "All data", value = vals) |>
      echarts4r::e_charts(group) |>
      echarts4r::e_boxplot(value) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_y_axis(name = "Value") |>
      echarts4r::e_x_axis(axisLabel = list(show = FALSE), name = NULL) |>
      echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%") |>
      echarts4r::e_datazoom(type = "inside", y_index = 0) |>
      echarts4r::e_datazoom(type = "slider", y_index = 0, right = 6)
  })
  
  # ================= 4) Heatmap (CSV raw) =================
  output[["upload_heatmap"]] <- echarts4r::renderEcharts4r({
    d <- raw_react()
    .validate_raw(d)
    
    # Ensure we have a proper first column label and column headers
    cn <- colnames(d)
    if (is.null(cn) || all(cn == paste0("V", seq_len(ncol(d))))) {
      first_row <- as.character(unlist(d[1, , drop = TRUE], use.names = FALSE))
      colnames(d) <- c("ROW_LABEL", make.unique(trimws(first_row[-1]), sep = "_"))
      d <- d[-1, , drop = FALSE]
    } else {
      if (is.na(cn[1]) || cn[1] == "") cn[1] <- "ROW_LABEL"
      colnames(d)[1] <- "ROW_LABEL"
    }
    
    RowLabel <- as.character(d[["ROW_LABEL"]])
    Mat <- d[, setdiff(names(d), "ROW_LABEL"), drop = FALSE]
    
    # Robust numeric parsing
    Mat[] <- lapply(Mat, function(v) {
      if (is.numeric(v)) return(v)
      vx <- as.character(v); vx <- trimws(v)
      vx <- gsub("%", "", vx, fixed = TRUE)
      vx <- gsub("\\s+", "", vx)
      both <- grepl("\\.", vx) & grepl(",", vx)
      vx[both] <- gsub(",", "", vx[both], fixed = TRUE)
      only_comma <- !grepl("\\.", vx) & grepl(",", vx)
      vx[only_comma] <- gsub(",", ".", vx[only_comma], fixed = TRUE)
      suppressWarnings(as.numeric(vx))
    })
    
    ColLabel <- rep(colnames(Mat), each = nrow(Mat))
    RowRep   <- rep(RowLabel, times = ncol(Mat))
    Value    <- as.numeric(as.matrix(Mat))
    dfh <- data.frame(RowLabel = RowRep, ColLabel = ColLabel, value = Value, check.names = FALSE)
    
    validate(need(nrow(dfh) > 0, "No values for heatmap."))
    
    # Keep stable Y order but ensure tooltip shows text
    row_levels <- unique(RowLabel)
    dfh$RowLabel <- factor(dfh$RowLabel, levels = row_levels)
    dfh$.RowText <- as.character(dfh$RowLabel)
    
    vmin <- suppressWarnings(min(dfh$value, na.rm = TRUE)); if (!is.finite(vmin)) vmin <- 0
    vmax <- suppressWarnings(max(dfh$value, na.rm = TRUE)); if (!is.finite(vmax)) vmax <- 1
    
    dfh |>
      echarts4r::e_charts(ColLabel) |>
      # add the row text so it's available in the tooltip
      echarts4r::e_add(".RowText", .RowText) |>
      echarts4r::e_heatmap(RowLabel, value) |>
      echarts4r::e_visual_map(
        value, min = vmin, max = vmax, calculable = TRUE,
        left = "left", top = "middle",
        outOfRange = list(color = "#d9d9d9")
      ) |>
      echarts4r::e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS(
          "function(p){
           // ECharts heatmap value format: [ColLabel, RowLabelIndexOrText, numeric]
           var col = p.name; // x-axis category (ColLabel)
           // prefer our attached text (if available)
           var rowTxt = (p.data && p.data['.RowText']) ? p.data['.RowText']
                       : (p.value && p.value[1]) ? p.value[1] : '';
           var val = (p.value && p.value.length>2) ? p.value[2] : p.value;
           return 'Row: ' + rowTxt + '<br/>Col: ' + col + '<br/>Value: ' + val;
         }"
        )
      ) |>
      echarts4r::e_y_axis(type = "category", axisLabel = list(interval = 0)) |>
      echarts4r::e_x_axis(axisLabel = list(interval = 0)) |>
      echarts4r::e_grid(left = '18%', right = '14%', bottom = '22%', top = '6%') |>
      echarts4r::e_datazoom(type = "inside", x_index = 0) |>
      echarts4r::e_datazoom(type = "inside", y_index = 0) |>
      echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8) |>
      echarts4r::e_datazoom(type = "slider", y_index = 0, right = 6)
  })
  
}
