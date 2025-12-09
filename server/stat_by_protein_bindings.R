# Server/stat_by_protein_bindings.R
# Binder for the "Resume by Protein" tab (echarts4r-only)
# Uses the processed numeric DF (post Quick Profiling).

bind_stat_by_protein <- function(output, processed_react, input, session) {
  ns <- session$ns
  
  # ---------- Helpers ----------
  .validate_processed <- function(d) {
    validate(need(!is.null(d) && is.data.frame(d) && nrow(d) > 0 && ncol(d) > 0,
                  "Run Quick Profiling first (no processed data yet)."))
  }
  
  .protein_choices <- function(d) {
    cn <- colnames(d)
    if (is.null(cn) || !length(cn)) cn <- paste0("Protein_", seq_len(ncol(d)))
    # Ensure unique, non-empty names:
    cn[is.na(cn) | cn == ""] <- paste0("Protein_", which(is.na(cn) | cn == ""))
    make.unique(cn, sep = "_")
  }
  
  .safe_vector <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    v[is.finite(v)]
  }
  
  # ---------- Populate selects when processed DF changes ----------
  observeEvent(processed_react(), {
    d <- processed_react()
    if (is.null(d) || !is.data.frame(d) || !ncol(d)) return()
    choices <- .protein_choices(d)
    
    # Primary protein default: first column
    shiny::updateSelectInput(session, "protein_select",
                             choices = choices,
                             selected = choices[[1]]
    )
    # Comparison default: second column if exists, else first
    comp_default <- if (length(choices) >= 2) choices[[2]] else choices[[1]]
    shiny::updateSelectInput(session, "protein_compare_select",
                             choices = choices,
                             selected = comp_default
    )
  }, ignoreInit = FALSE)
  
  # ---------- Reactive getters for selected columns ----------
  current_primary_name <- reactive({
    d <- processed_react(); .validate_processed(d)
    nm <- input$protein_select
    if (is.null(nm) || !nzchar(nm) || !(nm %in% colnames(d))) colnames(d)[1] else nm
  })
  
  current_compare_name <- reactive({
    d <- processed_react(); .validate_processed(d)
    nm <- input$protein_compare_select
    if (is.null(nm) || !nzchar(nm) || !(nm %in% colnames(d))) {
      if (ncol(d) >= 2) colnames(d)[2] else colnames(d)[1]
    } else nm
  })
  
  current_primary_vec <- reactive({
    d <- processed_react(); .validate_processed(d)
    v <- d[[ current_primary_name() ]]
    suppressWarnings(as.numeric(v))
  })
  
  current_compare_vec <- reactive({
    d <- processed_react(); .validate_processed(d)
    v <- d[[ current_compare_name() ]]
    suppressWarnings(as.numeric(v))
  })
  
  # 1) Pie: % Missing for the selected protein (only NA counted as missing)
  output[["missing_by_protein_plot"]] <- echarts4r::renderEcharts4r({
    v  <- current_primary_vec()
    nm <- current_primary_name()
    
    # Totals based strictly on NA:
    n_total   <- length(v)
    n_present <- sum(!is.na(v))      # rows that are NOT NA
    n_missing <- n_total - n_present # rows that ARE NA
    
    validate(need(n_total > 0, "No values."))
    
    df_pie <- data.frame(
      category = c("Missing", "Present"),
      value    = c(n_missing, n_present)
    )
    
    df_pie |>
      echarts4r::e_charts(category) |>
      echarts4r::e_pie(
        value,
        radius = c("40%", "70%"),
        label = list(formatter = "{b}: {d}%")
      ) |>
      echarts4r::e_tooltip(trigger = "item") |>
      echarts4r::e_legend(orient = "vertical", left = "left")
  })
  
  
  # ======================================================================
  # 2) Histogram (unnormalized) for the selected protein
  # ======================================================================
  output[["dist_by_protein_plot"]] <- echarts4r::renderEcharts4r({
    v  <- current_primary_vec()
    nm <- current_primary_name()
    v  <- v[is.finite(v)]
    
    validate(need(length(v) > 0, "No numeric values to plot."))
    
    h <- hist(v, breaks = "FD", plot = FALSE)
    df_h <- data.frame(x = h$mids, y = as.numeric(h$counts))
    
    echarts4r::e_charts(x, data = df_h) |>
      echarts4r::e_bar(y, name = "Count") |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_y_axis(name = "Count") |>
      echarts4r::e_x_axis(name = nm) |>
      echarts4r::e_grid(left = "8%", right = "3%", bottom = "18%") |>
      echarts4r::e_datazoom(type = "inside", x_index = 0) |>
      echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8)
  })
  
  # ======================================================================
  # 3) Boxplot for the selected protein
  # ======================================================================
  output[["boxplot_by_protein"]] <- echarts4r::renderEcharts4r({
    v  <- current_primary_vec()
    nm <- current_primary_name()
    v  <- v[is.finite(v)]
    
    validate(need(length(v) > 0, "No numeric values to plot."))
    
    dfb <- data.frame(group = nm, value = v, stringsAsFactors = FALSE)
    
    dfb |>
      echarts4r::e_charts(group) |>
      echarts4r::e_boxplot(value) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_y_axis(name = "Value") |>
      echarts4r::e_x_axis(axisLabel = list(show = FALSE), name = NULL) |>
      echarts4r::e_grid(left = "10%", right = "12%", bottom = "10%") |>
      echarts4r::e_datazoom(type = "inside", y_index = 0) |>
      echarts4r::e_datazoom(type = "slider", y_index = 0, right = 6)
  })
  
  # ======================================================================
  # 4) Scatter: primary vs comparison protein (row-wise)
  # ======================================================================
  output[["scatter_plot"]] <- echarts4r::renderEcharts4r({
    xname <- current_primary_name()
    yname <- current_compare_name()
    x <- current_primary_vec()
    y <- current_compare_vec()
    
    # Only keep complete rows
    keep <- is.finite(x) & is.finite(y)
    x <- x[keep]; y <- y[keep]
    validate(need(length(x) > 1 && length(y) > 1, "Not enough paired values to plot."))
    
    df_sc <- data.frame(x = x, y = y)
    
    df_sc |>
      echarts4r::e_charts(x) |>
      echarts4r::e_scatter(y, name = paste(yname, "vs", xname)) |>
      echarts4r::e_tooltip(trigger = "item") |>
      echarts4r::e_x_axis(name = xname) |>
      echarts4r::e_y_axis(name = yname) |>
      echarts4r::e_grid(left = "10%", right = "10%", bottom = "16%") |>
      echarts4r::e_datazoom(type = "inside", x_index = 0) |>
      echarts4r::e_datazoom(type = "slider", x_index = 0, bottom = 8) |>
      echarts4r::e_datazoom(type = "inside", y_index = 0) |>
      echarts4r::e_datazoom(type = "slider", y_index = 0)
  })
}
