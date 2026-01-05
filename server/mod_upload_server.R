# Server/mod_upload_server.R
library(shiny)
library(DT)
library(shinyjs)
library(stringr)
library(bslib)

`%||%` <- function(a, b) if (!is.null(a)) a else b

upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    # ---- Reactives ----
    rv <- reactiveValues(
      data = NULL,           # raw / working dataset
      processed_df = NULL,   # cleaned numeric dataset (used by stats)
      processed_path = NULL, # temp file path for download
      profiled = FALSE
    )
    stat_summary_bound    <- reactiveVal(FALSE)
    stat_by_protein_bound <- reactiveVal(FALSE)
    
    # Disable until file load
    set_btn_states <- function(enabled) {
      shinyjs::toggleState(id = ns("transpose_btn"),      condition = enabled)
      shinyjs::toggleState(id = ns("set_headers_btn"),    condition = enabled)
      shinyjs::toggleState(id = ns("upload_profile_btn"), condition = enabled)
      shinyjs::toggleState(id = ns("download_processed"), condition = enabled) # existe en UI (hidden)
    }
    set_btn_states(FALSE)
    
    # ===== Helper para embeber YouTube en modal =====
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
    
    # ===== Configuración por sección/tab + botón dinámico =====
    howto_map <- list(
      preview = list(
        button_label = "How to use: Preview section",
        modal_title  = "How to use: Preview section",
        youtube_id   = "YdoiLBH50Ic"
      ),
      stats = list(
        button_label = "How to use: Statistical Summary section",
        modal_title  = "How to use: Statistical Summary section",
        youtube_id   = "QQW185JSfPc"
      ),
      resume_by_protein = list(
        button_label = "How to use: Resume by Protein section",
        modal_title  = "How to use: Resume by Protein section",
        youtube_id   = "8kVnNonXJAE"
      )
    )
    
    current_upload_tab <- reactive({
      input$upload_tabs %||% "preview"
    })
    
    output$howto_btn_ui <- renderUI({
      tab <- current_upload_tab()
      cfg <- howto_map[[tab]] %||% howto_map[["preview"]]
      
      actionButton(
        inputId = ns("howto_btn"),
        label   = tags$strong(cfg$button_label),
        icon    = icon("circle-play"),
        class   = "btn btn-outline-primary btn-sm",
        style   = "
          margin-top:4px;
          font-weight:700;
          width:100%;
          white-space:normal;
          text-align:left;
        "
      )
    })
    
    # ===== Click -> modal con video según sección =====
    observeEvent(input$howto_btn, {
      tab <- current_upload_tab()
      cfg <- howto_map[[tab]] %||% howto_map[["preview"]]
      
      showModal(modalDialog(
        title = cfg$modal_title,
        tutorial_iframe(paste0("https://www.youtube.com/embed/", cfg$youtube_id)),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    })
    
    # ---------- Helpers ----------
    .safe_ids <- function(x, prefix) {
      x <- as.character(x); x[is.na(x)] <- ""; x <- trimws(x)
      empty <- which(x == ""); if (length(empty)) x[empty] <- paste0(prefix, "_", empty)
      make.unique(x, sep = "_")
    }
    
    .is_numeric_like <- function(x) {
      x <- trimws(as.character(x)); x[x == "" | is.na(x)] <- NA_character_
      test <- gsub(",", ".", x, fixed = TRUE)
      suppressWarnings(!is.na(as.numeric(test)))
    }
    
    .guess_sep <- function(path, n = 1L) {
      line <- tryCatch(readLines(path, n = n, warn = FALSE), error = function(e) "")[1] %||% ""
      cand <- c("," = ",", ";" = ";", "\t" = "\t")
      counts <- vapply(cand, function(s) stringr::str_count(line, stringr::fixed(s)), numeric(1))
      names(which.max(counts))[1] %||% ","
    }
    
    .parse_numeric_vec <- function(x) {
      if (is.numeric(x)) return(x)
      x <- as.character(x); x <- trimws(x)
      x <- gsub("%", "", x, fixed = TRUE)
      x <- gsub("\\s+", "", x)
      both <- grepl("\\.", x) & grepl(",", x)
      x[both] <- gsub(",", "", x[both], fixed = TRUE)
      only_comma <- !grepl("\\.", x) & grepl(",", x)
      x[only_comma] <- gsub(",", ".", x[only_comma], fixed = TRUE)
      suppressWarnings(as.numeric(x))
    }
    
    .clean_numeric_df <- function(df) {
      if (is.null(df) || !is.data.frame(df) || ncol(df) < 1) return(data.frame())
      # Si primera columna es ID (Protein/Patient), la descartamos para análisis numérico
      if (!is.null(colnames(df)) && ncol(df) >= 2 && colnames(df)[1] %in% c("Protein", "Patient")) {
        df <- df[, -1, drop = FALSE]
      }
      out <- lapply(df, .parse_numeric_vec)
      out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
      keep <- vapply(out, function(v) any(is.finite(v)), logical(1))
      out[, keep, drop = FALSE]
    }
    
    # ---------------- Binders (helpers) ----------------
    bind_stat_if_needed <- function() {
      if (isTRUE(stat_summary_bound())) return(invisible(TRUE))
      if (!exists("bind_stat_summary")) {
        showNotification("Statistical Summary bindings not found. Did you source('server/stat_summary_bindings.R')?", type = "error")
        return(invisible(FALSE))
      }
      bind_stat_summary(
        output          = output,
        raw_react       = reactive(rv$data),          # CSV crudo tal cual (para heatmap)
        processed_react = reactive(rv$processed_df),  # DF numérico tras Quick Profiling (para dist/box/donut)
        session         = session
      )
      stat_summary_bound(TRUE)
      invisible(TRUE)
    }
    
    bind_stat_by_protein_if_needed <- function() {
      if (isTRUE(stat_by_protein_bound())) return(invisible(TRUE))
      if (!exists("bind_stat_by_protein")) {
        showNotification("By-Protein bindings not found. Did you source('server/stat_by_protein_bindings.R')?", type = "error")
        return(invisible(FALSE))
      }
      bind_stat_by_protein(
        output          = output,
        processed_react = reactive(rv$processed_df),  # <- SIEMPRE el DF procesado
        input           = input,
        session         = session
      )
      stat_by_protein_bound(TRUE)
      invisible(TRUE)
    }
    
    # ---------------- Header fixer ----------------
    set_headers_protein_layout <- function(
    df,
    protein_col_name = "Protein",
    forbid_numeric_headers = TRUE,
    forbid_numeric_protein_ids = TRUE
    ) {
      validate(need(nrow(df) >= 2, "Need at least 2 rows (header + data)."))
      validate(need(ncol(df) >= 2, "Need at least 2 columns (protein + one patient)."))
      
      raw_headers <- as.character(unlist(df[1, -1], use.names = FALSE))
      hdr_trim <- trimws(raw_headers)
      non_empty_hdr <- hdr_trim[hdr_trim != "" & !is.na(hdr_trim)]
      if (forbid_numeric_headers && length(non_empty_hdr) > 0) {
        if (all(.is_numeric_like(non_empty_hdr))) {
          stop("Row 1 headers are numeric-only. They cannot be used as column names.")
        }
      }
      
      if (forbid_numeric_protein_ids) {
        protein_candidates <- df[-1, 1, drop = TRUE]
        protein_candidates <- protein_candidates[!is.na(protein_candidates) & trimws(protein_candidates) != ""]
        if (length(protein_candidates) > 0) {
          if (any(.is_numeric_like(protein_candidates))) {
            stop("First column (Protein) contains numeric-only values. Fix IDs before applying 'Set Headers'.")
          }
        }
      }
      
      headers <- hdr_trim
      empty_idx <- which(headers == "" | is.na(headers))
      if (length(empty_idx)) headers[empty_idx] <- paste0("Patient_", empty_idx)
      headers <- make.unique(headers, sep = "_")
      colnames(df) <- c(protein_col_name, headers)
      df <- df[-1, , drop = FALSE]; rownames(df) <- NULL
      df[[1]] <- .safe_ids(df[[1]], "Protein")
      
      if (ncol(df) > 1) {
        for (j in 2:ncol(df)) suppressWarnings({ df[[j]] <- as.numeric(df[[j]]) })
      }
      df
    }
    
    # ---------------- Transpose toggle ----------------
    transpose_toggle <- function(df) {
      validate(need(ncol(df) >= 2, "Need at least 2 columns to transpose."))
      if (!is.null(colnames(df)) && identical(colnames(df)[1], "Protein")) {
        proteins <- .safe_ids(df[[1]], "Protein")
        patients <- .safe_ids(colnames(df)[-1], "Patient")
        vals     <- as.matrix(df[, -1, drop = FALSE]); tx_vals  <- t(vals)
        tx <- as.data.frame(tx_vals, stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
        tx <- cbind(setNames(data.frame(patients, stringsAsFactors = FALSE), "Patient"), tx)
        colnames(tx) <- c("Patient", proteins); rownames(tx) <- NULL; return(tx)
      }
      if (!is.null(colnames(df)) && identical(colnames(df)[1], "Patient")) {
        patients <- .safe_ids(df[[1]], "Patient")
        proteins <- .safe_ids(colnames(df)[-1], "Protein")
        vals     <- as.matrix(df[, -1, drop = FALSE]); tx_vals  <- t(vals)
        tx <- as.data.frame(tx_vals, stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
        tx <- cbind(setNames(data.frame(proteins, stringsAsFactors = FALSE), "Protein"), tx)
        colnames(tx) <- c("Protein", patients); rownames(tx) <- NULL; return(tx)
      }
      tx <- as.data.frame(t(as.matrix(df)), stringsAsFactors = FALSE)
      if (is.null(colnames(tx))) colnames(tx) <- paste0("V", seq_len(ncol(tx)))
      rownames(tx) <- NULL; tx
    }
    
    # ---------------- 1) File upload ----------------
    observeEvent(input$upload_file, {
      req(input$upload_file)
      path <- input$upload_file$datapath; req(file.exists(path))
      sep_guess <- .guess_sep(path)
      
      df <- tryCatch(
        utils::read.table(
          path, header = FALSE, sep = sep_guess, quote = "\"", fill = TRUE,
          check.names = FALSE, stringsAsFactors = FALSE, blank.lines.skip = FALSE,
          na.strings = c("NA", "")
        ),
        error = function(e) { showNotification(paste("Failed to read file:", e$message), type = "error"); NULL }
      )
      if (is.null(df) || !is.data.frame(df) || (nrow(df) == 0 && ncol(df) == 0)) {
        showNotification("Read returned no data. Check separator/encoding.", type = "error")
        return(invisible(NULL))
      }
      
      rv$data <- df
      rv$processed_df <- NULL
      rv$processed_path <- NULL
      rv$profiled <- FALSE
      set_btn_states(TRUE)
      shinyjs::hide(id = ns("download_processed"))         # ocultar botón
      shinyjs::toggleState(id = ns("download_processed"), condition = FALSE)
      
      showNotification(sprintf("File loaded (sep='%s'). See Preview tab.", sep_guess), type = "message")
      ## ===== CHANGED 1: usar id sin ns() para tab interna del módulo
      updateTabsetPanel(session, inputId = "upload_tabs", selected = "preview")
    }, ignoreInit = TRUE)
    
    # ---------------- 2) Preview ----------------
    output$upload_preview_tbl <- DT::renderDT({
      d <- rv$data
      validate(need(!is.null(d), ""))
      validate(need(is.data.frame(d) && ncol(d) > 0, "No columns to display."))
      
      d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
      
      DT::datatable(
        d,
        options = list(
          pageLength   = 10,
          lengthChange = TRUE,
          lengthMenu   = list(
            c(10, 20, 50, 100, 200, 500, -1),
            c('10', '20', '50', '100', '200', '500', 'All')
          ),
          scrollX = TRUE,
          dom     = "lftip"
        ),
        rownames = FALSE
      )
    }, server = TRUE)
    
    # ---------------- 3) Transpose ----------------
    observeEvent(input$transpose_btn, {
      req(rv$data)
      set_btn_states(FALSE)
      withProgress(message = "Transposing dataset…", value = 0, {
        incProgress(0.2, detail = "Validating data…")
        d <- rv$data
        incProgress(0.6, detail = "Rearranging matrix…")
        d2 <- transpose_toggle(d)
        incProgress(0.95, detail = "Finalizing…")
        rv$data <- d2
      })
      set_btn_states(TRUE)
      showNotification("Transpose applied, preserving IDs and headers.", type = "message")
    })
    
    # ---------------- 4) Set Headers ----------------
    observeEvent(input$set_headers_btn, {
      req(rv$data)
      x2 <- tryCatch(
        set_headers_protein_layout(rv$data, protein_col_name = "Protein",
                                   forbid_numeric_headers = TRUE, forbid_numeric_protein_ids = TRUE),
        error = function(e) { showNotification(paste("Could not set headers:", e$message), type = "error"); NULL }
      )
      req(!is.null(x2))
      rv$data <- x2
      rv$profiled <- FALSE
      rv$processed_df <- NULL
      rv$processed_path <- NULL
      shinyjs::hide(id = ns("download_processed"))
      shinyjs::toggleState(id = ns("download_processed"), condition = FALSE)
      showNotification("Headers applied (row 1 → patients) and first column preserved (Protein).", type = "message")
      ## ===== CHANGED 2: usar id sin ns() para tab interna del módulo
      updateTabsetPanel(session, inputId = "upload_tabs", selected = "preview")
    })
    
    # ---------------- 5) Quick Profiling ----------------
    observeEvent(input$upload_profile_btn, {
      req(rv$data)
      
      d <- rv$data
      is_protein_by_patient <- (!is.null(colnames(d)) && identical(colnames(d)[1], "Protein"))
      is_patient_by_protein <- (!is.null(colnames(d)) && identical(colnames(d)[1], "Patient"))
      vals <- if (is_protein_by_patient || is_patient_by_protein) d[, -1, drop = FALSE] else d
      
      n_proteins  <- if (is_protein_by_patient) nrow(d) else if (is_patient_by_protein) ncol(d) - 1 else NA_integer_
      n_patients  <- if (is_protein_by_patient) ncol(d) - 1 else if (is_patient_by_protein) nrow(d) else NA_integer_
      total_cells <- as.numeric(nrow(vals) * ncol(vals))
      non_missing <- suppressWarnings(sum(!is.na(as.matrix(vals))))
      
      showModal(modalDialog(
        title = "Confirm: Are headers correct?",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          modalButton("No, review"),
          actionButton(ns("confirm_profiling"), "Yes, continue", class = "btn btn-primary")
        ),
        div(
          p("Before running Quick Profiling, please confirm the dataset orientation:"),
          tags$ul(
            tags$li(strong("Columns = Proteins")),
            tags$li(strong("Rows = Patients"))
          ),
          if (!is.na(n_proteins) && !is.na(n_patients)) {
            div(
              tags$hr(),
              h4("Counts (values only, excluding ID column):"),
              HTML(sprintf("<b>Proteins:</b> %s &nbsp;&nbsp; <b>Patients:</b> %s", n_proteins, n_patients)),
              HTML(sprintf("<br/><b>Total cells:</b> %s &nbsp;&nbsp; <b>Non-missing cells:</b> %s",
                           format(total_cells, big.mark=","), format(non_missing, big.mark=",")))
            )
          } else {
            div(
              tags$hr(),
              h4("Counts (orientation not detected)"),
              HTML(sprintf("<b>Total cells:</b> %s &nbsp;&nbsp; <b>Non-missing cells:</b> %s",
                           format(total_cells, big.mark=","), format(non_missing, big.mark=","))),
              p(class = "text-warning", "Tip: use 'Set Headers' before profiling.")
            )
          }
        )
      ))
    })
    
    # ---------- Confirm profiling: process, save, download, go to stats ----------
    observeEvent(input$confirm_profiling, {
      removeModal()
      rv$profiled <- TRUE
      
      # 1) Build processed DF (robust numeric cleaning)
      proc <- .clean_numeric_df(rv$data)
      
      # ... (resto del original sin cambios)
      
      rv$processed_df <- proc
      
      # ---- GUARDAR IDs + BASE NUMÉRICA (tmp base) PARA TODA LA APP ----
      d_raw <- rv$data
      base_ids <- NULL
      
      if (!is.null(d_raw) && is.data.frame(d_raw) &&
          !is.null(colnames(d_raw)) &&
          colnames(d_raw)[1] %in% c("Protein", "Patient")) {
        base_ids <- as.character(d_raw[[1]])
      } else if (nrow(proc) > 0) {
        base_ids <- paste0("Row_", seq_len(nrow(proc)))
      }
      
      cat(">>> [upload] seteando qp_row_ids / qp_tmp_base\n")
      cat("    base_ids length:", if (is.null(base_ids)) "NULL" else length(base_ids), "\n")
      if (!is.null(proc)) {
        cat("    proc dims:", paste(dim(proc), collapse = " x "), "\n")
      }
      
      session$userData$qp_row_ids  <- reactive(base_ids)
      session$userData$qp_tmp_base <- reactive(proc)
      
      # Publish for the Transform module:
      session$userData$tfm_base <- reactive(rv$processed_df)
      
      # 2) Save to a temp CSV for download
      f <- tempfile(pattern = "processed_", fileext = ".csv")
      utils::write.csv(proc, f, row.names = FALSE, na = "")
      rv$processed_path <- f
      
      # 3) Show + enable + auto-click download button
      shinyjs::show(id = ns("download_processed"))
      shinyjs::toggleState(id = ns("download_processed"), condition = TRUE)
      shinyjs::click(id = ns("download_processed"))
      
      # 4) Go to stats tab
      updateTabsetPanel(session, inputId = "upload_tabs", selected = "stats")
      
      showNotification("Quick profiling confirmed. Processed CSV generated and downloaded.", type = "message")
    })
    
    # 6) Download handler (serves latest processed file)
    output$download_processed <- downloadHandler(
      filename = function() {
        paste0("processed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$processed_path, file.exists(rv$processed_path))
        file.copy(rv$processed_path, file, overwrite = TRUE)
      }
    )
    
    # Bind stats when entering tabs
    observeEvent(input$upload_tabs, {
      if (identical(input$upload_tabs, "stats")) {
        bind_stat_if_needed()
      }
      if (identical(input$upload_tabs, "resume_by_protein")) {
        if (is.null(rv$processed_df)) {
          showNotification("Run Quick Profiling first.", type = "warning")
        } else {
          bind_stat_by_protein_if_needed()
        }
      }
    }, ignoreInit = FALSE)
    
    # ... (resto del original sin cambios)
    
    observeEvent(input$go_transform_btn, {
      session$onFlushed(function() {
        shinyjs::runjs("
      (function(){
        var navId   = 'main_nav';
        var value   = 'transform_tab';
        var titleTx = 'Transformación';

        function log(msg){ try{ console.log('[go_transform]', msg); }catch(e){} }

        function clickEl(el){
          if (!el) return false;
          try {
            var ev = new MouseEvent('click', {bubbles:true, cancelable:true, view:window});
            var ok = el.dispatchEvent(ev);
            if (!ok && typeof el.click === 'function') el.click();
            return true;
          } catch(e){ return false; }
        }

        // 1) por data-value (global)
        var byVal = document.querySelector('[data-value=\"'+value+'\"]');
        if (clickEl(byVal)) { log('click por data-value (global)'); return; }

        // 2) por data-value dentro del navbar
        var byValScoped = document.querySelector('#'+navId+' [data-value=\"'+value+'\"]');
        if (clickEl(byValScoped)) { log('click por data-value (scoped)'); return; }

        // 3) por texto exacto dentro del navbar
        var links = document.querySelectorAll('#'+navId+' .nav-link, #'+navId+' a.nav-link, #'+navId+' button.nav-link');
        for (var i=0;i<links.length;i++){
          if ((links[i].textContent||'').trim() === titleTx){
            if (clickEl(links[i])) { log('click por texto exacto'); return; }
          }
        }

        // 4) por texto 'incluye' (para espacios o íconos)
        for (var j=0;j<links.length;j++){
          var txt = (links[j].textContent||'').replace(/\\s+/g,' ').trim();
          if (txt.indexOf(titleTx) !== -1){
            if (clickEl(links[j])) { log('click por texto (incluye)'); return; }
          }
        }

        // 5) Último recurso: empujar input al servidor
        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('main_nav', value, {priority:'event'});
          log('setInputValue fallback');
        } else {
          log('no se encontró target del tab :(');
        }
      })();
    ")
      }, once = TRUE)
    })
    
    # dentro de observeEvent(input$go_transform_btn, { ... })
    session$userData$tfm_base <- reactive(rv$processed_df)
    
    # ------------- Reactives for backwards-compat (not used now) -------------
    num_df <- reactive({
      req(rv$data)
      d <- rv$data
      if (!is.null(colnames(d)) && ncol(d) >= 2 && colnames(d)[1] %in% c("Protein", "Patient")) {
        dnum <- d[, -1, drop = FALSE]
      } else dnum <- d
      for (j in seq_along(dnum)) if (!is.numeric(dnum[[j]])) suppressWarnings({ dnum[[j]] <- as.numeric(dnum[[j]]) })
      dnum[, sapply(dnum, is.numeric), drop = FALSE]
    })
  })
}