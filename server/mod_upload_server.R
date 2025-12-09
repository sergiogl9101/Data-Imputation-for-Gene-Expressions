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
    # ---------- Confirm profiling: process, save, download, go to stats ----------
    observeEvent(input$confirm_profiling, {
      removeModal()
      rv$profiled <- TRUE
      
      # 1) Build processed DF (robust numeric cleaning)
      proc <- .clean_numeric_df(rv$data)
      
      # --- NEW: global and per-column missingness validation ---
      if (!is.null(proc) && is.data.frame(proc) && nrow(proc) > 0 && ncol(proc) > 0) {
        miss_mat     <- is.na(as.matrix(proc))
        total_cells  <- nrow(proc) * ncol(proc)
        missing_global <- if (total_cells > 0) {
          sum(miss_mat) / total_cells * 100
        } else {
          NA_real_
        }
        
        missing_by_col <- colMeans(miss_mat) * 100
        
        if (is.finite(missing_global) && missing_global >= 45) {
          # Build HTML table with per-column NA%
          miss_df <- data.frame(
            Variable       = colnames(proc),
            `Missing (%)`  = round(missing_by_col, 1),
            stringsAsFactors = FALSE,
            check.names      = FALSE
          )
          
          rows_html <- apply(miss_df, 1, function(r) {
            var <- r[["Variable"]]
            pct <- as.numeric(r[["Missing (%)"]])
            bg  <- "#f1f3f5"
            col <- "#212529"
            
            if (is.finite(pct)) {
              if (pct < 30) {
                bg  <- "rgba(209,247,214,.8)"   # greenish
                col <- "#0f5132"
              } else if (pct < 60) {
                bg  <- "rgba(255,243,205,.9)"   # yellow
                col <- "#664d03"
              } else {
                bg  <- "rgba(248,215,218,.9)"   # reddish
                col <- "#842029"
              }
            }
            
            sprintf(
              "<tr><td>%s</td><td style='background-color:%s;color:%s;font-weight:600;'>%.1f&nbsp;%%</td></tr>",
              htmltools::htmlEscape(var), bg, col, pct
            )
          })
          
          table_html <- paste0(
            "<table class='table table-sm table-bordered' style='font-size:0.9rem;'>",
            "<thead><tr><th>Variable</th><th>Missing (%)</th></tr></thead>",
            "<tbody>", paste(rows_html, collapse = ""), "</tbody></table>"
          )
          
          showModal(modalDialog(
            title = "Dataset rejected for downstream analysis",
            size  = "l",
            easyClose = TRUE,
            footer = modalButton("Close"),
            HTML(sprintf(
              "<p><b>Global missingness: %.1f&nbsp;%%</b> (≥ 45&nbsp;%%).</p>",
              missing_global
            )),
            HTML(
              "<p>With this level of missing data, any imputation would become highly speculative.<br/>
           Please review the original file, remove variables with too many missing values,
           or pre-filter the dataset before running Quick Profiling again.</p>"
            ),
            HTML(
              "<p>Color scale by column:<br/>
           <span style='background-color:rgba(209,247,214,.8);padding:2px 6px;margin-right:4px;'>Low (&lt; 30&nbsp;%)</span>
           <span style='background-color:rgba(255,243,205,.9);padding:2px 6px;margin-right:4px;'>Medium (30–60&nbsp;%)</span>
           <span style='background-color:rgba(248,215,218,.9);padding:2px 6px;'>High (&gt; 60&nbsp;%)</span>
           </p>"
            ),
            HTML(table_html),
            HTML(
              "<p style='margin-top:0.75rem;font-style:italic;'>
           Note: once global missingness is around ~45&nbsp;% or higher, the dataset is
           no longer considered reliable for downstream normalization and imputation modules
           in this app, so it is intentionally <b>not saved nor sent forward</b>.
           </p>"
            )
          ))
          
          showNotification(
            sprintf(
              "Dataset was NOT saved or sent to Transform/Imputation: global missingness is %.1f%% (≥ 45%%).",
              missing_global
            ),
            type = "error"
          )
          
          # Make sure this dataset is NOT used downstream
          rv$profiled       <- FALSE
          rv$processed_df   <- NULL
          rv$processed_path <- NULL
          shinyjs::hide(id = ns("download_processed"))
          shinyjs::toggleState(id = ns("download_processed"), condition = FALSE)
          
          return(invisible(NULL))
        }
      }
      # --- END NEW BLOCK ---
      
      # If it passes the check, continue as before:
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
    
    # ===== CAMBIO FINAL: Navegar a "Transformación" usando bslib::nav_select con reintento diferido =====
    # ===== FIX ULTRA-ROBUSTO (corregido): navegación por JS con 4 estrategias + logs =====
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
    
    # --- AÑADIR (refrescar dataset y gatillar reload en transform) ---
    session$userData$tfm_base <- reactive(rv$processed_df)
    # ---------------------------------------------------------------
    
    # ===== FIN FIX ULTRA-ROBUSTO (corregido) =====
    
    
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
