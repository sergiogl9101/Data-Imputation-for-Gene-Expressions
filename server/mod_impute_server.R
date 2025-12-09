# R/mod_impute_server.R
# Server: Imputation panel
# Server/mod_impute_server.R
source("server/hp_suggest.R", local = TRUE)
source("R/utils.R", local = TRUE) 
source("server/transform_pipeline.R", local = TRUE) 
# Server/mod_impute_server.R
source("server/transform_pipeline.R", local = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b  # por si aquí no lo tenías


impute_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # 🔢 Helper: redondear todas las columnas numéricas
    round_numeric_df <- function(df, digits = 6) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      df[] <- lapply(df, function(x) {
        if (is.numeric(x)) round(x, digits) else x
      })
      df
    }
    
    # ===== Definición de modelos disponibles (UI) =====
    # Single imputation (sin MICE)
    stat_plain_choices <- c(
      "Mean Imputation"   = "mean_plain",
      "Median Imputation" = "median_plain",
      "Linear Regression" = "linear_plain"
    )
    
    ml_plain_choices <- c(
      "KNN Imputation"  = "knn_plain",
      "LS Impute"       = "ls_plain",
      "BPCA Impute"     = "bpca_plain"
    )
    
    # MICE variants (multiple imputation)
    mice_choices <- c(
      "Mean (MICE)"              = "mean_mice",
      "Median (MICE)"            = "median_mice",
      "Linear Regression (MICE)" = "linear_mice",
      "KNN (MICE)"               = "knn_mice",
      "LS (MICE)"                = "ls_mice",
      "BPCA (MICE)"              = "bpca_mice"
    )
    
    rv_imp <- reactiveValues(
      active_df              = NULL,
      base_df                = NULL,
      current_df             = NULL,
      z_df                   = NULL,
      yj_df                  = NULL,
      
      transform_pipeline     = character(0),  # orden de pasos ("log", "zscore", "yeojohnson", ...)
      pipeline_meta          = NULL,          # NUEVO: parámetros para invertir el pipeline
      
      recommended_method     = NULL,
      recommended_family     = NULL,
      recommended_model_code = NULL,
      recommendation_reason  = NULL,
      
      # Dataset imputado:
      #  - current_imputed_df        = SIEMPRE en escala ORIGINAL (des-transformado)
      #  - current_imputed_df_scaled = imputado en escala transformada (log/z/YJ)
      current_imputed_df          = NULL,     
      current_imputed_df_scaled   = NULL,     # NUEVO
      mice_diagnostics            = NULL,
      
      # sugerencias de hiperparámetros
      hp_suggest             = list(
        knn_k    = NULL,
        bpca_nc  = NULL,
        ls_tol   = NULL
      ),
      
      # resultados de evaluate_imputer() (utils.R)
      eval_results           = NULL          
    )
    
    
    # ===== Helper: limpiar resultados de imputación y preview =====
    clear_imputation_results <- function() {
      rv_imp$current_imputed_df        <- NULL      # des-transformado
      rv_imp$current_imputed_df_scaled <- NULL      # transformado
      rv_imp$mice_diagnostics          <- NULL
      rv_imp$eval_results              <- NULL
      
      # limpiar payload global de visualización
      rv$viz_payload <- NULL
      
      # dejar el preview vacío
      output$imp_preview_data <- DT::renderDataTable({
        DT::datatable(
          data.frame(),
          options  = list(dom = 't'),
          rownames = FALSE
        )
      })
      
      # ocultar la card final (si existe)
      shinyjs::hide(id = ns("final_viz_card_container"))
    }
    
    
    # Inicialmente el preview va vacío
    clear_imputation_results()
    
    
    # ===== Ejecutar hp_suggest APENAS se cargue el módulo (si ya hay data) =====
    session$onFlushed(function() {
      isolate({
        df <- rv_imp$active_df
        if (!is.null(df)) {
          df_num <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
          if (ncol(df_num) > 0) {
            rv_imp$hp_suggest <- list(
              knn_k   = suggest_knn_k(df_num),
              bpca_nc = suggest_bpca_ncomp(df_num),
              ls_tol  = suggest_ls_tolerance(df_num)
            )
          }
        }
      })
    }, once = TRUE)
    
    
    # ===== Cargar payload desde Transform (siempre usamos current_df) =====
    # NOTA: 'rv' es un reactiveValues global definido en app.R
    observeEvent(rv$impute_payload, {
      ep <- rv$impute_payload
      req(ep)
      
      clear_imputation_results()
      
      rv_imp$base_df    <- ep$base_df
      rv_imp$current_df <- ep$current_df
      rv_imp$z_df       <- ep$z_df
      rv_imp$yj_df      <- ep$yj_df
      
      # Dataset ACTIVO que usará recomendador e imputación
      rv_imp$active_df  <- ep$current_df
      
      # NUEVO: pipeline de transformaciones que viene de Transform
      rv_imp$transform_pipeline <- ep$transform_pipeline %||% character(0)
      
      # ---- NUEVO: calcular sugerencias de hiperparámetros ----
      df_num <- ep$current_df[, vapply(ep$current_df, is.numeric, logical(1)), drop = FALSE]
      if (ncol(df_num) > 0) {
        rv_imp$hp_suggest <- list(
          knn_k   = suggest_knn_k(df_num),
          bpca_nc = suggest_bpca_ncomp(df_num),
          ls_tol  = suggest_ls_tolerance(df_num)
        )
      } else {
        rv_imp$hp_suggest <- list(knn_k = NULL, bpca_nc = NULL, ls_tol = NULL)
      }
      
      showNotification(
        "Imputation payload received (using transformed current dataset).",
        type = "message"
      )
    }, ignoreInit = FALSE)
    
    # ===== Actualizar modelos según la categoría seleccionada =====
    # ===== Actualizar modelos según la categoría seleccionada =====
    # OJO: aquí ya NO fijamos 'selected', solo cambiamos las choices.
    # El recomendado se fija en el observer del botón "Recommend Method".
    observeEvent(input$imp_model_category, {
      cat_val  <- input$imp_model_category %||% ""
      rec_code <- isolate(rv_imp$recommended_model_code)
      
      if (cat_val == "Statistical (Single Imputation)") {
        sel <- if (!is.null(rec_code) && rec_code %in% stat_plain_choices) rec_code else NULL
        
        updateSelectInput(
          session, "imp_model_list",
          choices  = stat_plain_choices,
          selected = sel
        )
        
      } else if (cat_val == "ML (Single Imputation)") {
        sel <- if (!is.null(rec_code) && rec_code %in% ml_plain_choices) rec_code else NULL
        
        updateSelectInput(
          session, "imp_model_list",
          choices  = ml_plain_choices,
          selected = sel
        )
        
      } else if (cat_val == "MICE (Multiple Imputation)") {
        sel <- if (!is.null(rec_code) && rec_code %in% mice_choices) rec_code else NULL
        
        updateSelectInput(
          session, "imp_model_list",
          choices  = mice_choices,
          selected = sel
        )
      }
    }, ignoreInit = FALSE)
    
    
    # ===== Textos del recomendador en la UI =====
    output$imp_recommended_method <- renderText({
      rv_imp$recommended_method %||% "No method recommended yet."
    })
    
    output$imp_recommendation_reason <- renderText({
      rv_imp$recommendation_reason %||%
        "Click 'Recommend Method' to analyze the dataset and get a suggestion."
    })
    
    # ======== Función recomendador (usa SIEMPRE el current/active_df) ========
    # ================== Model choice maps (para labels) ==================
    # Usamos los mismos códigos que la UI
    stat_single_choices <- stat_plain_choices   # mean_plain, median_plain, linear_plain
    ml_single_choices   <- ml_plain_choices     # knn_plain, ls_plain, bpca_plain
    mice_label_choices  <- mice_choices         # mean_mice, knn_mice, etc.
    
    # ================== Recommender core ==================
    # ================== Recommender==================
    recommend_model <- function(df_num) {
      df_num <- as.data.frame(df_num, check.names = FALSE, stringsAsFactors = FALSE)
      
      # Asegurar solo numéricas
      num_mask <- vapply(df_num, function(x) is.numeric(x) || is.integer(x), logical(1))
      df_num   <- df_num[, num_mask, drop = FALSE]
      
      n <- nrow(df_num)
      p <- ncol(df_num)
      
      if (n == 0 || p == 0) {
        stop("No numeric data available for recommendation.")
      }
      
      total_cells   <- n * p
      missing_global <- sum(is.na(df_num)) / total_cells * 100
      if (!is.finite(missing_global)) missing_global <- 0
      
      # ---- Missingness por columna ----
      missing_col <- colMeans(is.na(df_num)) * 100
      max_missing_col    <- max(missing_col, na.rm = TRUE)
      median_missing_col <- stats::median(missing_col, na.rm = TRUE)
      prop_cols_gt30     <- if (length(missing_col)) mean(missing_col > 30, na.rm = TRUE) else NA_real_
      prop_cols_gt50     <- if (length(missing_col)) mean(missing_col > 50, na.rm = TRUE) else NA_real_
      prop_cols_gt70     <- if (length(missing_col)) mean(missing_col > 70, na.rm = TRUE) else NA_real_
      
      # ---- Correlación ----
      mean_abs_cor <- NA_real_
      if (p >= 2) {
        cor_mat <- suppressWarnings(cor(df_num, use = "pairwise.complete.obs"))
        if (all(dim(cor_mat) > 1)) {
          upper <- cor_mat[upper.tri(cor_mat)]
          upper <- upper[is.finite(upper)]
          if (length(upper) > 0) {
            mean_abs_cor <- mean(abs(upper))
          }
        }
      }
      
      # ---- Skewness & Kurtosis ----
      sk <- vapply(df_num, function(col) {
        v <- suppressWarnings(as.numeric(col))
        v <- v[is.finite(v)]
        if (length(v) < 3) return(NA_real_)
        e1071::skewness(v, na.rm = TRUE)
      }, numeric(1))
      
      ku <- vapply(df_num, function(col) {
        v <- suppressWarnings(as.numeric(col))
        v <- v[is.finite(v)]
        if (length(v) < 4) return(NA_real_)
        e1071::kurtosis(v, na.rm = TRUE) - 3  # exceso de kurtosis
      }, numeric(1))
      
      prop_skewed <- if (all(is.na(sk))) NA_real_ else mean(abs(sk) > 1, na.rm = TRUE)
      prop_heavy_tails <- if (all(is.na(ku))) NA_real_ else mean(ku > 2, na.rm = TRUE)
      
      p_over_n <- if (n > 0) p / n else Inf
      
      # ---- Texto base de justificación ----
      reason_parts <- c(
        sprintf("Global missingness: %.1f%%.", missing_global),
        sprintf("Max missingness by column: %.1f%%.", max_missing_col),
        sprintf("Median missingness by column: %.1f%%.", median_missing_col),
        sprintf(
          "Mean absolute correlation: %s.",
          if (!is.finite(mean_abs_cor)) "not enough columns" else sprintf("%.2f", mean_abs_cor)
        ),
        sprintf(
          "Proportion of strongly skewed variables (|skew| > 1): %s.",
          if (!is.finite(prop_skewed)) "N/A" else sprintf("%.2f", prop_skewed)
        ),
        sprintf(
          "Proportion of heavy-tailed variables (excess kurtosis > 2): %s.",
          if (!is.finite(prop_heavy_tails)) "N/A" else sprintf("%.2f", prop_heavy_tails)
        )
      )
      
      if (is.finite(prop_cols_gt50) && prop_cols_gt50 > 0.4) {
        reason_parts <- c(
          reason_parts,
          sprintf("Around %.0f%% of the variables have over 50%% missingness; ",
                  prop_cols_gt50 * 100),
          "complex models can be fragile for those variables."
        )
      }
      if (is.finite(prop_cols_gt70) && prop_cols_gt70 > 0.2) {
        reason_parts <- c(
          reason_parts,
          sprintf("Roughly %.0f%% of the variables have over 70%% missingness; ",
                  prop_cols_gt70 * 100),
          "you should consider dropping them before imputing."
        )
      }
      
      # --------- Corte duro: demasiado faltante ---------
      if (missing_global >= 45) {
        reason_parts <- c(
          reason_parts,
          "Global missingness is above 45%%. This dataset is outside the recommended range for imputation; ",
          "please review it at the upload step."
        )
        return(list(
          code            = NA_character_,
          family          = "Not recommended",
          label           = "No model recommended",
          reason          = paste(reason_parts, collapse = " "),
          imputation_type = "None"
        ))
      }
      
      # ================== 1) Modelo base SOLO por % de NA ==================
      model_code      <- NULL
      family_label    <- NULL
      imputation_type <- NULL
      
      if (missing_global < 2) {
        # Muy pocos NA → single simple
        family_label    <- "Statistical (Single Imputation)"
        imputation_type <- "Single"
        
        if (is.finite(prop_skewed) && prop_skewed < 0.3 &&
            is.finite(prop_heavy_tails) && prop_heavy_tails < 0.3) {
          model_code <- "mean_plain"
          reason_parts <- c(
            reason_parts,
            "Global missingness < 2%. A simple mean single-imputation is adequate for descriptive work on mostly symmetric variables."
          )
        } else {
          model_code <- "median_plain"
          reason_parts <- c(
            reason_parts,
            "Global missingness < 2%, but many variables are skewed or heavy-tailed. A median single-imputation is safer."
          )
        }
        
      } else if (missing_global < 5) {
        # Bajo (2–5%) → Single por defecto, MICE solo como recomendación en texto
        family_label    <- "Statistical (Single Imputation)"
        imputation_type <- "Single"
        
        if (is.finite(prop_skewed) && prop_skewed < 0.3 &&
            is.finite(prop_heavy_tails) && prop_heavy_tails < 0.3) {
          model_code <- "mean_plain"
        } else {
          model_code <- "median_plain"
        }
        
        reason_parts <- c(
          reason_parts,
          "Global missingness between 2% and 5%. Single-imputation is still acceptable, but MICE would be preferable if you plan to do formal inference."
        )
        
      } else if (missing_global <= 25) {
        # Moderado (5–25%) → MICE sí o sí (tipo se decide luego)
        family_label    <- "MICE (Multiple Imputation)"
        imputation_type <- "MICE"
        
        if (is.finite(prop_skewed) && prop_skewed < 0.3 &&
            is.finite(prop_heavy_tails) && prop_heavy_tails < 0.3) {
          model_code <- "mean_mice"
        } else {
          model_code <- "median_mice"
        }
        
        reason_parts <- c(
          reason_parts,
          "Global missingness between 5% and 25%. Multiple imputation (MICE) is recommended to propagate uncertainty."
        )
        
      } else if (missing_global <= 40) {
        # Alto (25–40%) → MICE obligado, backbone robusto
        family_label    <- "MICE (Multiple Imputation)"
        imputation_type <- "MICE"
        model_code      <- "median_mice"
        
        reason_parts <- c(
          reason_parts,
          "Global missingness between 25% and 40%. Multiple imputation is strongly recommended; a robust median-based backbone is used by default."
        )
        
      } else { # 40–45%
        family_label    <- "MICE (Multiple Imputation)"
        imputation_type <- "MICE"
        model_code      <- "median_mice"
        
        reason_parts <- c(
          reason_parts,
          "Global missingness above 40% (but below 45%). Any imputation will be fragile; a conservative median-based MICE model is chosen."
        )
      }
      
      # ================== 2) Overrides por otras métricas ==================
      # Aquí las OTRAS variables pueden "sobresalir",
      # pero SIN cambiar el nivel de missingness (Single vs MICE se mantiene).
      
      # ---- SINGLE (bajo %NA): usar regresión si correlaciones son muy fuertes ----
      if (identical(imputation_type, "Single") &&
          missing_global >= 2 && missing_global < 5 &&
          is.finite(mean_abs_cor) && mean_abs_cor >= 0.7 &&
          n >= 50 && p >= 2 &&
          (is.na(prop_skewed) || prop_skewed <= 0.5)) {
        
        model_code <- "linear_plain"
        reason_parts <- c(
          reason_parts,
          "Correlations between variables are very strong with low missingness. A regression-based single imputation is preferred over pure mean/median."
        )
      }
      
      # ---- MICE (5–25%): usar KNN/linear si la estructura es muy marcada ----
      if (identical(imputation_type, "MICE") &&
          missing_global >= 5 && missing_global <= 25) {
        
        if (is.finite(mean_abs_cor) && mean_abs_cor >= 0.6 &&
            n >= 100 &&
            is.finite(prop_cols_gt50) && prop_cols_gt50 < 0.3) {
          
          model_code <- "knn_mice"
          reason_parts <- c(
            reason_parts,
            "Correlations are strong, sample size is sufficient and most variables have <50% missingness. A KNN-based MICE model can exploit the structure better than pure mean/median."
          )
          
        } else if (is.finite(mean_abs_cor) && mean_abs_cor >= 0.5 &&
                   n < 100 &&
                   is.finite(prop_cols_gt50) && prop_cols_gt50 < 0.3) {
          
          model_code <- "linear_mice"
          reason_parts <- c(
            reason_parts,
            "Correlations are strong but sample size is modest. A regression-based MICE model is more stable than KNN here."
          )
        }
      }
      
      # ---- MICE (25–45%): LS o BPCA si la estructura lo justifica ----
      if (identical(imputation_type, "MICE") &&
          missing_global > 25 && missing_global < 45) {
        
        if (is.finite(p_over_n) && p_over_n > 1 &&
            is.finite(prop_cols_gt50) && prop_cols_gt50 < 0.5) {
          
          model_code <- "bpca_mice"
          reason_parts <- c(
            reason_parts,
            "High missingness with p > n and at least half of the variables below 50% missingness. A BPCA-based MICE model is appropriate for high-dimensional data."
          )
          
        } else if (p < 50 &&
                   is.finite(mean_abs_cor) && mean_abs_cor >= 0.3 &&
                   is.finite(prop_cols_gt50) && prop_cols_gt50 < 0.5) {
          
          model_code <- "ls_mice"
          reason_parts <- c(
            reason_parts,
            "High missingness, moderate dimensionality and non-trivial correlations. An LS-based MICE model stabilizes imputation while reflecting uncertainty."
          )
        }
      }
      
      # ================== 3) Label legible ==================
      label <- switch(
        family_label,
        "Statistical (Single Imputation)" = {
          nm <- names(stat_single_choices)[stat_single_choices == model_code]
          if (length(nm) == 0) model_code else nm
        },
        "ML (Single Imputation)" = {
          nm <- names(ml_single_choices)[ml_single_choices == model_code]
          if (length(nm) == 0) model_code else nm
        },
        "MICE (Multiple Imputation)" = {
          nm <- names(mice_label_choices)[mice_label_choices == model_code]
          if (length(nm) == 0) model_code else nm
        },
        model_code
      )
      
      list(
        code            = model_code,
        family          = family_label,
        label           = label,
        reason          = paste(reason_parts, collapse = " "),
        imputation_type = imputation_type
      )
    }
    

    
    observeEvent(input$imp_run_recommend_method, {
      df <- rv_imp$active_df
      if (is.null(df)) {
        showNotification("No active dataset available for recommendation.", type = "error")
        return(NULL)
      }
      
      # Solo columnas numéricas
      num_cols <- vapply(df, is.numeric, logical(1))
      df_num   <- df[, num_cols, drop = FALSE]
      
      if (ncol(df_num) == 0) {
        showNotification("No numeric columns available for recommendation.", type = "error")
        return(NULL)
      }
      
      # Notificación tipo "loading"
      load_id <- showNotification(
        "Analyzing dataset and searching for a good imputation model...",
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(load_id), add = TRUE)
      
      rec <- NULL
      withProgress(message = "Computing recommendation...", value = 0, {
        incProgress(0.6, detail = "Analyzing missingness and correlation structure...")
        rec <- recommend_model(df_num)
        incProgress(0.4, detail = "Updating UI with suggested model...")
      })
      
      rv_imp$recommended_method     <- rec$label
      rv_imp$recommended_family     <- rec$family
      rv_imp$recommended_model_code <- rec$code
      rv_imp$recommendation_reason  <- rec$reason
      rv_imp$recommended_type       <- rec$imputation_type  # "Single" o "MICE"
      
      # Si la familia es "Not recommended", no intentamos sincronizar selects
      if (!identical(rec$family, "Not recommended")) {
        updateSelectInput(
          session, "imp_model_category",
          selected = rec$family
        )
      }
      
      # Mensaje extra explicando por qué Single vs MICE
      extra_note <- if (identical(rec$imputation_type, "MICE")) {
        "This recommendation uses a MICE-based model because the amount and pattern of missingness make single imputation too optimistic for most analyses."
      } else if (identical(rec$imputation_type, "Single")) {
        "This recommendation uses a single-imputation model; if you plan to run formal inference or compare groups, consider using the same backbone model within a MICE workflow."
      } else {
        ""
      }
      
      showNotification(
        paste("Recommended model:", rec$label, "-", extra_note),
        type     = if (rec$family == "Not recommended") "error" else "message",
        duration = 8
      )
    })
    
    
    # ===== UI de hiperparámetros según modelo seleccionado =====
    observeEvent(input$imp_model_list, {
      model <- input$imp_model_list
      if (is.null(model)) return(NULL)
      
      clear_imputation_results()
      shinyjs::hide(id = ns("final_viz_card_container"))  # <- redundante pero claro
      
      
      # ---------------- SINGLE IMPUTATION (sin MICE) ----------------
      if (model == "mean_plain") {
        output$imp_model_config <- renderUI({
          tagList(
            helpText("Single mean imputation on the active dataset."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
        
      } else if (model == "median_plain") {
        output$imp_model_config <- renderUI({
          tagList(
            helpText("Single median imputation on the active dataset."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
        
      } else if (model == "linear_plain") {
        output$imp_model_config <- renderUI({
          tagList(
            helpText("Single regression-based imputation. You can later extend this to select predictors per variable."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
        
      } else if (model == "knn_plain") {
        output$imp_model_config <- renderUI({
          k_default <- rv_imp$hp_suggest$knn_k %||% 5L
          tagList(
            numericInput(
              ns("knn_neighbors_plain"),
              "Number of neighbors (KNN)",
              value = k_default, min = 1, step = 1
            ),
            helpText("Single KNN-based imputation on the active dataset."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
        
      } else if (model == "ls_plain") {
        output$imp_model_config <- renderUI({
          ls_default <- rv_imp$hp_suggest$ls_tol %||% 1e-4
          tagList(
            numericInput(
              ns("ls_tolerance_plain"),
              "LS tolerance",
              value = ls_default, min = 1e-8, step = 1e-4
            ),
            helpText("Single least-squares-based imputation on the active dataset."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
        
      } else if (model == "bpca_plain") {
        output$imp_model_config <- renderUI({
          nc_default <- rv_imp$hp_suggest$bpca_nc %||% 2L
          tagList(
            numericInput(
              ns("bpca_ncomp_plain"),
              "Number of components (BPCA)",
              value = nc_default, min = 1, step = 1
            ),
            helpText("Single BPCA-based imputation on the active dataset."),
            actionButton(ns("imp_run_imputation"), "Run Imputation", class = "btn btn-success")
          )
        })
      
        
        # ---------------- MICE VARIANTS ----------------
      } else if (model %in% c("mean_mice", "median_mice", "linear_mice",
                              "knn_mice", "ls_mice", "bpca_mice")) {
        
        output$imp_model_config <- renderUI({
          # Defaults sugeridos (si no hay, usa fallback)
          k_default  <- rv_imp$hp_suggest$knn_k   %||% 5L
          nc_default <- rv_imp$hp_suggest$bpca_nc %||% 2L
          ls_default <- rv_imp$hp_suggest$ls_tol  %||% 1e-4
          
          tagList(
            # Contenedor centrado para los inputs
            div(
              style = "max-width: 420px; margin: 0 auto;",
              
              numericInput(
                ns("mice_m"),
                "Number of imputed datasets (m)",
                value = 5, min = 1, step = 1
              ),
              numericInput(
                ns("mice_maxit"),
                "Number of iterations (maxit)",
                value = 10, min = 1, step = 1
              ),
              helpText("Typical values: m = 5–10; maxit = 5–20 depending on missingness."),
              
              # ---- Hiperparámetros específicos según el modelo ----
              if (model == "knn_mice") {
                numericInput(
                  ns("knn_neighbors_mice"),
                  "KNN neighbors (for MICE step)",
                  value = k_default,
                  min   = 1,
                  step  = 1
                )
              } else NULL,
              
              if (model == "bpca_mice") {
                numericInput(
                  ns("bpca_ncomp_mice"),
                  "Number of components (BPCA in MICE)",
                  value = nc_default,
                  min   = 1,
                  step  = 1
                )
              } else NULL,
              
              if (model == "ls_mice") {
                numericInput(
                  ns("ls_tolerance_mice"),
                  "LS tolerance (for LS-based MICE)",
                  value = ls_default,
                  min   = 1e-8,
                  step  = 1e-4
                )
              } else NULL,
              
              helpText("These hyperparameters will be used inside MICE for each chained equation.")
            ),
            
            # Botón grande, centrado y alineado hacia abajo
            div(
              style = "text-align:center; margin-top: 20px;",
              actionButton(
                ns("imp_run_imputation"),
                "Run MICE Imputation",
                class = "btn btn-success btn-lg"
              )
            )
          )
        })
      }
      
    })
    
    
    # ===== Ejecutar imputación (ahora sí con los modelos reales) =====
    observeEvent(input$imp_run_imputation, {
      req(rv_imp$active_df)
      
      model <- input$imp_model_list
      df    <- rv_imp$active_df
      
      validate(
        need(!is.null(model), "Selecciona un modelo de imputación.")
      )
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      validate(
        need(length(num_cols) > 0, "No hay columnas numéricas para imputar.")
      )
      
      # Cada vez que corres una imputación nueva, limpiamos resultados anteriores
      clear_imputation_results()

      is_mice <- model %in% c(
        "mean_mice", "median_mice", "linear_mice",
        "knn_mice", "ls_mice", "bpca_mice"
      )
      
      # Notificación tipo "loading"
      load_id <- showNotification(
        "Running evaluation and imputation, please wait...",
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(load_id), add = TRUE)
      
      withProgress(message = "Imputation workflow in progress...", value = 0, {
        incProgress(0.05, detail = "Preparing evaluation dataset...")
        
        # ================== 1) EVALUAR MODELO SELECCIONADO ==================
        df_base <- rv_imp$base_df
        if (is.null(df_base)) {
          df_base <- df
        }
        
        num_cols_eval <- names(df_base)[sapply(df_base, is.numeric)]
        validate(
          need(length(num_cols_eval) > 0,
               "No hay columnas numéricas en el dataset original para evaluar.")
        )
        
        df_eval   <- df_base[, num_cols_eval, drop = FALSE]
        pipeline  <- rv_imp$transform_pipeline %||% character(0)
        
        impute_fun_eval <- function(df_masked, ...) {
          # 2) aplicar pipeline
          tf <- apply_transform_pipeline(
            df       = df_masked,
            pipeline = pipeline
          )
          df_trans_masked <- tf$data
          meta_steps      <- tf$meta
          
          # 3) imputar en escala transformada
          df_imputed_trans <- NULL
          
          if (!is_mice) {
            df_imputed_trans <- switch(
              model,
              "mean_plain"   = impute_mean(df_trans_masked,   cols = names(df_trans_masked)),
              "median_plain" = impute_median(df_trans_masked, cols = names(df_trans_masked)),
              "linear_plain" = impute_regression(df_trans_masked),
              
              "knn_plain" = {
                k <- input$knn_neighbors_plain
                if (is.null(k) || !is.finite(k)) k <- 5
                impute_knn(df_trans_masked, cols = names(df_trans_masked), k = as.integer(k))
              },
              
              "ls_plain" = {
                tol <- input$ls_tolerance_plain
                if (is.null(tol) || !is.finite(tol)) tol <- 1e-4
                impute_ls(df_trans_masked, cols = names(df_trans_masked),
                          rank = 2, maxit = 20, tol = tol)
              },
              
              "bpca_plain" = {
                ncomp <- input$bpca_ncomp_plain
                if (is.null(ncomp) || !is.finite(ncomp)) ncomp <- 2
                impute_bpca(df_trans_masked, cols = names(df_trans_masked),
                            ncomp = as.integer(ncomp))
              },
              
              df_trans_masked
            )
          } else {
            m_val     <- input$mice_m     %||% 5L
            maxit_val <- input$mice_maxit %||% 10L
            
            knn_k_val <- input$knn_neighbors_mice %||% 5L
            bpca_nc   <- input$bpca_ncomp_mice    %||% 2L
            ls_tol_v  <- input$ls_tolerance_mice  %||% 1e-4
            
            res_mice <- run_mice_imputation(
              data          = df_trans_masked,
              model_code    = model,
              m             = as.integer(m_val),
              maxit         = as.integer(maxit_val),
              knn_neighbors = as.integer(knn_k_val),
              bpca_ncomp    = as.integer(bpca_nc),
              ls_tol        = as.numeric(ls_tol_v)
            )
            df_imputed_trans <- res_mice$data
          }
          
          # 4) invertir pipeline → volver a escala original
          invert_transform_pipeline(
            df_trans = df_imputed_trans,
            pipeline = pipeline,
            meta     = meta_steps
          )
          
        }
        
        incProgress(0.25, detail = "Running masked evaluation (cross-validation)...")
        
        eval_res <- NULL
        try({
          eval_res <- evaluate_imputer(
            df          = df_eval,
            impute_fun  = impute_fun_eval,
            model_name  = model,
            props       = c(0.1, 0.2),  # ajustable
            n_rep       = 2             # ajustable
          )
        }, silent = TRUE)
        
        rv_imp$eval_results <- eval_res
        
        if (!is.null(eval_res)) {
          message("=== Evaluation metrics (ORIGINAL scale) for model: ", model, " ===")
          print(eval_res)
        } else {
          message("No evaluation metrics produced (check errors).")
        }
        
        # ================== 2) IMPUTACIÓN REAL EN EL DATASET COMPLETO ==================
        # ================== 2) IMPUTACIÓN REAL EN EL DATASET COMPLETO ==================
        incProgress(0.45, detail = "Imputing full dataset...")
        
        # SIEMPRE imputamos en la ESCALA ACTUAL (df = active_df transformado)
        imputed_trans <- NULL
        
        if (!is_mice) {
          imputed_trans <- switch(
            model,
            "mean_plain"   = impute_mean(df,   cols = num_cols),
            "median_plain" = impute_median(df, cols = num_cols),
            "linear_plain" = impute_regression(df),
            
            "knn_plain" = {
              k <- input$knn_neighbors_plain
              if (is.null(k) || !is.finite(k)) k <- 5
              impute_knn(df, cols = num_cols, k = as.integer(k))
            },
            
            "ls_plain" = {
              tol <- input$ls_tolerance_plain
              if (is.null(tol) || !is.finite(tol)) tol <- 1e-4
              impute_ls(df, cols = num_cols, rank = 2, maxit = 20, tol = tol)
            },
            
            "bpca_plain" = {
              ncomp <- input$bpca_ncomp_plain
              if (is.null(ncomp) || !is.finite(ncomp)) ncomp <- 2
              impute_bpca(df, cols = num_cols, ncomp = as.integer(ncomp))
            },
            
            df
          )
          
          rv_imp$mice_diagnostics <- NULL
          
          showNotification(
            paste("Imputation finished using model:", model),
            type = "message"
          )
          
        } else {
          m_val     <- input$mice_m     %||% 5L
          maxit_val <- input$mice_maxit %||% 10L
          
          knn_k_val <- input$knn_neighbors_mice %||% 5L
          bpca_nc   <- input$bpca_ncomp_mice    %||% 2L
          ls_tol_v  <- input$ls_tolerance_mice  %||% 1e-4
          
          res <- run_mice_imputation(
            data          = df,
            model_code    = model,
            m             = as.integer(m_val),
            maxit         = as.integer(maxit_val),
            knn_neighbors = as.integer(knn_k_val),
            bpca_ncomp    = as.integer(bpca_nc),
            ls_tol        = as.numeric(ls_tol_v)
          )
          
          imputed_trans             <- res$data
          rv_imp$mice_diagnostics   <- res$diagnostics
          
          showNotification(
            paste("MICE-like multiple imputation finished using model:", model),
            type = "message"
          )
        }
        
        # ================== 2.b) DES-TRANSFORMAR A ESCALA ORIGINAL ==================
        pipeline <- rv_imp$transform_pipeline %||% character(0)
        
        if (length(pipeline) > 0) {
          # Reconstruimos parámetros del pipeline a partir del dataset base
          tp_res <- apply_transform_pipeline(
            df       = rv_imp$base_df %||% df,  # fallback por si acaso
            pipeline = pipeline
          )
          rv_imp$pipeline_meta <- tp_res$meta
          
          imputed_denorm <- invert_transform_pipeline(
            df_trans = imputed_trans,
            pipeline = pipeline,
            meta     = rv_imp$pipeline_meta
          )
        } else {
          # No hubo transformaciones
          rv_imp$pipeline_meta <- list()
          imputed_denorm      <- imputed_trans
        }
        
        imputed_denorm <- round_numeric_df(imputed_denorm, digits = 6)
        
        # Guardamos ambas versiones:
        rv_imp$current_imputed_df_scaled <- imputed_trans      # escala transformada
        rv_imp$current_imputed_df        <- imputed_denorm      # escala ORIGINAL
        
        # ================== 3) PREVIEW + PAYLOAD VISUALIZACIÓN ==================
        incProgress(0.25, detail = "Building preview and sending data to Visualization panel...")
        
        if (!is.null(rv_imp$current_imputed_df)) {
          # PREVIEW: SIEMPRE escala ORIGINAL
          output$imp_preview_data <- DT::renderDataTable({
            DT::datatable(
              rv_imp$current_imputed_df,
              options = list(scrollX = TRUE)
            )
          })
          
          # Payload global para el módulo de Visualización
          rv$viz_payload <- list(
            # Data original "base" (antes de transformaciones)
            base_df            = rv_imp$base_df,
            # Data transformada (la que llegó desde Transform)
            current_df         = rv_imp$current_df,
            # Versiones opcionales (Z / YJ)
            z_df               = rv_imp$z_df,
            yj_df              = rv_imp$yj_df,
            # Dataset imputado:
            imputed_df         = rv_imp$current_imputed_df,        # escala ORIGINAL
            imputed_df_scaled  = rv_imp$current_imputed_df_scaled, # escala transformada
            # Diagnósticos de MICE
            mice_diag          = rv_imp$mice_diagnostics,
            # Info del pipeline
            transform_pipeline = rv_imp$transform_pipeline,
            transform_meta     = rv_imp$pipeline_meta,
            # Tablita de utils.R (evaluate_imputer)
            eval_results       = rv_imp$eval_results
          )
          
          shinyjs::show(id = ns("final_viz_card_container"))
        }
        
      }) # fin withProgress
    })
    
    
    
    
    # ===== Botón "Go to Final Visualization" =====
    observeEvent(input$imp_go_viz, {
      # Necesitamos al menos el dataset imputado (escala ORIGINAL)
      validate(
        need(!is.null(rv_imp$current_imputed_df),
             "Run an imputation first so we can send the completed dataset to the visualization panel.")
      )
      
      # Construimos el payload para visualizaciones
      rv$viz_payload <- list(
        # Data original "base" (antes de transformaciones)
        base_df            = rv_imp$base_df,
        # Data actual transformada (la que usaste para imputar)
        current_df         = rv_imp$current_df,
        # Versiones opcionales (Z-score / Yeo-Johnson)
        z_df               = rv_imp$z_df,
        yj_df              = rv_imp$yj_df,
        # Dataset imputado final
        imputed_df         = rv_imp$current_imputed_df,        # ORIGINAL
        imputed_df_scaled  = rv_imp$current_imputed_df_scaled, # TRANSFORMADO
        # Diagnósticos de MICE (para gráficas de convergencia)
        mice_diag          = rv_imp$mice_diagnostics,
        # Pipeline + meta (para reconstruir si hace falta)
        transform_pipeline = rv_imp$transform_pipeline,
        transform_meta     = rv_imp$pipeline_meta,
        # Tablita de utils.R (evaluate_imputer)
        eval_results       = rv_imp$eval_results
      )
      
      showNotification(
        "Imputed and original datasets have been sent to the Visualization panel.",
        type = "message"
      )
      
      # Navegar al tab de Visualización (igual que ya lo tenías)
      session$onFlushed(function() {
        shinyjs::runjs("
          (function(){
            var navId   = 'main_nav';       // id del navset/navbar principal
            var value   = 'viz_tab';        // data-value del tab de Visualización
            var titleTx = 'Visualization';  // texto visible del tab (por si falla lo de data-value)

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
            if (clickEl(byVal)) return;

            // 2) por data-value dentro del navbar
            var byValScoped = document.querySelector('#'+navId+' [data-value=\"'+value+'\"]');
            if (clickEl(byValScoped)) return;

            // 3) por texto exacto
            var links = document.querySelectorAll('#'+navId+' .nav-link, #'+navId+' a.nav-link, #'+navId+' button.nav-link');
            for (var i=0;i<links.length;i++){
              if ((links[i].textContent||'').trim() === titleTx){
                if (clickEl(links[i])) return;
              }
            }

            // 4) por texto que contiene
            for (var j=0;j<links.length;j++){
              var txt = (links[j].textContent||'').replace(/\\s+/g,' ').trim();
              if (txt.indexOf(titleTx) !== -1){
                if (clickEl(links[j])) return;
              }
            }

            // 5) fallback: empujar input al servidor
            if (window.Shiny && Shiny.setInputValue){
              Shiny.setInputValue('main_nav', value, {priority:'event'});
            }
          })();
        ")
      }, once = TRUE)
    })
  })
}

    