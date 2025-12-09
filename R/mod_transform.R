# R/mod_transform.R
# UI: UX-friendly (using external CSS classes .tfm-*)
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(echarts4r)
library(e1071)

transform_ui <- function(id) {
  ns <- NS(id)
  
  # Helper for help icon (uses CSS .help-icon in styles.css)
  help_q <- function(text) {
    tags$span("?", title = text, class = "help-icon")
  }
  
  bslib::page_sidebar(
    theme = bslib::bs_theme(version = 5),
    fillable = FALSE,
    
    # ===== SIDEBAR =====
    sidebar = bslib::sidebar(
      title = "Transformation",
      width = 300,
      collapsible = TRUE,
      open = "desktop",
      class = "tfm-sidebar",
      
      # CONTENIDO CON SCROLL
      div(class = "tfm-scroll",
          
          # Informational message
          div(
            class = "alert alert-info",
            tags$strong("Auto transform: a guide, not an absolute truth."),
            tags$p(
              "Applies the suggested sequence (Log → Z-score → Yeo-Johnson). ",
              "Use it as a starting point and always validate results visually and statistically."
            )
          ),
          
          # Global actions
          div(class = "d-grid gap-2 mb-2",
              div(
                class = "d-flex align-items-center justify-content-between",
                div(style = "flex:1;",
                    actionButton(ns("tfm_auto_btn"), "Auto transform", class = "btn btn-primary btn-lg w-100")
                ),
                help_q("Runs: (1) Log with adaptive offset, (2) Z-score, (3) Yeo-Johnson (bestNormalize).")
              ),
              div(
                class = "d-flex align-items-center justify-content-between",
                div(style = "flex:1;",
                    actionButton(ns("tfm_reset_btn"), "Reset to original", class = "btn btn-outline-secondary w-100")
                ),
                help_q("Discard all transformations and restore the original processed dataset.")
              )
          ),
          
          div(class = "tfm-divider"),
          
          # Step 1: Log
          div(class = "tfm-step",
              h5("1) Logarithmic"),
              p("Compresses right tails and stabilizes variance when data are positively skewed."),
              div(
                class = "d-flex align-items-center justify-content-between",
                div(style = "flex:1;",
                    actionButton(ns("tfm_log_apply"), "Apply log10", class = "btn btn-secondary w-100")
                ),
                help_q("Applies log10(x + offset) with an automatically calculated offset to handle zero or negative values.")
              ),
              div(class = "tfm-muted", "Offset is determined automatically to avoid non-positive values.")
          ),
          
          # Step 2: Z-score
          div(class = "tfm-step",
              h5("2) Normalization (z-score)"),
              p("Standardizes each column as (x − mean) / standard deviation."),
              div(
                class = "d-flex align-items-center justify-content-between",
                div(style = "flex:1;",
                    actionButton(ns("tfm_norm_apply"), "Apply z-score", class = "btn btn-secondary w-100")
                ),
                help_q("Centers and scales numeric variables automatically so they are on comparable scales.")
              )
          ),
          
          # Step 3: Yeo-Johnson
          div(class = "tfm-step",
              h5("3) Yeo-Johnson"),
              p("Power transformation to reduce residual skewness; works with zeros and negatives."),
              div(
                class = "d-flex align-items-center justify-content-between",
                div(style = "flex:1;",
                    actionButton(ns("tfm_yj_apply"), "Apply Yeo-Johnson", class = "btn btn-secondary w-100")
                ),
                help_q("Uses bestNormalize::yeojohnson to estimate λ per column, improving normality and stabilizing variance.")
              )
          ),
          
          # Hidden inputs (compatible with the server)
          shinyjs::hidden({
            numericInput(ns("tfm_log_offset"),  label = NULL, value = 1e-6, min = 0)
            checkboxInput(ns("tfm_norm_center"), label = NULL, value = TRUE)
            checkboxInput(ns("tfm_norm_scale"),  label = NULL, value = TRUE)
            checkboxInput(ns("tfm_yj_auto"),     label = NULL, value = TRUE)
            numericInput(ns("tfm_yj_lambda"),    label = NULL, value = 0, step = 0.1)
          })
      )
    ),
    
    # ===== MAIN CONTENT =====
    shiny::tabsetPanel(
      id = ns("tfm_tabs"),
      type = "tabs",
      
      # ==== BEFORE vs AFTER (always 2 plots: all data) ====
      shiny::tabPanel(
        title = "Before vs After",
        # Controls
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bslib::card(
              class = "mb-3",
              bslib::card_header("Graph type"),
              bslib::card_body(
                div(
                  class = "plot-type-pills",
                  shiny::radioButtons(
                    inputId = ns("plot_type"),
                    label   = NULL,
                    choices = c("Histogram", "Density", "Boxplot", "QQ-plot (full dataset)"),
                    selected = "Histogram",
                    inline   = TRUE
                  )
                ),
                shiny::helpText("Tip: Change the graph type to compare distributions and assumptions.")
              )
            )
          )
        ),
        # Two plots: BEFORE / AFTER (all data)
        shiny::fluidRow(
          shiny::column(
            width = 6,
            bslib::card(
              bslib::card_header("Before (All data)"),
              echarts4r::echarts4rOutput(ns("tfm_ba_plot_before"), height = "420px")
            )
          ),
          shiny::column(
            width = 6,
            bslib::card(
              bslib::card_header("After (All data)"),
              echarts4r::echarts4rOutput(ns("tfm_ba_plot_after"), height = "420px")
            )
          )
        ),
        # Comparison Z-score vs Yeo–Johnson (only if applied together)
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bslib::card(
              bslib::card_header("Z-score vs Yeo–Johnson (applied in the same operation)"),
              echarts4r::echarts4rOutput(ns("tfm_z_vs_yj_plot"), height = "360px")
            )
          )
        )
      ),
      
      # ==== SUMMARY (AFTER) ====
      shiny::tabPanel(
        title = "Summary (after)",
        
        # --- CTA banner to go to Explore & Selection ---
        shiny::fluidRow(
          shiny::column(
            width = 12,
            div(
              class = "alert alert-info d-flex align-items-center justify-content-between",
              div(
                tags$strong("Tip:"),
                " After finishing your transformations, jump to the ",
                tags$em("Imputation"),
                " tab to run missing-data imputation."
              ),
              actionButton(
                ns("go_impute_btn"),
                label = "Go to Imputation",
                class = "btn btn-outline-primary"
              )
            )
          )
        ),
        
        # Card 1: Summary stats (transposed)
        bslib::card(
          class = "tfm-summary-card",
          bslib::card_header("Summary statistics of transformed data"),
          div(
            class = "tfm-summary-wrap",
            DT::dataTableOutput(ns("tfm_summary_after_tbl"), width = "100%")
          )
        ),
        
        shiny::br(),
        
        # Card 2: Shapiro by Protein
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bslib::card(
              bslib::card_header("Shapiro-Wilk Normality Test by Protein"),
              DT::dataTableOutput(ns("tfm_shapiro_tbl"), width = "100%")
            )
          )
        ),
        
        shiny::br(),
        
        # Card 3: Global Summary Before vs After
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bslib::card(
              bslib::card_header("Global Summary Before vs After (full dataset)"),
              DT::dataTableOutput(ns("tfm_summary_var_tbl"), width = "100%")
            )
          )
        )
      )
    )
  )
}
