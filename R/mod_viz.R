# R/mod_viz.R
# UI: Visualization panel

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(echarts4r)

viz_ui <- function(id) {
  ns <- NS(id)
  
  # Helper para ícono de ayuda (reusa la clase .help-icon de styles.css)
  help_q <- function(text) {
    tags$span("?", title = text, class = "help-icon")
  }
  
  # Helper para métricas
  metric_card <- function(title, label, desc) {
    div(
      class = "p-3 mb-3 bg-light rounded-3 shadow-sm h-100",
      tags$div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$span(title, class = "fw-bold"),
        tags$span(label, class = "badge bg-primary")
      ),
      tags$p(class = "mb-0 small text-muted", desc)
    )
  }
  
  bslib::page_sidebar(
    theme    = bslib::bs_theme(version = 5),
    fillable = FALSE,
    
    # ===== SIDEBAR =====
    sidebar = bslib::sidebar(
      title       = "Visualization",
      width       = 300,
      collapsible = TRUE,
      open        = "desktop",
      class       = "viz-sidebar",
      
      div(
        class = "viz-scroll",
        
        div(
          class = "alert alert-info",
          tags$strong("What you will see here"),
          tags$p(
            "1) Evaluation metrics and diagnostic plots comparing the transformed dataset ",
            "without imputation vs the final imputed dataset (denormalized)."
          ),
          tags$p(
            "2) A heatmap summarizing structure or differences, and ",
            "the final imputed dataset ready to download."
          )
        ),
        
        tags$hr()
      )
    ),
    
    # ===== MAIN CONTENT =====
    tabsetPanel(
      id   = ns("viz_tabs"),
      type = "tabs",
      
      # ------------------------------------------------------------------
      # TAB 1: EVALUATION & DIAGNOSTICS
      # ------------------------------------------------------------------
      tabPanel(
        title = "Evaluation & Diagnostics",
        
        # ---- Métricas de evaluación ----
        fluidRow(
          column(
            width = 12,
            bslib::card(
              class = "mb-3",
              bslib::card_header("Evaluation metrics (masked imputation tests)"),
              bslib::card_body(
                p(
                  "These metrics come from the masked-evaluation procedure (e.g. ",
                  "artificially hiding values, imputing them and comparing against the truth ",
                  "in the original scale)."
                ),
                # Tabla de métricas (MSE, RMSE, MAE, R², prop, rep, model, etc.)
                DT::dataTableOutput(ns("viz_eval_tbl"), width = "100%"),
                
                tags$hr(),
                
                # ======= Guía en un solo renglón por métrica, dentro de accordion =======
                bslib::accordion(
                  id = ns("viz_metric_help"),
                  bslib::accordion_panel(
                    title = "Quick guide to the main metrics",
                    value = "metrics",
                    div(
                      class = "small",
                      tags$ul(
                        class = "list-unstyled mb-0",
                        
                        tags$li(
                          tags$strong("MSE"), " – ",
                          tags$em("Mean Squared Error"), ": ",
                          "Average of the squared differences between true and imputed values. ",
                          "Penalizes large errors more strongly. Lower is better."
                        ),
                        
                        tags$li(
                          tags$strong("RMSE"), " – ",
                          tags$em("Root MSE"), ": ",
                          "Square root of MSE, in the same units as the data. ",
                          "Helps interpret the typical size of imputation errors."
                        ),
                        
                        tags$li(
                          tags$strong("MAE"), " – ",
                          tags$em("Mean Absolute Error"), ": ",
                          "Average of absolute differences between true and imputed values. ",
                          "More robust to outliers than MSE/RMSE. Lower is better."
                        ),
                        
                        tags$li(
                          HTML("<strong>R²</strong> – "),
                          tags$em("Coefficient of determination"), ": ",
                          "Measures how well the imputed values reconstruct the masked true values during validation.",
                          "Values close to 1 indicate strong agreement between real and imputed data;",
                          "values near 0 indicate poor reconstruction."
                        ),
                        
                        tags$li(
                          tags$strong("prop"), " – ",
                          tags$em("Masked fraction"), ": ",
                          "Proportion of values that were artificially hidden in each evaluation scenario ",
                          "to test the imputation model."
                        ),
                        
                        tags$li(
                          tags$strong("rep"), " – ",
                          tags$em("Repetition"), ": ",
                          "Index of the repetition in the masking–imputation–evaluation loop. ",
                          "Helps assess stability across runs."
                        ),
                        
                        tags$li(
                          tags$strong("model"), " – ",
                          tags$em("Model code"), ": ",
                          "Code/name of the imputation model used in that evaluation run ",
                          "(e.g., mean_plain, knn_mice, bpca_plain, etc.)."
                        )
                      )
                    )
                  )
                )
                # ======= FIN CAMBIO =======
              )
            )
          )
        ),
        
        # ---- Controles para tipo de gráfica ----
        fluidRow(
          column(
            width = 12,
            bslib::card(
              class = "mb-3",
              bslib::card_header("Graph type for distribution comparison"),
              bslib::card_body(
                div(
                  class = "plot-type-pills",
                  radioButtons(
                    inputId  = ns("viz_plot_type"),
                    label    = NULL,
                    choices  = c(
                      "Histogram",
                      "Density",
                      "Boxplot",
                      "QQ-plot"
                    ),
                    selected = "Histogram",
                    inline   = TRUE
                  )
                ),
                
                helpText(
                  "These plots compare the distribution of ",
                  "all numeric values in the transformed dataset WITHOUT imputation ",
                  "vs the FINAL IMPUTED dataset in the original (denormalized) scale."
                )
              )
            )
            
          )
        ),
        
        # ---- Gráficas comparativas: Transformado vs Imputado ----
        fluidRow(
          column(
            width = 6,
            bslib::card(
              bslib::card_header("Transformed dataset (no imputation)"),
              echarts4rOutput(ns("viz_plot_transformed_untransformed"), height = "420px")
            )
          ),
          column(
            width = 6,
            bslib::card(
              bslib::card_header("Imputed dataset"),
              echarts4rOutput(ns("viz_plot_imputed_untransformed"), height = "420px")
            )
          )
        ),
        
        br(),
        
        # ---- Heatmaps ----
        fluidRow(
          column(
            width = 6,
            bslib::card(
              bslib::card_header(
                div(
                  "Heatmap – original dataset (no imputation / no transformation)",
                  help_q(
                    "Numeric matrix coming directly from Quick Profiling (before transformations and imputation)."
                  )
                )
              ),
              bslib::card_body(
                echarts4rOutput(ns("viz_heatmap_original"), height = "380px")
              )
            )
          ),
          column(
            width = 6,
            bslib::card(
              bslib::card_header(
                div(
                  "Heatmap – final imputed dataset (denormalized)",
                  help_q(
                    "Same rows/IDs but after running the full transform + imputation pipeline and inverting transformations."
                  )
                )
              ),
              bslib::card_body(
                echarts4rOutput(ns("viz_heatmap_imputed"), height = "380px")
              )
            )
          )
        )
        
      ),
      
      # ------------------------------------------------------------------
      # TAB 2: FINAL DATASET & EXPORT
      # ------------------------------------------------------------------
      tabPanel(
        title = "Final dataset & Export",
        
        fluidRow(
          column(
            width = 12,
            bslib::card(
              class = "mb-3",
              bslib::card_header(
                div(
                  class = "d-flex align-items-center justify-content-between",
                  div(
                    tags$strong("Final imputed dataset (original scale)"),
                    tags$p(
                      class = "mb-0",
                      "This table shows the fully imputed dataset after inverting ",
                      "all transformations"
                    )
                  ),
                  # Botón de descarga
                  downloadButton(
                    outputId = ns("viz_download_imputed"),
                    label    = "Download full dataset (.csv)",
                    class    = "btn btn-primary"
                  )
                )
              ),
              bslib::card_body(
                DT::dataTableOutput(ns("viz_imputed_tbl"), width = "100%")
              )
            )
          )
        )
      )
    )
  )
}
