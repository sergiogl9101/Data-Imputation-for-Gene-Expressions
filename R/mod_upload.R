# R/mod_upload.R
# UI module with collapsible sidebar (module-safe via layout_sidebar)
library(shiny)
library(bslib)
library(DT)
library(shinyjs)
library(echarts4r)

upload_ui <- function(id) {
  ns <- NS(id)
  
  # Necesario para que toggleState/enable/disable funcionen dentro del módulo
  shinyjs::useShinyjs()
  
  bslib::layout_sidebar(
    # --- SIDEBAR ---
    sidebar = sidebar(
      width = 300,
      collapsible = TRUE,
      open = "desktop",
      
      h3("Upload & Profiling"),
      uiOutput(ns("howto_btn_ui")),
      helpText("Upload a CSV file. This section is UI only; processing will be connected later."),
      
      fileInput(
        inputId = ns("upload_file"),
        label   = "File (CSV/TXT)",
        accept  = c(".csv", ".txt")
      ),
      
      # Transpose & Set Headers — enabled once a file is loaded
      fluidRow(
        column(
          width = 6,
          actionButton(
            inputId = ns("transpose_btn"),
            label   = "Transpose Dataset",
            width   = "100%"
          )
        ),
        column(
          width = 6,
          actionButton(
            inputId = ns("set_headers_btn"),
            label   = "Set Headers",
            width   = "100%"
          )
        )
      ),
      
      # Botón de descarga (se muestra hasta que haya CSV procesado)
      div(
        style = "margin-top:10px;",
        shinyjs::hidden(
          downloadButton(ns("download_processed"), label = "Download processed CSV", class = "btn btn-success w-100")
        )
      ),
      
      helpText("After profiling, check the preview, missing data, and distributions.")
    ),
    
    # --- MAIN CONTENT ---
    tabsetPanel(
      id   = ns("upload_tabs"),
      type = "tabs",
      
      tabPanel(
        title = "Preview",
        value = "preview",  # ← estable
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Dataset Preview"),
              DTOutput(ns("upload_preview_tbl")),
              helpText("Note: Ensure that proteins are set as columns."),
              fluidRow(
                column(
                  width = 12,
                  actionButton(
                    inputId = ns("upload_profile_btn"),
                    label   = "Quick Profiling",
                    class   = "btn btn-primary w-100"
                  )
                )
              )
            )
          )
        )
      ),
      
      tabPanel(
        title = "Statistical Summary",
        value = "stats",    # ← estable (usaremos este en server)
        div(class = "stat-summary-container wide",
            
            bslib::card(
              class = "stat-card",
              bslib::card_header("Percentage of Missing Data"),
              div(class = "stat-card-body",
                  echarts4r::echarts4rOutput(ns("upload_missing_plot"), height = 480)
              )
            ),
            
            bslib::card(
              class = "stat-card",
              bslib::card_header("Data Distribution (Unnormalized)"),
              div(class = "stat-card-body",
                  echarts4r::echarts4rOutput(ns("upload_dist_plot"), height = 480)
              )
            ),
            
            bslib::card(
              class = "stat-card",
              bslib::card_header("Boxplot"),
              div(class = "stat-card-body",
                  echarts4r::echarts4rOutput(ns("upload_boxplot"), height = 480)
              )
            ),
            
            bslib::card(
              class = "stat-card",
              bslib::card_header("Heatmap"),
              div(class = "stat-card-body",
                  echarts4r::echarts4rOutput(ns("upload_heatmap"), height = 480)
              )
            )
        )
      ),
      
      # --- Resume by Protein ---
      tabPanel(
        title = "Resume by Protein",
        value = "resume_by_protein",
        
        # Banner con CTA para ir a Transformación
        fluidRow(
          column(
            width = 12,
            div(class = "alert alert-info d-flex align-items-center justify-content-between",
                div(
                  tags$strong("Tip:"),
                  " Once you explore a protein, jump to the ",
                  tags$em("Transformation"),
                  " tab to normalize or power-transform your data."
                ),
                actionButton(ns("go_transform_btn"),
                             label = "Go to Transform",
                             class = "btn btn-outline-primary")
            )
          )
        ),
        
        # Selector row
        fluidRow(
          column(
            width = 6,
            wellPanel(
              h4("Protein selection"),
              selectInput(ns("protein_select"),
                          label   = "Primary protein:",
                          choices = NULL
              )
            )
          ),
          column(
            width = 6,
            wellPanel(
              h4("Comparison for scatter"),
              selectInput(ns("protein_compare_select"),
                          label   = "Compare with:",
                          choices = NULL
              )
            )
          )
        ),
        
        # 3 cards (pie, histogram, boxplot) for the primary protein
        fluidRow(
          column(
            width = 4,
            wellPanel(
              h4("Percentage of Missing Data by Protein"),
              echarts4r::echarts4rOutput(ns("missing_by_protein_plot"), height = "260px")
            )
          ),
          column(
            width = 4,
            wellPanel(
              h4("Data Distribution (Unnormalized) by Protein"),
              echarts4r::echarts4rOutput(ns("dist_by_protein_plot"), height = "260px")
            )
          ),
          column(
            width = 4,
            wellPanel(
              h4("Boxplot by Protein"),
              echarts4r::echarts4rOutput(ns("boxplot_by_protein"), height = "260px")
            )
          )
        ),
        
        # scatter
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Search and Scatter Plot"),
              echarts4r::echarts4rOutput(ns("scatter_plot"), height = "380px"),
              helpText("Disclaimer: Results may not be visually ideal since no transformations have been applied yet.")
            )
          )
        )
      )
    )
  )
}
