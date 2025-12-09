# R/mod_explore.R
# Módulo: Explorar & Selección (solo UI)

explore_ui <- function(id) {
  ns <- NS(id)
  
  sidebar <- shiny::sidebarPanel(
    width = 3,
    shiny::h3("Explorar & Selección"),
    shiny::helpText("Explora variables, visualiza distribuciones y preview de heatmap (solo UI)."),
    shiny::checkboxInput(
      inputId = ns("exp_transpose"),
      label   = "Transponer matriz",
      value   = FALSE
    ),
    shiny::h4("Selector de variables"),
    shiny::uiOutput(ns("exp_var_selector_ui")),
    shiny::uiOutput(ns("exp_target_var_ui")),
    shiny::actionButton(
      inputId = ns("exp_refresh_btn"),
      label   = "Actualizar vista"
    ),
    shiny::helpText("Los selectores y vistas se poblarán desde el server.")
  )
  
  main <- shiny::mainPanel(
    width = 9,
    shiny::tabsetPanel(
      id = ns("exp_tabs"),
      type = "tabs",
      
      shiny::tabPanel(
        title = "Distribución variable seleccionada",
        shiny::h4("Distribución"),
        shiny::plotOutput(ns("exp_target_var_plot"), height = "360px"),
        shiny::h4("Estadísticos"),
        shiny::verbatimTextOutput(ns("exp_target_var_stats"))
      ),
      
      shiny::tabPanel(
        title = "Matriz de calor (preview)",
        shiny::h4("Vista previa de heatmap"),
        shiny::plotOutput(ns("exp_heatmap_preview"), height = "420px")
      ),
      
      shiny::tabPanel(
        title = "Metadatos",
        shiny::h4("Información del dataset / variables"),
        shiny::verbatimTextOutput(ns("exp_meta_info"))
      )
    )
  )
  
  shiny::sidebarLayout(sidebar, main)
}
