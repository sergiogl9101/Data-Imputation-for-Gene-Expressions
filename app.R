datatables_html <- NULL

library(shiny)
library(bslib)
library(DT)
library(shinyjs)

options(shiny.maxRequestSize = 200 * 1024^2)

# --- UI modules ---
source("R/mod_upload.R",    local = TRUE, encoding = "UTF-8")
source("R/mod_transform.R", local = TRUE, encoding = "UTF-8")
source("R/mod_impute.R",    local = TRUE, encoding = "UTF-8")
source("R/mod_viz.R",       local = TRUE, encoding = "UTF-8")

# --- Helpers/Bindings para Statistical Summary ---
if (file.exists("server/stat_summary_bindings.R")) {
  source("server/stat_summary_bindings.R", local = TRUE, encoding = "UTF-8")
}
# --- Helpers/Bindings para Resume by Protein ---
if (file.exists("server/stat_by_protein_bindings.R")) {
  source("server/stat_by_protein_bindings.R", local = TRUE, encoding = "UTF-8")
}

# --- Server modules ---
source("server/mod_upload_server.R",    local = TRUE, encoding = "UTF-8")
if (file.exists("server/mod_transform_server.R")) {
  source("server/mod_transform_server.R", local = TRUE, encoding = "UTF-8")
}
if (file.exists("server/mod_impute_server.R")) {
  source("server/mod_impute_server.R",   local = TRUE, encoding = "UTF-8")
}
if (file.exists("server/mod_viz_server.R")) {                     
  source("server/mod_viz_server.R", local = TRUE, encoding = "UTF-8")
}

ui <- bslib::page_navbar(
  theme = bs_theme(version = 5),
  id = "main_nav",
  
  
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('show_final_viz_card', function(message) {
      document.getElementById(message.id).style.display = 'block';
    });
  ")),
    div(
      style = "text-align:center; margin:10px 0;",
      tags$h1(
        "Data Imputation For Gene Expression",
        style = "font-size:3em;font-weight:bold;color:#333;text-align:center;"
      )
    )
  ),
  
  bslib::nav_panel(
    "Upload & Profiling",
    div(style = "margin-bottom:30px; font-size:1.5em;", upload_ui("upload"))
  ),
  
  bslib::nav_panel(
    title = "Transformation",
    value = "transform_tab",
    div(style = "margin-bottom:30px; font-size:1.5em;", transform_ui("transform"))
  ),
  
  bslib::nav_panel(
    title = "Imputation",
    value = "imputation_tab",
    div(style = "margin-bottom:30px; font-size:1.5em;", impute_ui("impute"))
  ),
  
  # <<< NEW: pestaña final de visualización
  bslib::nav_panel(
    title = "Visualization",
    value = "viz_tab",
    div(style = "margin-bottom:30px; font-size:1.5em;", viz_ui("viz"))
  )
)


server <- function(input, output, session) {
  rv <<- reactiveValues(
    base_df        = NULL,
    current_df     = NULL,
    z_df           = NULL,
    yj_df          = NULL,
    impute_payload = NULL,
    viz_payload    = NULL
  )
  
  if (exists("upload_server"))    upload_server("upload")
  if (exists("transform_server")) transform_server("transform")
  if (exists("impute_server"))    impute_server("impute")
  if (exists("viz_server"))       viz_server("viz")
  
  # Helper de navegación
  nav_to <- function(value_or_title) {
    try(
      bslib::nav_select(
        session$rootScope(),
        inputId = "main_nav",
        selected = value_or_title
      ),
      silent = TRUE
    )
  }
  
  options(APP_NAV_TO = nav_to)
}

shinyApp(ui, server)
