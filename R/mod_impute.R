# R/mod_impute.R
# UI: Imputation panel (updated, sin selección de dataset/columnas)

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(echarts4r)

# Cargar modelos
source("Models/model_mean.R", local = TRUE)
source("Models/model_median.R", local = TRUE)
source("Models/model_regression.R", local = TRUE)
source("Models/model_knn.R",       local = TRUE)
source("Models/model_ls.R",        local = TRUE)
source("Models/model_bpca.R",      local = TRUE)
source("Models/model_mice.R",      local = TRUE)


impute_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      
      # ============================ SIDEBAR ============================
      sidebarPanel(
        width = 4,
        h4("Imputation Models Selection"),
        
        selectInput(
          ns("imp_model_category"),
          label = "Select Model Category",
          choices = c(
            "Statistical (Single Imputation)",
            "ML (Single Imputation)",
            "MICE (Multiple Imputation)"
          ),
          selected = "Statistical (Single Imputation)"
        ),
        
        selectInput(
          ns("imp_model_list"),
          label = "Select Model",
          choices = NULL
        )
      ),
      
      # ============================ MAIN PANEL ============================
      mainPanel(
        width = 8,
        
        # --- Recommended Method ---
        bslib::card(
          bslib::card_header("Recommend Method"),
          bslib::card_body(
            p(
              strong("Recommended Method: "),
              textOutput(ns("imp_recommended_method"), inline = TRUE)
            ),
            p(
              strong("Reason: "),
              textOutput(ns("imp_recommendation_reason"), inline = TRUE)
            ),
            actionButton(
              ns("imp_run_recommend_method"),
              "Recommend Method",
              class = "btn btn-info"
            )
          )
        ),
        
        # --- Hyperparameters (dinámico) ---
        bslib::card(
          bslib::card_header("Set Hyperparameters"),
          bslib::card_body(
            uiOutput(ns("imp_model_config"))
          )
        ),
        
        # --- Dataset Preview ---
        bslib::card(
          bslib::card_header("Imputed Dataset Preview"),
          bslib::card_body(
            DT::dataTableOutput(ns("imp_preview_data"), width = "100%")
          )
        ),
        
        # --- FINAL VISUALIZATION CARD (OCULTO AL INICIO POR SHINYJS) ---
        div(
          id = ns("final_viz_card_container"),
          # sin style = "display:none;",
          
          bslib::card(
            bslib::card_header("Final Visualization"),
            bslib::card_body(
              p("Your imputed dataset is ready! You can now inspect all datasets in the Final Visualization panel."),
              actionButton(
                ns("imp_go_viz"),
                "Go to Final Visualization",
                class = "btn btn-primary btn-lg"
              )
            )
          )
        )
        
        
      ) # cierre mainPanel
    )   # cierre sidebarLayout
  )     # cierre fluidPage
}       # cierre función
