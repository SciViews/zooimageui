#' page_training_sets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_training_sets_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      
# Training Set Preparation ------------------------------------------------
      
      tabPanel("Training Set Preparation",
        tags$br(),
        sidebarLayout(
          
          # Création des training sets
          sidebarPanel(
            tags$h4("Preparation"),
            textInput(ns("tsprep_name"), "Name of the new Training Set"),
            checkboxGroupInput(ns("tsprep_zidbs"), label = "" , choices = NULL),
            selectInput(ns("tsprep_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            actionButton(ns("tsprep_prepare"), "Prepare Training Set"),
          ),
          
          # Affichage des training sets existants
          mainPanel(
            tags$h4("Existing Training Sets"),
            verbatimTextOutput(ns("tsprep_existing_show")),
            selectInput(ns("tsprep_folder_select"), "Train Set to visualize", choices = NULL ),
            verbatimTextOutput(ns("tsprep_folder_content")),
          )
          
        )
      )
      
    )
  )
}
    
#' page_training_sets Server Functions
#'
#' @noRd 
mod_page_training_sets_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
    # Récupération Des Variables ----------------------------------------------
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    
    observe({
      updateCheckboxGroupInput(session, "tsprep_zidbs", label = "Select samples :", choices = zidb_files())
    })
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
