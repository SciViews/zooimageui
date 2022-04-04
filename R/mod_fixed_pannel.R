#' fixed_pannel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fixed_pannel_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "fixed_pannel_div",
             tags$h4("Global Informations", id = "fixed_pannel_title"),
             
             tags$hr(),
             tags$h4("-> Global Settings"),
             tags$h5("Data folder path :"),
             textOutput(ns("data_folder_path")),
             
             tags$hr(),
             tags$h4("-> Samples"),
             tags$h5("Samples folder path :"),
             textOutput(ns("Samples_folder_path")),
             tags$br(),
             selectInput(ns("zidb_show"), NULL, choices = NULL),
             textOutput(ns("zidb_show_nrow")),
             
             tags$hr(),
             tags$h4("-> Training Sets"),
             tags$p("- Empty 1"),
             tags$p("- Empty 2"),
             
             tags$hr(),
             tags$h4("-> Models"),
             tags$p("- Empty 1"),
             tags$p("- Empty 2"),
             
             tags$hr(),
             tags$h4("-> Results"),
             tags$p("- Empty 1"),
             tags$p("- Empty 2"),
    )
  )
}

#' fixed_pannel Server Functions
#'
#' @noRd 
mod_fixed_pannel_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Récupération Des Variables ----------------------------------------------
    
    # settings vars
    data_folder_path <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path})
    
    # Samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    zidb_df_nrow <- reactive({ all_vars$samples_vars$zidb_df_nrow })
    
    # Settings ----------------------------------------------------------------
    
    # Affichage // data_folder_path
    output$data_folder_path <- renderText({ data_folder_path() })
    # Affichage // Samples_folder_path
    output$Samples_folder_path <- renderText({ Samples_folder_path() })
    
    # Samples -----------------------------------------------------------------
    
    # Mise à jour du choix du ZIDB à montrer dans samples
    observe({
      if (length(zidb_files()) > 0) {
        updateSelectInput(session, "zidb_show", NULL, choices = zidb_files())
      } else {
        updateSelectInput(session, "zidb_show", NULL, choices = "No ZIDB file yet")
      }
    })
    
    # Affichage // Nombre de lignes dans le ZIDB choisi
    output$zidb_show_nrow <- renderText({
      if (length(zidb_files()) > 0) {
        paste0(zidb_df_nrow(), " rows in the sample")
      } else {
        "No ZIDB file yet"
      }
    })
    
    # Communication -----------------------------------------------------------
    
    to_Samples_vars <- reactiveValues(
      zidb_show = NULL,
    )
    
    observe({
      to_Samples_vars$zidb_show <- if (req(input$zidb_show) != "No ZIDB file yet") {
        input$zidb_show
      }
    })
    
    return(to_Samples_vars)
    
  })
}

## To be copied in the UI
# mod_fixed_pannel_ui("fixed_pannel_ui_1")

## To be copied in the server
# mod_fixed_pannel_server("fixed_pannel_ui_1")
