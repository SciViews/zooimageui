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
             textOutput(ns("data_folder_path")),
             tags$hr(),
             tags$h4("-> Samples"),
             selectInput(ns("zidb_show"), NULL, choices = NULL),
             tags$p("- 2"),
             tags$hr(),
             tags$h4("-> Training Sets"),
             tags$p("- 1"),
             tags$p("- 2"),
             tags$hr(),
             tags$h4("-> Models"),
             tags$p("- 1"),
             tags$p("- 2"),
             tags$hr(),
             tags$h4("-> Results"),
             tags$p("- 1"),
             tags$p("- 2"),
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
    
    # Samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    
    # Settings ----------------------------------------------------------------
    
    output$data_folder_path <- renderText({ data_folder_path() })
    
    # Samples -----------------------------------------------------------------
    
    observeEvent(zidb_files(), {
      if (length(zidb_files()) > 0) {
        updateSelectInput(session, "zidb_show", NULL, choices = zidb_files())
      } else {
        updateSelectInput(session, "zidb_show", NULL, choices = "No ZIDB file yet")
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
