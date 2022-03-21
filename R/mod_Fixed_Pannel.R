#' Fixed_Pannel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Fixed_Pannel_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "fixed_pannel_div",
      tags$h4("Global Informations", id = "fixed_pannel_title"),
      tags$hr(),
      tags$h4("-> Global Settings"),
      textOutput(ns("data_folder_path")),
      tags$hr(),
      tags$h4("-> Samples"),
      tags$p("- 1"),
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
    
#' Fixed_Pannel Server Functions
#'
#' @noRd 
mod_Fixed_Pannel_server <- function(id, settings_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_folder_path <- reactive ({ settings_vars$data_folder_path_rea })
    
    output$data_folder_path <- renderText({ data_folder_path() })
  })
}
    
## To be copied in the UI
# mod_Fixed_Pannel_ui("Fixed_Pannel_ui_1")
    
## To be copied in the server
# mod_Fixed_Pannel_server("Fixed_Pannel_ui_1")
