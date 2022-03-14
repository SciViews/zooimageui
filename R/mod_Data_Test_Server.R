#' Data_Test_Server UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data_folder_path global.R variable
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_Test_Server_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Table of titanic from ~/shared/data/titanic_data.csv"),
    dataTableOutput(ns("titanic_data_table")),
    verbatimTextOutput(ns("path"))
  )
}
    
#' Data_Test_Server Server Functions
#'
#' @noRd 
mod_Data_Test_Server_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # 
    # data_folder_path <- golem::get_golem_options("data_folder_path")
    
    tita_data <- read.csv("~/shared/data/titanic_data.csv")
    
    output$titanic_data_table <- renderDataTable({
      tita_data
    })
    
    output$path <- renderPrint({
      # vient du global.R
      data_folder_path
    })
    
  })
}
    
## To be copied in the UI
# mod_Data_Test_Server_ui("Data_Test_Server_ui_1")
    
## To be copied in the server
# mod_Data_Test_Server_server("Data_Test_Server_ui_1")
