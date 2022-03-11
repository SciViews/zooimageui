#' ZI_Test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZI_Test_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  textOutput(ns("test"))
  )
}
    
#' ZI_Test Server Functions
#'
#' @noRd 
mod_ZI_Test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$test <- renderText({
      # J'avais testé de prendre data_folder_path après l'avoir définit dans ZooImageUI-package.R, mais on dirait que ce n'est pas la bonne idée
      "data_folder_path"
    })
  })
}
    
## To be copied in the UI
# mod_ZI_Test_ui("ZI_Test_ui_1")
    
## To be copied in the server
# mod_ZI_Test_server("ZI_Test_ui_1")
