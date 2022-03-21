#' ZI_Test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param smpfiles,smps global.R variables
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZI_Test_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  verbatimTextOutput(ns("exemple")),
  verbatimTextOutput(ns("texte"))
  
  )
}
    
#' ZI_Test Server Functions
#'
#' @noRd 
mod_ZI_Test_server <- function(id, test){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$exemple <- renderPrint({
      "Print is working"
    })
    
    output$texte <- renderPrint({
      test$data_folder_path_rea
    })
    
  })
}
    
## To be copied in the UI
# mod_ZI_Test_ui("ZI_Test_ui_1")
    
## To be copied in the server
# mod_ZI_Test_server("ZI_Test_ui_1")
