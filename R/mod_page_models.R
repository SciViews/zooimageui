#' page_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_models_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' page_models Server Functions
#'
#' @noRd 
mod_page_models_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_models_ui("page_models_ui_1")
    
## To be copied in the server
# mod_page_models_server("page_models_ui_1")
