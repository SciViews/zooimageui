#' Data_Folder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_Folder_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Data_Folder Server Functions
#'
#' @noRd 
mod_Data_Folder_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
  })
}
    
## To be copied in the UI
# mod_Data_Folder_ui("Data_Folder_ui_1")
    
## To be copied in the server
# mod_Data_Folder_server("Data_Folder_ui_1")
