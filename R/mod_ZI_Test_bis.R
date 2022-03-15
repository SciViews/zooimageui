#' ZI_Test_bis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZI_Test_bis_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("textinput"), "Enter what you want :"),
  )
}
    
#' ZI_Test_bis Server Functions
#'
#' @noRd 
mod_ZI_Test_bis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    montexte <- reactiveValues(
      texte = NULL
    )
    
    observe({
      montexte$texte <- input$textinput
    })
    
    return(montexte)
  })
}
    
## To be copied in the UI
# mod_ZI_Test_bis_ui("ZI_Test_bis_ui_1")
    
## To be copied in the server
# mod_ZI_Test_bis_server("ZI_Test_bis_ui_1")
