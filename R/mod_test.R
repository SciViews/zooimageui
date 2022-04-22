#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    # 
    # selectInput(ns("test_select1"), "Select 1 :", choices = NULL),
    # selectInput(ns("test_select2"), "Select 2 :", choices = NULL),
    # 
    # h3("Resultat"),
    # verbatimTextOutput(ns("resultat")),
  )
}
    
#' test Server Functions
#'
#' @noRd 
mod_test_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # 
    # zidbs <- reactive({ all_vars$samples_vars$zidb_files })
    # 
    # observe({
    #   if (length(zidbs()) > 0) {
    #     updateSelectInput(session, "test_select1", "Select 1 :", choices = zidbs())
    #     updateSelectInput(session, "test_select2", "Select 2 :", choices = zidbs())
    #   } else {
    #     updateSelectInput(session, "test_select1", "Select 1 :", choices = "[NONE]")
    #     updateSelectInput(session, "test_select2", "Select 2 :", choices = "[NONE]")
    #   }
    # })
    # 
    # result <- reactiveVal()
    # 
    # observeEvent(input$test_select1, {
    #   result(input$test_select1)
    # })
    # 
    # observeEvent(input$test_select2, {
    #   result(input$test_select2)
    # })
    # 
    # 
    # output$resultat <- renderPrint({
    #   result()
    # })
    
    test <- reactive({
      "Bonjour"
    })
  })
}
    
## To be copied in the UI
# mod_test_ui("test_ui_1")
    
## To be copied in the server
# mod_test_server("test_ui_1")
