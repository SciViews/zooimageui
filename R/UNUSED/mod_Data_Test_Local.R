#' Data_Test_Local UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_Test_Local_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      
      sidebarPanel(
        textInput(ns("local_folder_path"),"Folder Path :", value = "~/"),
        selectInput(ns("first_folder"), "First folder :", choices = list.files("~/"))
      ),
      
      mainPanel(
        verbatimTextOutput(ns("folder_ls1")),
        verbatimTextOutput(ns("folder_ls2"))
      )
    )
  )
}
    
#' Data_Test_Local Server Functions
#'
#' @noRd 
mod_Data_Test_Local_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$folder_ls1 <- renderPrint({
      list.files(input$local_folder_path)
    })
    output$folder_ls2 <- renderPrint({
      list.files(paste0("~/",input$first_folder))
    })
  })
}
    
## To be copied in the UI
# mod_Data_Test_Local_ui("Data_Test_Local_ui_1")
    
## To be copied in the server
# mod_Data_Test_Local_server("Data_Test_Local_ui_1")
