#' mod_01_ZI_data_dir_show UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_01_ZI_data_dir_show_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Table of titanic from ~/shared/data/titanic_data.csv"),
    
    sidebarLayout(
      sidebarPanel(
          textInput(ns("user_folder_path"),"Folder path : ") # Ne fonctionne pas ainsi
        ),
      
      mainPanel(
        dataTableOutput(ns("titanic_data_table")),
        verbatimTextOutput(ns("path"))
      )
    )
  )
}
    
#' mod_01_ZI_data_dir_show Server Functions
#'
#' @noRd 
mod_mod_01_ZI_data_dir_show_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_folder_path <- reactive({
      if(golem::get_golem_options("data_folder_path") != "") { # Ne fonctionne pas ainsi
        golem::get_golem_options("data_folder_path") # Ne fonctionne pas ainsi
      } else { # Ne fonctionne pas ainsi
        input$user_folder_path # Ne fonctionne pas ainsi
      } # Ne fonctionne pas ainsi
    })
    
    tita_data <- reactive({
      read.csv(paste0(data_folder_path,"/titanic_data.csv"))
    })
    
    output$titanic_data_table <- renderDataTable({
      tita_data()
    })
    
    output$path <- renderPrint({
      paste0(data_folder_path,"/titanic_data.cvs")
    })
    
  })
}
    
## To be copied in the UI
# mod_mod_01_ZI_data_dir_show_ui("mod_01_ZI_data_dir_show_ui_1")
    
## To be copied in the server
# mod_mod_01_ZI_data_dir_show_server("mod_01_ZI_data_dir_show_ui_1")
