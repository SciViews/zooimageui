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
    
    conditionalPanel(
      # condition = 'output.is_server_sided == "TRUE",
      condition = "output['is_server_sided'] == 'TRUE'",
      ns = ns,
      
      # textOutput(ns("server_data_folder_path"), "Server Data Folder :"),
      textOutput("server_data_folder_path", "Server Data Folder :"),
    )
  )
}
    
#' Data_Folder Server Functions
#'
#' @noRd 
mod_Data_Folder_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Création de la variable réactive pour le panneau conditionnel mode Server
    output$is_server_sided <- reactive({
      if ( golem::get_golem_options("data_folder_path") != "" ){
        "TRUE"
      } else {
        "FALSE"
      }
    })
    # Nécessaire pour le que browser charge la valeur la plus récente (Dynamic UI)
    # d'output pour évaluer correctement la condition
    outputOptions( output, "is_server_sided", suspendWhenHidden = FALSE )
    
    output$server_data_folder_path <- renderText({
      golem::get_golem_options("data_folder_path")
    })
  })
}
    
## To be copied in the UI
# mod_Data_Folder_ui("Data_Folder_ui_1")
    
## To be copied in the server
# mod_Data_Folder_server("Data_Folder_ui_1")
