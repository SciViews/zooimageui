#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_Data_Folder_server("Data_Folder_ui_1")
  # mod_Data_Test_Server_server("Data_Test_Server_ui_1")
  # mod_Data_Test_Local_server("Data_Test_Local_ui_1")
}
