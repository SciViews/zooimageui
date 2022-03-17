#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # Test d'une fenêtre conditionnelle qui affiche soit le dossier des données du serveur,
  # soit qui demande à l'utilisateur de choisir son dossier où sont ses données
  mod_Data_Folder_bis_server("Data_Folder_bis_ui_1")
  
  # Tests de communication entre modules
  test <- mod_ZI_Test_bis_server("ZI_Test_bis_ui_1")
  
  # Test variables globales
  mod_ZI_Test_server("ZI_Test_ui_1", test = test)
  
  # --- Anciens tests ---
  # mod_Data_Test_Server_server("Data_Test_Server_ui_1")
  # mod_Data_Test_Local_server("Data_Test_Local_ui_1")
  # --- Anciens tests ---
  
}
