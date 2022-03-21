#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # Panneau fixe de mon app
  mod_Fixed_Pannel_server("Fixed_Pannel_ui_1", settings_vars = settings_vars)
  
  # === Settings === (Page)
  settings_vars <- mod_Data_Folder_Settings_server("Data_Folder_Settings_ui_1")
  
  # === Samples === (Page)
  mod_Samples_main_server("Samples_main_ui_1", settings_vars = settings_vars)
  
  # Tests de communication entre modules
  test <- mod_ZI_Test_bis_server("ZI_Test_bis_ui_1")
  
  # Test variables globales
  mod_ZI_Test_server("ZI_Test_ui_1", test = settings_vars)
  
  # --- Anciens tests ---
  # mod_Data_Test_Server_server("Data_Test_Server_ui_1")
  # mod_Data_Test_Local_server("Data_Test_Local_ui_1")
  # --- Anciens tests ---
  
}
