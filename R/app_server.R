#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # Management des communications
  # Permet de tout utiliser partout ainsi que clarifier les arguments des modules.
  all_vars <- reactiveValues(
    fixed_pannel_vars = NULL,
    settings_vars = NULL,
    Samples_vars = NULL,
  )
  # On peut tout mettre en un seul observe, mais dans ce cas, tout est toujours actualisé.
  observe({
    all_vars$fixed_pannel_vars <- fixed_pannel_vars
  })
  observe({
    all_vars$settings_vars <- settings_vars
  })
  observe({
    all_vars$Samples_vars <- Samples_vars
  })
  
  # === Panneau Fixe ===
  fixed_pannel_vars <- mod_Fixed_Pannel_server("Fixed_Pannel_ui_1", all_vars = all_vars)
  
  # === Settings === (Page)
  settings_vars <- mod_Data_Folder_Settings_server("Data_Folder_Settings_ui_1")
  
  # === Samples === (Page)
  Samples_vars <- mod_Samples_main_server("Samples_main_ui_1", all_vars = all_vars)
  
  # Tests de communication entre modules
  test <- mod_ZI_Test_bis_server("ZI_Test_bis_ui_1")
  
  # Test variables globales
  mod_ZI_Test_server("ZI_Test_ui_1", test = settings_vars, all_vars = all_vars)
  
  # --- Anciens tests ---
  # mod_Data_Test_Server_server("Data_Test_Server_ui_1")
  # mod_Data_Test_Local_server("Data_Test_Local_ui_1")
  # --- Anciens tests ---
  
}
