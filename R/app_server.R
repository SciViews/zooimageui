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
    samples_vars = NULL,
    training_sets_vars = NULL,
    models_vars = NULL,
    results_vars = NULL,
  )
  # On peut tout mettre en un seul observe, mais dans ce cas, tout est toujours actualisé.
  observe({
    all_vars$fixed_pannel_vars <- fixed_pannel_vars
  })
  observe({
    all_vars$settings_vars <- settings_vars
  })
  observe({
    all_vars$samples_vars <- samples_vars
  })
  observe({
    all_vars$training_sets_vars <- training_sets_vars
  })
  observe({
    all_vars$models_vars <- models_vars
  })
  observe({
    all_vars$results_vars <- results_vars
  })
  
  # === Panneau Fixe ===
  fixed_pannel_vars <- mod_fixed_pannel_server("fixed_pannel_ui_1", all_vars = all_vars)
  
  # === Settings === (Page)
  settings_vars <- mod_page_settings_server("page_settings_ui_1")
  
  # === Samples === (Page)
  samples_vars <- mod_page_samples_server("page_samples_ui_1", all_vars = all_vars)
  
  # === Training Sets === (Page)
  training_sets_vars <- mod_page_training_sets_server("page_training_sets_ui_1", all_vars = all_vars)
  
  # === Models === (Page)
  models_vars <- mod_page_models_server("page_models_ui_1", all_vars = all_vars)
  
  # === Results === (Page)
  results_vars <- mod_page_results_server("page_results_ui_1", all_vars = all_vars)
  
  # === Tests === (Page)
  mod_page_contact_server("page_contact_ui_1")
  
}
