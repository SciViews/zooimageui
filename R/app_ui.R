#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      navbarPage("ZooImage-UI",
                 
        # Page dans laquelle je peux définir le dossier qui contient mes données
        tabPanel("Data Folder Settings",
          mod_Data_Folder_Settings_ui("Data_Folder_Settings_ui_1")
        ),
        
        # == Tests de com entre modules
        tabPanel("Communicating modules test",
          mod_ZI_Test_bis_ui("ZI_Test_bis_ui_1")
        ),
        
        tabPanel("Tests",
          mod_ZI_Test_ui("ZI_Test_ui_1")
        ),
        # == Tests de com entre modules
        
        
        # --- Anciens tests ---
        # tabPanel("Server folder",
          # mod_Data_Test_Server_ui("Data_Test_Server_ui_1")
        # ),
        
        # tabPanel("Local Folder",
          # mod_Data_Test_Local_ui("Data_Test_Local_ui_1")
        # )
        # --- Anciens tests ---
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'TemplateGolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

