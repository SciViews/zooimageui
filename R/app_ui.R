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
      h1("Nouvelle app ZooImage avec Golem"),
      navbarPage("ZooImage-UI",
                 
        # tentative de page dans laquelle j'aurais un "panel" conditionnel pour pouvoir choisir le dossier des données dans le cas de l'utilisation de l'app en local
        tabPanel("Data Folder All In One",
          mod_Data_Folder_bis_ui("Data_Folder_bis_ui_1")
        ),
        
        tabPanel("Tests",
          mod_ZI_Test_ui("ZI_Test_ui_1")
        ),
        
        tabPanel("Server folder",
          mod_Data_Test_Server_ui("Data_Test_Server_ui_1")
        ),
        
        tabPanel("Local Folder",
          mod_Data_Test_Local_ui("Data_Test_Local_ui_1")
        )
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

