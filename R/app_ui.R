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
    fluidPage(theme = shinytheme("cerulean"),
      fluidRow(id="body",
        
        column(width = 2, id = "left_col",
          
          mod_fixed_pannel_ui("fixed_pannel_ui_1")
        ),
        
        column(width = 10, id = "right_col",
          tags$div(id = "right_col_div",
            navbarPage("ZooImage-UI v1.0", id = "ziui",
                       
              # Page dans laquelle je peux définir et voir le dossier qui contient mes données
              tabPanel("Settings",
                h3("Settings"),
                tags$hr(),
                mod_page_settings_ui("page_settings_ui_1")
              ),
              
              tabPanel("Samples",
                h3("Samples"),
                tags$hr(),
                mod_page_samples_ui("page_samples_ui_1")
              ),
              
              tabPanel("Training Sets",
                h3("Training Sets"),
                tags$hr(),
                mod_page_training_sets_ui("page_training_sets_ui_1")
              ),
              
              tabPanel("Models",
                h3("Models"),
                tags$hr(),
                mod_page_models_ui("page_models_ui_1")
              ),
              
              tabPanel("Results",
                h3("Results"),
                tags$hr(),
                mod_page_results_ui("page_results_ui_1")
              ),
            )
          )
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
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ZooImageUI'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

