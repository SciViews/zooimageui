#' page_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_models_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

# Create Classifier UI ----------------------------------------------------

      tabPanel("Create Classifier",
        tags$br(),
        sidebarLayout(
          
          sidebarPanel(
            h4("Chose the Script to use :"),
            selectInput(ns("modcre_selected_script"), NULL, choices = NULL),
            h4("Chosen Traing Set :"),
            textOutput(ns("modcre_selected_ts")),
            tags$br(),
            shinyjs::disabled(actionButton(ns("modcre_use_selected_script"), "Use Script")),
          ),
          
          mainPanel(
            h4("Existing Scripts :"),
            actionButton(ns("modcre_refresh"), "Refresh Scripts List"),
            tags$br(),
            tags$br(),
            verbatimTextOutput(ns("modcre_existing_script_show")),
            verbatimTextOutput(ns("modcre_test")),
          ),
          
        ),
      ),
      
# Test Classifier UI ------------------------------------------------------
      
      tabPanel("Test Classifier",
        
      ),
      
# Visualise Classifier UI -------------------------------------------------
      
      tabPanel("Visualise Classifier",
        tags$h4("Current Classifier :"),
        textOutput(ns("modvis_cur_clas")),
        tags$h4("Summary of the Classifier :"),
        verbatimTextOutput(ns("modvis_clas_sum")),
        tags$h4("Confusion Matrix of the Classifier :"),
        verbatimTextOutput(ns("modvis_clas_conf")),
      ),
      
    )
  )
}
    
#' page_models Server Functions
#'
#' @noRd 
mod_page_models_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
# Récupération Des Variables -----------------------------------------------
    
    # Settings Vars :
    data_folder_path_rea <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    
    # Training Sets Vars :
    ts_name <- reactive({ all_vars$training_sets_vars$ts_name })
    ts_training_set <- reactive({ all_vars$training_sets_vars$ts_training_set })
    
    
# Variables Globales ------------------------------------------------------
    
    # Variable : Chemin vers le dossier des TS !! Peut être pas nécessaire
    ts_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Traing_Sets")
    })
    
    # Variable : Chemin vers le dossier des scripts models
    models_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Models")
    })
    
    # Variable : liste des scripts dans Models
    scripts_list <- reactive({
      input$modcre_refresh
      list.files(models_folder_path())
    })
    
# Create Classifier Server ------------------------------------------------
    
    # Mise à jour du sélecteur de scripts models
    observe({
      # Si il y a des scripts :
      if (length(scripts_list()) > 0) {
        updateSelectInput(session, "modcre_selected_script", NULL, choices = scripts_list())
      # Si pas :
      } else {
        updateSelectInput(session, "modcre_selected_script", NULL, choices = "No Script yet")
      }
    })
    
    # Affichage // Training Set Choisi
    output$modcre_selected_ts <- renderText({
      ts_name()
    })
    
    # Mise à jour du bouton pour utiliser un script models
    observe({
      shinyjs::disable("modcre_use_selected_script")
      # Si script choisi, script est .R, TS chargé, alors bouton actif
      if (req(input$modcre_selected_script) != "No Script yet" && 
          grepl(".R", req(input$modcre_selected_script)) && 
          !is.null(ts_training_set())) {
        shinyjs::enable("modcre_use_selected_script")
      }
    })
    
    # Affichage // Liste des scipts dans le dossier Models
    output$modcre_existing_script_show <- renderPrint({
      # Si il y en a :
      if (length(scripts_list()) > 0) {
        scripts_list()
      # Si pas :
      } else {
        "No Script yet"
      }
    })
    
    # Variable : Si on utilise le script, retourne la variable result de ce dernier
    modcre_classif <- eventReactive(input$modcre_use_selected_script, {
      
      # Variable qui prend le training set chargé pour le script
      training_set <- ts_training_set()
      
      # Il faut un local = TRUE au source afin que l'environnement utilisé soit celui de l'app
      source(fs::path(models_folder_path(), input$modcre_selected_script), local = TRUE)
      
      # Si la variable classif existe bien, on la renvoie
      if (exists("classif")) {
        attr(classif, "is_mlRforest") <- is_mlRforest
        return(classif)
      } else {
        return(NULL)
      }
    })
    
    # Affichage // Test de récupérer le résultat d'un script
    output$modcre_test <- renderPrint({
      req(modcre_classif())
    })
    
    # Variable : Matrice de confusion du classifieur
    modcre_classif_conf <- eventReactive(req(modcre_classif()), {
      if (attr(req(modcre_classif()), "is_mlRforest")) {
        return(confusion(modcre_classif(), predict(modcre_classif(), method = "oob")))
      } else {
        return(confusion(modcre_classif()))
      }
    })
    
# Test Classifier Server --------------------------------------------------
    
    
    
# Visualise Classifier Server ---------------------------------------------
    
    # Affichage // script de classifieur utilisé
    output$modvis_cur_clas <- renderText({
      input$modcre_selected_script
    })
    
    # Affichage // Summary du classifieur
    output$modvis_clas_sum <- renderPrint({
      summary(req(modcre_classif()))
    })
    
    # Affichage // Matrice de confusion du classifieur
    output$modvis_clas_conf <- renderPrint({
      req(modcre_classif_conf())
    })
    
  })
}
    
## To be copied in the UI
# mod_page_models_ui("page_models_ui_1")
    
## To be copied in the server
# mod_page_models_server("page_models_ui_1")
