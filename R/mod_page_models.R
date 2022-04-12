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
            h4("Model :"),
            selectInput(ns("modcre_selected_script"), NULL, choices = NULL),
            verbatimTextOutput(ns("modcre_mod_message")),
            verbatimTextOutput(ns("modcre_mod_comment")),
            h4("Traing Set :"),
            textOutput(ns("modcre_selected_ts")),
            tags$br(),
            shinyjs::disabled(actionButton(ns("modcre_use_selected_script"), "Use Script")),
          ),
          
          mainPanel(
            h4("Existing Models :"),
            actionButton(ns("modcre_refresh"), "Refresh Models List"),
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
    ts_name <- reactive({ all_vars$fixed_pannel_vars$ts_select })
    ts_training_set <- reactive({ all_vars$training_sets_vars$ts_training_set })
    
    
# Variables Globales ------------------------------------------------------
    
    # Variable : timer pour enclencher de la réactivité
    timer <- reactiveTimer(2000)
    
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
        updateSelectInput(session, "modcre_selected_script", NULL, choices = "No Model yet")
      }
    })
    
    # Test 1 ! Training Set Chargé ?
    modcre_is_ts_loaded <- reactive({
      !is.null(ts_training_set())
    })
    
    # Affichage // Training Set Choisi
    output$modcre_selected_ts <- renderText({
      test <- if (modcre_is_ts_loaded()) { "(Correct)" } else { "(Not Correct)" }
      paste(ts_name(), test)
    })
    
    # Test 2 ! d'éxécution du script (modèle) choisi (renvoie vrai ou faux)
    modcre_is_mod_correct <- reactive({
      req(input$modcre_selected_script, data_folder_path_rea())
      
      # Tout d'abord, test du nom du fichier : si ne ressemble pas à un script R, on arrête avec message d'erreur
      if ( input$modcre_selected_script == "No Model yet" || !grepl(".R", input$modcre_selected_script)) {
        res <- FALSE
        attr(res, "message") <- "Selected Model doesn't have .R extension"
        return(res)
      } else {
      # Ensuite, si jamais le nom est bon :
      
        # Supprime la possible ancienne fonction et source le nouveau script
        if (exists("get_classif")) { rm(get_classif) }
        script_path <- fs::path(models_folder_path(), input$modcre_selected_script)
        try(source(script_path , local = TRUE), silent = TRUE)
        
        # Test si une fonction get_classif est récupérée
        if (!exists("get_classif")) {
          res <- FALSE
          attr(res, "message") <- "Selected Model doesn't return a function called get_classif"
          return(res)
        # Test si la fonction a un argument training_set
        } else if (any(!"training_set" %in% names(formals(get_classif)))) {
          res <- FALSE
          attr(res, "message") <- "Selected Model doesn't have a training_set argument"
          return(res)
        # Enfin, si ça passe, alors le modèle est bon
        } else {
          res <- exists("get_classif")
          attr(res, "message") <- "Model : Ok"
          return(res)
        }
        
      }
    })
    
    # Affichage // Message lié au modèle choisi
    output$modcre_mod_message <- renderText({
      # modcre_is_mod_correct()
      # Récupère le message qui explique si le modèle est bon ou non
      attr(modcre_is_mod_correct(), "message")
    })
    
    # Affichage // Commentaire lié au modèle choisi
    output$modcre_mod_comment <- renderText({
      comment(modcre_model())
    })
    
    # Variable : fonction du modèle
    modcre_model <- reactive({
      if (req(modcre_is_mod_correct())) {
        # Chemin du script
        script_path <- fs::path(models_folder_path(), input$modcre_selected_script)
        # Source du script pour récupérer la fonction
        source(script_path , local = TRUE)
        return(get_classif)
      }
    })
    
    # Mise à jour du bouton pour utiliser un script models
    observe({
      shinyjs::disable("modcre_use_selected_script")
      # Si le modèle est correcte et que le TS est chargé alors on peut créer le classifieur
      if (modcre_is_mod_correct() && modcre_is_ts_loaded()) {
        shinyjs::enable("modcre_use_selected_script")
      }
    })
    
    # Variable : Classifieur si appui sur le bouton
    modcre_classif <- eventReactive(input$modcre_use_selected_script, {
      # Crée le classifieur
      return(modcre_model()(ts_training_set()))
    })
    
    # Affichage // Liste des scipts dans le dossier Models
    output$modcre_existing_script_show <- renderPrint({
      # Si il y en a :
      if (length(scripts_list()) > 0) {
        scripts_list()
      # Si pas :
      } else {
        "No Model yet"
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
