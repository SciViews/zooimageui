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
        
        fluidRow(
          
          column(width = 4,
                 
            fluidRow(
              sidebarPanel(width = 12,
                h4("Model :"),
                # Choix du script
                selectInput(ns("modcre_selected_script"), NULL, choices = NULL),
                # Messages d'aide
                textOutput(ns("modcre_mod_message")),
                textOutput(ns("modcre_mod_comment")),
                # Affichage Training Set sélectionné
                h4("Training Set :"),
                selectInput(ns("modcre_sel_ts"), NULL, choices = NULL),
                textOutput(ns("modcre_selected_ts")),
                # Est-ce que tout fonctionne ?
                h4("Building Classifier"),
                textOutput(ns("modcre_is_everything_ok")),
                # Choix du nom pour sauvegarde
                h4("Save Name :"),
                textInput(ns("modcre_save_name"), NULL),
                # Création du classifieur
                shinyjs::disabled(actionButton(ns("modcre_use_selected_script"), "Create Classifier")),
                tags$br(),
                tags$br(),
                verbatimTextOutput(ns("modcre_is_saved")),
              ),
            ),
            
            fluidRow(
              sidebarPanel(width = 12,
                tags$h4("Load a Classifier :"),
                selectInput(ns("modcre_sel_sav_cla"), NULL, choices = NULL),
                actionButton(ns("sa_re"), "Refresh"),
                shinyjs::disabled(actionButton(ns("modcre_use_saved_class"), "Use Saved Classif")),
                
                tags$h4("Delete a Classifier :"),
                selectInput(ns("modcre_sel_todel"), NULL, choices = NULL),
                shinyjs::disabled(actionButton(ns("modcre_delete"), "Delete")),
              )
            ),
          ),
          
          column(width = 8,
            # Scripts existants
            h4("Existing Models :"),
            actionButton(ns("modcre_refresh"), "Refresh Models List"),
            tags$br(),
            tags$br(),
            verbatimTextOutput(ns("modcre_existing_script_show")),
            verbatimTextOutput(ns("modcre_show_classif")),
          ),
          
        ),
      ),
      
# Test Classifier UI ------------------------------------------------------
      
      # tabPanel("Test Classifier",
      #   
      # ),
      
# Visualise Classifier UI -------------------------------------------------
      
      tabPanel("Visualise Classifier",
        tags$br(),
        # Différents choix de visualisation
        navlistPanel( widths = c(3,9),
          
          # Affichage // Summary
          tabPanel("Summary",
            tags$h4("Summary of the Classifier :"),
            checkboxInput(ns("modvis_clas_showall"), "Show all variables"),
            verbatimTextOutput(ns("modvis_clas_sum")),
          ),
          
          # Affichage // Matrice de Confusion
          tabPanel("Confusion Matrix",
            tags$h4("Confusion Matrix of the Classifier :"),
            verbatimTextOutput(ns("modvis_clas_conf")),
          ),
          
          # Affichage // Plots de la Matrice de Confusion
          tabPanel("Plots",
            tags$h4("Plots of the Confusion Matrix"),
            selectInput(ns("modvis_plot_type"), "Type of Plot :", choices = c("image", "dendrogram", "barplot", "stars")),
            plotOutput(ns("modvis_conf_plot"), height = "700px"),
          ),
        ),
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
    ts_list <- reactive({ all_vars$training_sets_vars$ts_list })
    
    
# Variables Globales ------------------------------------------------------
    
    # Variable : timer pour enclencher de la réactivité
    timer <- reactiveTimer(2000)
    
    # Variable : Chemin vers le dossier des TS !! Peut être pas nécessaire
    ts_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
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
    
    # ---------- Création d'un classifieur ----------
    
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
    
    # Mise à jour du sélecteur de training set
    observe({
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "modcre_sel_ts", NULL, choices = ts_list() )
      } else {
        updateSelectInput(session, "modcre_sel_ts", NULL, choices = "No Training Set yet")
      }
    })
    
    # Variable pour savoir combien de vignettes sont classées afin d'empêcher la récupération si aucune
    modcre_ts_classed_vign <- reactive({
      # Commentaire en page Training Sets
      if (data_folder_path_rea() != "" && req(input$modcre_sel_ts) != "No Training Set yet") {
        dir <- fs::path(ts_folder_path(), input$modcre_sel_ts)
        ts_total_vign <- length(fs::dir_ls(dir, glob = "*.jpg", recurse = TRUE))
        ts_unsorted_vign <- try(length(fs::dir_ls(fs::path(dir, "_"), glob = "*.jpg", recurse = TRUE)), silent = TRUE)
        if (inherits(ts_unsorted_vign, "try-error")) { return(0) }
        ts_sorted_vign <- ts_total_vign - ts_unsorted_vign
        return(ts_sorted_vign)
      }
    })
    
    # Variable charger le training set si possible
    ts_training_set <- reactive ({
      if (req(input$modcre_sel_ts) != "No Training Set yet" && req(modcre_ts_classed_vign()) != 0) {
        path <- fs::path(isolate(ts_folder_path()), input$modcre_sel_ts)
        train <- try(getTrain(path), silent = TRUE)
        # Il y a un problème avec cette version de zooimage, il faut changer
        # manuellement la class du Training Set pour qu'il soit en facteur
        train$Class <- factor(train$Class, levels = basename(attr(train, "path")))
        if (!inherits(train, "try-error")) {
          return(train)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    
    # Test 1 ! Training Set Chargé ?
    modcre_is_ts_loaded <- reactive({
      !is.null(ts_training_set())
    })
    
    # Affichage // Training Set Choisi
    output$modcre_selected_ts <- renderText({
      test <- if (modcre_is_ts_loaded()) { "(Correct)" } else { "(Not Correct)" }
      paste0(input$modcre_sel_ts," ",test)
    })
    
    # Test 2 ! d'éxécution du script (modèle) choisi (renvoie vrai ou faux)
    modcre_is_mod_correct <- reactive({
      req(input$modcre_selected_script, data_folder_path_rea())
      
      # Vérification de mon modèle
      is_script_good_model(models_folder_path(), input$modcre_selected_script)
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
    
    # Test global
    modcre_everything_ok <- reactive({
      data_folder_path_rea()
      result <- FALSE
      attr(result, "message") <- "Training Set or Model not ready"
      # Si le reste est bon
      if (modcre_is_mod_correct() && modcre_is_ts_loaded()) {
        # On essaie de d'utiliser la fonction car pourrait bugger
        script_path <- fs::path(isolate(models_folder_path()), input$modcre_selected_script)
        source(script_path, local = TRUE)
        res <- try(get_classif(ts_training_set()), silent = TRUE)
        # Renvoie FALSE si la fonction bug et un message qui explique
        if (inherits(res, "try-error")) {
          result <- FALSE
          attr(result, "message") <- paste0("Error in Model : ",attr(res, "condition"))
          return(result)
        } else {
          result <- TRUE
          attr(result, "message") <- "Classifier Built"
          attr(result, "classif") <- res
          return(result)
        }
      }
      return(result)
    })
    
    # Affichage // Tout est ok pour la suite ?
    output$modcre_is_everything_ok <- renderText({
      attr(modcre_everything_ok(), "message")
    })
    
    # Variable pour mettre à jour le bouton pour créer le classifieur
    modcre_create_up <- reactiveVal(0)
    
    # Mise à jour du bouton pour utiliser un script models
    observe({
      modcre_create_up()
      # Si le modèle est correcte et que le TS est chargé alors on peut créer le classifieur
      if (modcre_everything_ok()) {
        shinyjs::enable("modcre_use_selected_script")
      # Si pas, on ne peut pas
      } else {
        shinyjs::disable("modcre_use_selected_script")
      }
    })
    
    # Variable : Classifieur si appui sur le bouton
    modcre_classif_new <- eventReactive(modcre_everything_ok(), {
      if (modcre_everything_ok()) {
        return(attr(modcre_everything_ok(), "classif"))
      }
    })
    
    # Event pour la sauvegarde
    is_saved <- eventReactive(input$modcre_use_selected_script, {
      
      req(modcre_classif_new())
      
      # Variable : Nom pour la sauvegarde
      name_without_special_char <- stringr::str_replace_all(input$modcre_save_name, "[^[:alnum:]]", "")
      cla_name <- paste(name_without_special_char, ".RData", sep = "")
      # Variable : Chemin du dossier pour la sauvegarde
      saved_classif_dir <- fs::path(data_folder_path_rea(), "Saved_Classif")
      
      # Création du dossier si il n'existe pas (pour sauvergade)
      if (!"Saved_Classif" %in% list.files(data_folder_path_rea())) {
        fs::dir_create(saved_classif_dir)
      }
      
      # Test si existe déjà
      if (input$modcre_save_name == "") {
        res <- "Not saved, name empty."
        return(res)
      } else if (!cla_name %in% list.files(saved_classif_dir)) {
        classifier <- modcre_classif_new()
        save(classifier, file = fs::path(saved_classif_dir, cla_name))
        modcre_saved_update(modcre_saved_update()+1)
        res <- "Saved !"
        return(res)
      } else {
        res <- "Not saved, model already used. If you want to save a new one, please delete the old one first."
        return(res)
      }
    })
    
    # ---------- Classifieurs sauvegardés ----------
    
    # Variable : Liste des classifieurs sauvegardés
    modcre_saved_classif_list <- reactive({
      input$sa_re
      modcre_saved_update()
      path <- fs::path(data_folder_path_rea(), "Saved_Classif")
      list.files(path)
    })
    
    # Mise à jour de la sélection d'un classifieur sauvegardé
    observe({
      if (length(modcre_saved_classif_list()) > 0) {
        updateSelectInput(session, "modcre_sel_sav_cla", NULL, choices = modcre_saved_classif_list())
      } else {
        updateSelectInput(session, "modcre_sel_sav_cla", NULL, choices = "No Saved Classif yet")
      }
    })
    
    # Mise à jour du bouton pour utiliser le classifieur sauvegardé
    observe({
      shinyjs::disable("modcre_use_saved_class")
      if (req(input$modcre_sel_sav_cla) != "No Saved Classif yet") {
        shinyjs::enable("modcre_use_saved_class")
      }
    })
    
    # Affichage // Est-ce que le classifieur a été sauvegardé ?
    output$modcre_is_saved <- renderText({
      req(modcre_classif_new())
      is_saved()
    })
    
    # ---------- Récupère soit un nouveau classif, soit un sauvegardé ----------
    
    # Variable : classifieur
    modcre_classif <- reactiveVal()
    # Si on Crée un classifieur, alors le classifieur utilisé sera celui là
    new_cla <- reactiveVal() # Pour activer la réactivité du nom spécifiquement
    observe({
      modcre_classif(req(modcre_classif_new()))
      new_cla(new_cla()+1) # Fait réagir spécifiquement
    })
    # Si on charge un classifieur sauvegardé, alors le classifieur utilisé sera celui là
    saved_cla <- reactiveVal() # Pour activer la réactivité du nom spécifiquement
    observeEvent(input$modcre_use_saved_class, {
      # Si classifier existe déjà (d'un autre .RData) on le supprime
      if (exists("classifier")) { rm(classifier) }
      path <- fs::path(data_folder_path_rea(),"Saved_Classif",input$modcre_sel_sav_cla)
      load(path)
      # Récupération de la variable classifier depuis le .RData
      modcre_classif(classifier)
      saved_cla(saved_cla()+1) # Fait réagir spécifiquement
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
    output$modcre_show_classif <- renderPrint({
      req(modcre_classif())
    })
    
    # Variable : Matrice de confusion du classifieur
    modcre_classif_conf <- eventReactive(req(modcre_classif()), {
      # Pour la méthode mlRforest, j'utilise les éléments out-of-bag
      # Le choix de leur utilisation pourrait être laissé à l'utilisateur
      if (attr(modcre_classif(), "method") == "mlRforest") {
        res <- confusion(modcre_classif(), predict(modcre_classif(), method = "oob"))
      } else {
        confusion(modcre_classif())
      }
    })
    
    # ---------- Suppression d'un classifieur sauvegardé ----------
    
    # Mise à jour de la sélection du classifieur à supprimer
    observe({
      if (length(modcre_saved_classif_list()) > 0) {
        updateSelectInput(session, "modcre_sel_todel", NULL, choices = modcre_saved_classif_list())
      } else {
        updateSelectInput(session, "modcre_sel_todel", NULL, choices = "No Saved Classif yet")
      }
    })
    
    # Mise à jour du bouton pour supprimer un classif sauvegardé
    observe({
      shinyjs::disable("modcre_delete")
      if (req(input$modcre_sel_todel) != "No Saved Classif yet") {
        shinyjs::enable("modcre_delete")
      }
    })
    
    # Variable : Mise à jour de la liste des classif sauvegardés
    modcre_saved_update <- reactiveVal(0)
    
    # Suppression du classif sauvegardé si appui
    observeEvent(input$modcre_delete, {
      # Chemin
      path <- fs::path(data_folder_path_rea(), "Saved_Classif")
      oldir <- setwd(path)
      on.exit(setwd(oldir))
      # Suppression du fichier
      unlink(input$modcre_sel_todel)
      # Mise à jour de la liste
      modcre_saved_update(modcre_saved_update()+1)
      # Si il était actif, on désactive le classifieur
      if (input$modcre_sel_todel == paste0(modvis_clas_name(), ".RData")) {
        modcre_classif(NULL)
        modvis_clas_name(NULL)
      }
    })
    
# Test Classifier Server --------------------------------------------------
    
    # Peut-être plus tard.
    
# Visualise Classifier Server ---------------------------------------------
    
    # Variable : Nom du classifieur actif
    modvis_clas_name <- reactiveVal()
    # Soit le nom d'un nouveau
    observeEvent(new_cla(), {
      req(modcre_classif(), data_folder_path_rea())
      modvis_clas_name(sub("\\.R$", "", input$modcre_selected_script))
    })
    # Soit le nom d'un sauvegardé
    observeEvent(saved_cla(), {
      req(modcre_classif(), data_folder_path_rea())
      modvis_clas_name(sub("\\.RData$", "", input$modcre_sel_sav_cla))
    })
    
    # Affichage // Summary du classifieur
    output$modvis_clas_sum <- renderPrint({
      if (!input$modvis_clas_showall) {
        summary(req(modcre_classif()), type = c("Fscore", "Recall", "Precision"))
      } else {
        summary(req(modcre_classif()))
      }
    })
    
    # Affichage // Matrice de confusion du classifieur
    output$modvis_clas_conf <- renderPrint({
      req(modcre_classif_conf())
    })
    
    # Affichage // Plot de Matrice de Confusion
    output$modvis_conf_plot <- renderPlot({
      req(data_folder_path_rea())
      plot(req(modcre_classif_conf()), type = input$modvis_plot_type)
    })
    
# Communication -----------------------------------------------------------
    
    # Préparation des variables dans un paquet
    models_vars <- reactiveValues(
      modvis_clas_name = NULL,
      modcre_classif = NULL,
    )
    
    # Mise à jour des variables dans le paquet
    observe({
      models_vars$modvis_clas_name <- modvis_clas_name()
      models_vars$modcre_classif <- modcre_classif()
    })
    
    # Envoi du packet qui contient toutes les variables
    return(models_vars)
    
  })
}
    
## To be copied in the UI
# mod_page_models_ui("page_models_ui_1")
    
## To be copied in the server
# mod_page_models_server("page_models_ui_1")
