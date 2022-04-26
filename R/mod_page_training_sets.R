#' page_training_sets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_training_sets_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    tabsetPanel(
      
# Local TS Preparation UI ------------------------------------------------
      
      tabPanel("Prepare Local Training Set",
        tags$br(),
        tags$h4("LOCAL MODE NOTE"),
        tags$p("If you are using local mode, you can
                do it directly in your folders, without zipping
                and unzipping."),
        tags$p("The resulting sorted training set must be put
                in the Training_Sets folder. You can then click on the
                refresh button to see it."),
        tags$br(),
        
        sidebarLayout(
          
          # Création des training sets
          sidebarPanel(
            tags$h4("Training Set Preparation"),
            textInput(ns("ltsp_name"), "Name"),
            # Choix des zidbs
            selectInput(ns("ltsp_zidbs"), label = "" , choices = NULL, multiple = TRUE),
            # Choix du template ziclass
            selectInput(ns("ltsp_template"), "Template", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            # Préparer le training set (désactivé de base)
            shinyjs::disabled(actionButton(ns("ltsp_prepare"), "Prepare Training Set")),
          ),
          
          # Affichage des training sets existants
          mainPanel(
            tags$h4("Existing Training Sets"),
            # Affichage des training sets existants
            verbatimTextOutput(ns("ltsp_existing_show")),
            actionButton(ns("ltsp_refresh"), "Refresh"),
          )
          
        )
      ),

# Server TS Preparation UI ----------------------------------------------------

      tabPanel("Prepare Server Training Set",
               
        # Présentation
        tags$br(),
        tags$h4("SERVER MODE NOTE"),
        tags$p("In this window, you can download the unsorted 
               training set that you want to use, unzip it, 
               and then manually sort the vignettes inside the 
               good folders."),
        tags$p("You can then zip the sorted training set 
               folder (and only this folder), and upload it to 
               continue."),
        tags$br(),
        
        fluidRow(
          
          sidebarPanel(width = 5,
            tags$h4("Training Set preparation"),
            textInput(ns("stsp_name"), "Name"),
            # Choix des zidbs
            selectInput(ns("stsp_zidbs"), label = "" , choices = NULL, multiple = TRUE),
            # Choix du template ziclass
            selectInput(ns("stsp_template"), "Template", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            # Préparer le training set (désactivé de base)
            shinyjs::disabled(downloadButton(ns("stsp_ts_dl"), "Prepare and Download")),
          ),
          
          sidebarPanel(width = 5,
            tags$h4("Sorted Training Set Upload"),
            # Upload du training set trié
            fileInput(ns("stsp_ts_up"), "Zipped Training Set", multiple = FALSE),
            # Message d'erreur ou de succès
            textOutput(ns("stsp_up_error")),
            tags$h4("Existing Training Sets"),
            # Montrer les ts qui existent
            verbatimTextOutput(ns("stsp_existing_show")),
            # Rafraichir la liste
            actionButton(ns("stsp_refresh"), "Refresh"),
            tags$br(),
            tags$h4("Training Set Delete"),
            # Suppression d'un training set
            selectInput(ns("stsp_ts_to_delete"), NULL, choices = NULL),
            shinyjs::disabled(actionButton(ns("stsp_delete"), "Delete"))
          ),
        ),
      ),

# Training Set Explore UI ----------------------------------------------

      tabPanel("Explore Training Set",
               tags$br(),
               tags$h4("Active Training Set"),
               selectInput(ns("train_set_selection1"), NULL, choices = NULL),
               actionButton(ns("tsv_ref)"), "Refresh"),
               tags$hr(),
               tags$h4("Training Set's Objects"),
               # Affichage du contenu du Training Set choisi
               verbatimTextOutput(ns("tsv_ts_content")),
               tags$hr(),
               # Affichage des classes et du nombre d'éléments dans celles-ci
               tags$h4("Items per Classes from Depth :"),
               selectInput(ns("tsv_depth"), NULL, choices = c(1:5), selected = 5),
               verbatimTextOutput(ns("tsv_classes")),
      )
      
    )
  )
}
    
#' page_training_sets Server Functions
#'
#' @noRd 
mod_page_training_sets_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Taille max pour upload :
    options(shiny.maxRequestSize=30*1024^2)
    # Variable : timer pour enclencher de la réactivité
    timer <- reactiveTimer(3000)
    
# Récupération Des Variables ----------------------------------------------
    
    # settings_vars
    data_folder_path_rea <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path })
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    
    # models_vars
    train_set_selection2 <- reactive({ all_vars$models_vars$train_set_selection2 })

# Variables Globales -------------------------------------------------------------

    # Variable : Chemin du dossier des ts
    ts_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
    })
    
    # Variable : Pour actualiser la liste
    ts_list_update <- reactiveVal(0)
    
    # Variable : liste des training sets existant
    ts_list <- reactive({
      input$ltsp_refresh
      input$stsp_refresh
      input$tsv_ref
      ts_list_update()
      list.files(ts_folder_path())
    })
    
# Local TS Preparation Server ------------------------------------------------
    
    # Mise à jour de la sélection des ZIDB pour la création d'un training set
    observe({
      data_folder_path_rea()
      # Si zidb_files() change et qu'il est non vide, alors on affiche les zidb_files()
      if (length(zidb_files()) > 0) {
        # Local
        updateSelectInput(session, "ltsp_zidbs", label = "Samples", choices = sub("\\.zidb$", "",zidb_files()))
        # Server
        updateSelectInput(session, "stsp_zidbs", label = "Samples", choices = sub("\\.zidb$", "",zidb_files()))
      # Si pas, alors on affiche rien
      } else {
        # Local
        updateSelectInput(session, "ltsp_zidbs", label = "Samples", choices = "No ZIDB file yet")
        # Server
        updateSelectInput(session, "stsp_zidbs", label = "Samples", choices = "No ZIDB file yet")
      }
    })
    
    # Affichage // Liste des training sets existants Local
    output$ltsp_existing_show <- renderPrint ({
      if (length(ts_list()) > 0) {
        ts_list()
      } else {
        "No Training Set yet"
      }
    })
    
    # Variable : nom du training set doit être correct
    ltsp_name <- reactive({
      cor_ts_name(input$ltsp_name, ts_list())
    })
    
    # Variable : input zidb sélectionnés pour le training set mode local
    ltsp_zidbs <- reactive({
      if (req(input$ltsp_zidbs)[1] != "No ZIDB file yet") {
        return(paste0(req(input$ltsp_zidbs), ".zidb"))
      } else {
        return(NULL)
      }
    })
    
    # Création du training set si on clique sur le bouton (et si le nom est correcte, et que des zidb sont sélectionnés)
    observeEvent( input$ltsp_prepare, {
      
      # Test si nom existant et si il y a des zidbs sélectionnés
      req( ltsp_name() , length( ltsp_zidbs() ) > 0)
      
      # Variables : Arguments du prepareTrain
      train_dir <- fs::path(ts_folder_path(),ltsp_name())
      train_files <- fs::path(Samples_folder_path(), ltsp_zidbs())
      train_template <- input$ltsp_template
      
      # Création du training set
      prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
      
      # Actualise la liste des training sets
      ts_list_update(ts_list_update()+1)
    })
    
    # shinyjs : Si nom + zidbs correctes : on peut le créer, sinon bouton désactivé
    observe({
      shinyjs::disable("ltsp_prepare")
      req(data_folder_path_rea(), ltsp_name(), ltsp_zidbs()) 
      shinyjs::enable("ltsp_prepare")
    })
    
# Server TS Preparation Server ---------------------------------------------
    
    # Variable : Nom du Training Set doit être correcte
    stsp_name <- reactive({
      cor_ts_name(input$stsp_name, ts_list())
    })
    
    # Variable : input zidb sélectionnés pour le training set mode server
    stsp_zidbs <- reactive({
      if (req(input$stsp_zidbs) != "No ZIDB file yet") {
        paste0(req(input$stsp_zidbs), ".zidb")
      } else {
        return(NULL)
      }
    })
    
    # Download : Training Set
    output$stsp_ts_dl <- downloadHandler(
      filename = function() {
        # Le fichier s'appellera :
        paste(req(stsp_name()), ".zip", sep = "")
      },
      content = function(file) {
        # Set up du dossier de travail enregistré pour remettre l'ancien
        oldir <- setwd(ts_folder_path())
        
        # A la fin (après l'envoi du zip) supprime le dossier et tout son contenu
        on.exit(unlink(stsp_name(), recursive = TRUE))
        # Remet l'ancien dossier de travail
        on.exit(setwd(oldir), add = TRUE, after = TRUE)
        
        # Test si nom existant et si des zidbs sont sélectionnés
        req( stsp_name() , length( stsp_zidbs() ) > 0)
        
        # Variables : Arguments du prepareTrain
        train_dir <- fs::path(ts_folder_path(),stsp_name())
        train_files <- fs::path(Samples_folder_path(), stsp_zidbs())
        train_template <- input$stsp_template
        
        # Création du training set
        prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
        
        # Envoi du zip du training set
        return(zip(zipfile = file, files = stsp_name()))
      }
    )
    
    # shinyjs : Si nom + zidbs correctes : on peut le créer et télécharger sinon le bouton est désactivé
    observe({
      shinyjs::disable("stsp_ts_dl")
      req(data_folder_path_rea(), stsp_name(), stsp_zidbs()) 
      shinyjs::enable("stsp_ts_dl")
    })
    
    # Mise à jour du fileInput pour ne pas pouvoir le faire quand le chemin du data folder est vide
    observe({
      ts_folder_path()
      if (data_folder_path_rea() == "") {
        shinyjs::disable("stsp_ts_up")
      } else {
        shinyjs::enable("stsp_ts_up")
      }
    })
    
    # Upload : Training Set trié
    stsp_is_uploaded <- eventReactive( input$stsp_ts_up, {
      
      # Besoin d'un data_folder_path non nul
      req(data_folder_path_rea())
      
      # Désactive le bouton upload, pour ne pas surcharger
      shinyjs::disable("stsp_ts_up")
      
      result <- upload_ts(input$stsp_ts_up, ts_folder_path(), ts_list())
      
      # Actualise la liste des training sets
      ts_list_update(ts_list_update()+1)
      
      # Réactive le bouton upload, une fois que tout est fini
      shinyjs::enable("stsp_ts_up")
      
      return(result)
    })
    
    # ReactiveVal pour charger le TS uploadé
    new_ts_up <- reactiveVal(FALSE)
    observe({
      if (req(stsp_is_uploaded())) {
        new_ts_up(TRUE)
      } else {
        new_ts_up(FALSE)
      }
    })
    
    # Affichage // Message d'erreur lors de l'upload ou Done ! si fonctionne
    output$stsp_up_error <- renderText({
      if (!stsp_is_uploaded()) {
        attr(stsp_is_uploaded(), "error")
      } else {
        "Done !"
      }
    })
    
    # Affichage des Training Sets existants
    output$stsp_existing_show <- renderPrint({
      if (length(ts_list()) > 0) {
        ts_list()
      } else {
        "No Training Set yet"
      }
    })
    
    # Mise à jour de la sélection d'un Training Set à supprimer
    observe({
      # si liste de ts non vide
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "stsp_ts_to_delete", NULL, choices = c("[NONE]", ts_list()), selected = "[NONE]")
      } else {
        updateSelectInput(session, "stsp_ts_to_delete", NULL, choices = "[NONE]")
      }
    })
    
    # Mise à jour du bouton delete pour empêcher la suppresion quand pas possible
    observe({
      if (req(input$stsp_ts_to_delete) != "[NONE]") {
        shinyjs::enable("stsp_delete")
      } else {
        shinyjs::disable("stsp_delete")
      }
    })
    
    # Suppression du Training Set désiré
    observeEvent(input$stsp_delete, {
      oldir <- setwd(ts_folder_path())
      on.exit(setwd(oldir))
      unlink(input$stsp_ts_to_delete, recursive = TRUE)
      ts_list_update(ts_list_update()+1)
    })

# Explore Server ----------------------------------------------------
    
    # Mise à jour du sélecteur de Training Set
    observe({
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "train_set_selection1", NULL, choices = c("[NONE]", ts_list()), selected = "[NONE]")
      } else {
        updateSelectInput(session, "train_set_selection1", NULL, choices = "[NONE]")
      }
    })
    # Mise à jour du sélecteur de Training Set s'il a changé dans l'onglet models
    observeEvent(train_set_selection2(), {
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "train_set_selection1", NULL, choices = c("[NONE]", ts_list()), selected = train_set_selection2())
      } else {
        updateSelectInput(session, "train_set_selection1", NULL, choices = "[NONE]")
      }
    })
    # Si on upload, sélectionne le nouveau training set
    observe({
      if (new_ts_up() == TRUE) {
        updateSelectInput(session, "train_set_selection1", NULL, choices = c("[NONE]", ts_list()), selected = attr(stsp_is_uploaded(), "name"))
        new_ts_up(FALSE)
      }
    })
    
    # Affichage // Contenu du Training Set sélectionné
    output$tsv_ts_content <- renderPrint({
      # Si le training set est choisi : on affiche le contenu
      if (req(input$train_set_selection1 != "[NONE]")) {
        path <- fs::path(ts_folder_path(), input$train_set_selection1)
        res <- list.files(path)
        res <- sub("_dat[0-9A-Za-z]\\.RData$", "   ", res[grepl("\\.RData$", res)])
        return(noquote(res))
      } else {
        "No Active Training Set"
      }
    })
    
    # Variable pour savoir combien de vignettes sont classées afin d'empêcher la récupération si aucune
    tsv_ts_classed_vign <- reactive({
      # Si data_folder_path_rea() est non vide, et que on a sélectionné un training set
      if (data_folder_path_rea() != "" && req(input$train_set_selection1) != "[NONE]") {
        # Préparation du chemin
        dir <- fs::path(ts_folder_path(), input$train_set_selection1)
        # Comptage des vignettes totale dans le Training Set
        ts_total_vign <- length(fs::dir_ls(dir, glob = "*.jpg", recurse = TRUE))
        # Comptage des vignettes non classées dans le Training Set
        ts_unsorted_vign <- try(length(fs::dir_ls(fs::path(dir, "_"), glob = "*.jpg", recurse = TRUE)), silent = TRUE)
        # Si le sous-dossier _ n'existe pas :
        if (inherits(ts_unsorted_vign, "try-error")) { return(NULL) }
        ts_sorted_vign <- ts_total_vign - ts_unsorted_vign
        return(ts_sorted_vign)
      }
    })
    
    # Chargement du Training Set si correct
    tsv_training_set <- reactive({
      if (req(input$train_set_selection1) != "[NONE]" && req(tsv_ts_classed_vign()) != 0) {
        path <- fs::path(ts_folder_path(), input$train_set_selection1)
        train <- getTrain(path)
        # Il y a un problème avec cette version de zooimage, il faut changer
        # manuellement la class du Training Set pour qu'il soit en facteur
        train$Class <- factor(train$Class, levels = basename(attr(train, "path")))
        return(train)
      } else {
        return(NULL)
      }
    })
    
    # Affichage // classes du Training Set
    output$tsv_classes <- renderPrint({
      req(tsv_training_set())
      sort(table(recode(tsv_training_set(), depth = input$tsv_depth )$Class))
    })

# Communication -----------------------------------------------------------
    
    # Préparation des variables dans un paquet
    training_sets_vars <- reactiveValues(
      ts_folder_path = NULL,
      ts_list = NULL,
      ts_sel = NULL,
      
      # Come TS/Mod
      train_set_selection1 = NULL,
    )
    
    # Mise à jour des variables dans le paquet
    observe({
      training_sets_vars$ts_folder_path <- ts_folder_path()
      training_sets_vars$ts_list <- ts_list()
      training_sets_vars$ts_sel <- input$train_set_selection1
      
      # Com TS/Mod
      training_sets_vars$train_set_selection1 <- input$train_set_selection1
    })
    
    # Envoi du packet qui contient toutes les variables
    return(training_sets_vars)
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
