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
      
      tabPanel("Local TS Preparation",
        tags$br(),
        tags$h4("LOCAL MODE NOTE :"),
        tags$p("If you are using local mode, you can
                do it directly in your folders, without zipping
                and unzipping, nor downloading or uploading."),
        tags$p("The resulting sorted training set must be putted
                in the TS_Sorted folder. You can then refresh
                the page to see it."),
        tags$br(),
        
        sidebarLayout(
          
          # Création des training sets
          sidebarPanel(
            tags$h4("Training Set Preparation"),
            textInput(ns("ltsp_name"), "Name of the new Training Set"),
            # Choix des zidbs
            checkboxGroupInput(ns("ltsp_zidbs"), label = "" , choices = NULL),
            # Choix du template ziclass
            selectInput(ns("ltsp_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            # Préparer le training set (désactivé de base)
            shinyjs::disabled(actionButton(ns("ltsp_prepare"), "Prepare Training Set")),
          ),
          
          # Affichage des training sets existants
          mainPanel(
            tags$h4("Existing Training Sets"),
            # Affichage des training sets existants
            verbatimTextOutput(ns("ltsp_existing_show")),
            # Actualiser les choix car sinon ça changerais toutes les 3 secondes
            actionButton(ns("ltsp_refresh"), "Refresh"),
            tags$br(),
            tags$br(),
            # Choix du training set pour voir le contenu
            selectInput(ns("ltsp_folder_select"), "Training Set folder's content :", choices = NULL ),
            verbatimTextOutput(ns("ltsp_folder_content")),
          )
          
        )
      ),

# Server TS Preparation UI ----------------------------------------------------

      tabPanel("Server TS Preparation",
               
        # Présentation
        tags$br(),
        tags$h4("SERVER MODE NOTE :"),
        tags$p("In this window, you can download the unsorted 
               training set that you want to use, unzip it, 
               and then manually sort the vignettes inside the 
               good folders. You can then zip the training set 
               folder (and only this folder), and upload it to 
               continue in the process."),
        textOutput(ns("wdt")),
        tags$br(),
        
        fluidRow(
          
          column(width = 5, offset = 1,
            sidebarPanel(width = 12,
              tags$h4("Training Set preparation :"),
              textInput(ns("stsp_name"), "Name of the new Training Set"),
              # Choix des zidbs
              checkboxGroupInput(ns("stsp_zidbs"), label = "" , choices = NULL),
              # Choix du template ziclass
              selectInput(ns("stsp_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
              # Préparer le training set (désactivé de base)
              shinyjs::disabled(downloadButton(ns("stsp_ts_dl"), "Prepare and Download .zip")),
            ),
          ),
          
          column(width = 5,
            sidebarPanel(width = 12,
              tags$h4("Sorted Training Set Upload :"),
              # Upload du training set trié
              fileInput(ns("stsp_ts_up"), "Upload .zip", multiple = FALSE),
              tags$h5("Existing Training Sets :"),
              verbatimTextOutput(ns("stsp_existing_show")),
            ),
          ),
        ),
      ),

# Training Set Visualisation UI ----------------------------------------------

      tabPanel("Visualisation",
        h4("Visualise unsported or sorted ?"),
        selectInput(ns("tsv_unsorted_or_sorted"), label = NULL, choices = c("Unsorted", "Sorted")),
        
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
    
    # fixed_pannel_vars
    ts_fp_refresh <- reactive({ all_vars$fixed_pannel_vars$ts_fp_refresh })


# Global Vars -------------------------------------------------------------

    # Variable : Chemin du dossier des ts
    ts_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
    })
    
    # Variable : Pour actualiser la liste
    ts_list_update <- reactiveVal(0)
    
    # Variable : liste des training sets existant
    ts_list <- reactive({
      ts_fp_refresh()
      ts_list_update()
      input$ltsp_refresh
      list.files(ts_folder_path())
    })
    
# Local TS Preparation Server ------------------------------------------------
    
    # Mise à jour de la sélection des ZIDB pour la création d'un training set
    observe({
      # Si zidb_files() change et qu'il est non vide, alors on affiche les zidb_files()
      if (length(zidb_files()) > 0) {
        # Local
        updateCheckboxGroupInput(session, "ltsp_zidbs", label = "Select samples :", choices = zidb_files())
        # Server
        updateCheckboxGroupInput(session, "stsp_zidbs", label = "Select samples :", choices = zidb_files())
      # Si pas, alors on affiche rien
      } else {
        # Local
        updateCheckboxGroupInput(session, "ltsp_zidbs", label = "Select samples :", choices = NULL)
        # Server
        updateCheckboxGroupInput(session, "stsp_zidbs", label = "Select samples :", choices = NULL)
      }
    })
    
    # Mise à jour de la sélection du training set non trié pour voir son contenu
    observe({
      # Si la liste de training sets change et qu'elle est non vide alors :
      if (length(ts_list() > 0)) {
        # Local
        updateSelectInput(session, "ltsp_folder_select", "Training Set folder's content :", choices = ts_list())
      # Si pas, alors on affiche rien
      } else {
        # Local
        updateSelectInput(session, "ltsp_folder_select", "Training Set folder's content :", choices = "No Training Set yet")
      }
    })
    
    # Affichage // Liste des training sets existants Local
    output$ltsp_existing_show <- renderPrint ({
      if (length(ts_list()) > 0) {
        ts_list()
      } else {
        "No Training Set found"
      }
    })
    
    # Variable : nom du training set doit être correct
    ltsp_name <- reactive({
      cor_ts_name(input$ltsp_name, ts_list())
    })
    
    # Création du training set si on clique sur le bouton (et si le nom est correcte, et que des zidb sont sélectionnés)
    observeEvent( input$ltsp_prepare, {
      
      # Test si nom existant et si il y a des zidbs sélectionnés
      req( ltsp_name() , length( input$ltsp_zidbs ) > 0)
      
      # Variables : Arguments du prepareTrain
      train_dir <- fs::path(ts_folder_path(),ltsp_name())
      train_files <- fs::path(Samples_folder_path(), input$ltsp_zidbs)
      train_template <- input$ltsp_template
      
      # Création du training set
      prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
      
      # Actualise la liste des training sets
      ts_list_update(ts_list_update()+1)
    })
    
    # Si nom + zidbs correctes : on peut le créer, sinon bouton désactivé
    observe({
      shinyjs::disable("ltsp_prepare")
      req(data_folder_path_rea(), ltsp_name(), input$ltsp_zidbs) 
      shinyjs::enable("ltsp_prepare")
    })
    
    # Affichage // Contenu du training set choisi
    output$ltsp_folder_content <- renderPrint({
      if (length(ts_list()) > 0) {
        ltsp_selected_path <- fs::path(ts_folder_path(), req(input$ltsp_folder_select))
        list.files(ltsp_selected_path)
      }
    })
    
# Server TS Preparation Server ---------------------------------------------
    
    # Variable : Nom du Training Set doit être correcte
    stsp_name <- reactive({
      cor_ts_name(input$stsp_name, ts_list())
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
        req( stsp_name() , length( input$stsp_zidbs ) > 0)
        
        # Variables : Arguments du prepareTrain
        train_dir <- fs::path(ts_folder_path(),stsp_name())
        train_files <- fs::path(Samples_folder_path(), input$stsp_zidbs)
        train_template <- input$stsp_template
        
        # Création du training set
        prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
        
        # Envoi du zip du training set
        return(zip(zipfile = file, files = stsp_name()))
      }
    )
    
    # Si nom + zidbs correctes : on peut le créer et télécharger sinon le bouton est désactivé
    observe({
      shinyjs::disable("stsp_ts_dl")
      req(data_folder_path_rea(), stsp_name(), input$stsp_zidbs) 
      shinyjs::enable("stsp_ts_dl")
    })
    
    # Upload : Training Set trié
    observeEvent( input$stsp_ts_up, {
      
      # Besoin d'un data_folder_path non nul
      req(data_folder_path_rea())
      
      # Désactive le bouton upload, pour ne pas surcharger
      shinyjs::disable("stsp_ts_up")
      
      # Mise en place d'une variable pour récupérer les données dans la table,
      # et test du contenu de celle-ci
      stsp_ts_up <- input$stsp_ts_up
      if ( is.null(stsp_ts_up) )
        return()
      
      # "Copie" du fichier rentrant, pour le stocker dans le système
      file.copy(stsp_ts_up$datapath, fs::path(ts_folder_path(), stsp_ts_up$name))
      
      # Changement de dossier pour unzipper correctement
      oldir <- setwd(ts_folder_path())
      # A la fin, retour à lancien dossier de travail
      on.exit(unlink(stsp_ts_up$name))
      on.exit(setwd(oldir), add = TRUE, after = TRUE)
      unzip(stsp_ts_up$name)
      
      # Actualise la liste des training sets
      ts_list_update(ts_list_update()+1)
      
      # Réactive le bouton upload, une fois que tout est fini
      shinyjs::enable("stsp_ts_up")
    })
    
    output$stsp_existing_show <- renderPrint({
      ts_list()
    })
    
# Communication -----------------------------------------------------------
    
    # Préparation des variables dans un paquet
    training_sets_vars <- reactiveValues(
      ts_list = NULL,
      ts_folder_path = NULL,
    )
    
    # Mise à jour des variables dans le paquet
    observe({
      training_sets_vars$ts_folder_path <- ts_folder_path()
      training_sets_vars$ts_list <- ts_list()
    })
    
    # Envoi du packet qui contient toutes les variables
    return(training_sets_vars)
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
