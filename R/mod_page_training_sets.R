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
            checkboxGroupInput(ns("ltsp_zidbs"), label = "" , choices = NULL),
            selectInput(ns("ltsp_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            actionButton(ns("ltsp_prepare"), "Prepare Training Set"),
          ),
          
          # Affichage des training sets existants
          mainPanel(
            tags$h4("Existing Training Sets"),
            verbatimTextOutput(ns("ltsp_existing_show")),
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
              checkboxGroupInput(ns("stsp_zidbs"), label = "" , choices = NULL),
              selectInput(ns("stsp_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
              # selectInput(ns("tss_uts_select"), NULL, choices = NULL, width = "80%"),
              shinyjs::disabled(downloadButton(ns("stsp_ts_dl"), "Prepare and Download .zip")),
              textOutput(ns("stsp_existing_show")),
            ),
          ),
          
          column(width = 5,
            sidebarPanel(width = 12,
              tags$h4("Sorted Training Set Upload :"),
              fileInput(ns("stsp_ts_up"), "Upload .zip", multiple = FALSE),
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


# Global Vars -------------------------------------------------------------

    # Variable : Chemin du dossier des ts
    ts_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
    })
    
    # Variable : liste des training sets existant
    ts_list <- reactive({
      timer()
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
      # Si ts_list() change et qu'il est non vide, alors on affiche les ts non triés
      if (length(ts_list()) > 0) {
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
    
    # Création du training set si on clique sur le bouton (et si le nom est correcte, et que des zidb sont sélectionnés)
    observeEvent( input$ltsp_prepare, {
      
      # Vérification : Nom TS pas déjà utilisé et non vide
      ltsp_name <- cor_ts_name(input$ltsp_name, ts_list())
      
      req( ltsp_name , length( input$ltsp_zidbs ) > 0)
      
      # Variables : Arguments du prepareTrain
      train_dir <- fs::path(ts_folder_path(),ltsp_name)
      train_files <- fs::path(Samples_folder_path(), input$ltsp_zidbs)
      train_template <- input$ltsp_template
      
      # Création du training set
      prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
    })
    
    # Affichage // Contenu du training set choisi
    output$ltsp_folder_content <- renderPrint({
      if (length(ts_list()) > 0) {
        ltsp_selected_path <- fs::path(ts_folder_path(), req(input$ltsp_folder_select))
        list.files(ltsp_selected_path)
      }
    })
    
# Server TS Preparation Server ---------------------------------------------
    
    output$wdt <- renderText({
      timer()
      getwd()
    })
    
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
        on.exit(setwd(oldir))
        
        req( stsp_name() , length( input$stsp_zidbs ) > 0)
        
        # Variables : Arguments du prepareTrain
        train_dir <- fs::path(ts_folder_path(),stsp_name())
        train_files <- fs::path(Samples_folder_path(), input$stsp_zidbs)
        train_template <- input$stsp_template
        
        # Création du training set
        prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
        
        # A la fin (après l'envoi du zip) supprime le dossier et tout son contenu
        on.exit(unlink(stsp_name(), recursive = TRUE))
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
      file.copy(stsp_ts_up$datapath, fs::path(stsp_folder_path(), stsp_ts_up$name))
      unzip(fs::path(stsp_folder_path(), stsp_ts_up$name)) # ! Problème !
      
      # Réactive le bouton upload, une fois que tout est fini
      shinyjs::enable("stsp_ts_up")
    })
    
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
