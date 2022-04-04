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

# Local TS Preparation Server ------------------------------------------------
    
    # Variable : Chemin du dossier des ts local
    ltsp_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
    })
    
    # Variable : liste des training sets existants local
    ltsp_ts_list <- reactive({
      timer()
      list.files(ltsp_folder_path())
    })
    
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
    
    # Mise à jour de la sélection du training set non trié pour voir son contenu +++ Training Set Sorting !!!
    observe({
      # Si ltsp_ts_list() change et qu'il est non vide, alors on affiche les ts non triés
      if (length(ltsp_ts_list()) > 0) {
        # Local
        updateSelectInput(session, "ltsp_folder_select", "Training Set folder's content :", choices = ltsp_ts_list())
      # Si pas, alors on affiche rien
      } else {
        # Local
        updateSelectInput(session, "ltsp_folder_select", "Training Set folder's content :", choices = "No Training Set yet")
      }
    })
    
    # Affichage // Liste des training sets existants Local
    output$ltsp_existing_show <- renderPrint ({
      if (length(ltsp_ts_list()) > 0) {
        ltsp_ts_list()
      } else {
        "No Training Set found"
      }
    })
    
    # Création du training set si on clique sur le bouton (et si le nom est correcte, et que des zidb sont sélectionnés)
    observeEvent( input$ltsp_prepare, {
      
      # Variable : noms non autorisés car vide ou déjà utilisé
      invalid_names <- append(ltsp_ts_list(), "")
      
      # Retirer les caractères spéciaux du nom de dossier, et arrêter si le nom 
      # du dossier est vide ou si pas de zidb cochés
      ltsp_name <- if (!(input$ltsp_name %in% invalid_names)) {
        stringr::str_replace_all( input$ltsp_name, "[^[:alnum:]]", "")
      }
      req( ltsp_name , length( input$ltsp_zidbs ) > 0)
      
      # Variables : Arguments du prepareTrain
      train_dir <- fs::path(ltsp_folder_path(),ltsp_name)
      train_files <- fs::path(Samples_folder_path(), input$ltsp_zidbs)
      train_template <- input$ltsp_template
      
      # Création du training set
      prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template )
    })
    
    # Affichage // Contenu du training set choisi
    output$ltsp_folder_content <- renderPrint({
      if (length(ltsp_ts_list()) > 0) {
        ltsp_selected_path <- fs::path(ltsp_folder_path(), req(input$ltsp_folder_select))
        list.files(ltsp_selected_path)
      }
    })
    
# Server TS Preparation Server ---------------------------------------------
    
    # Download : Training Set non trié
    # output$stsp_ts_dl <- downloadHandler(
    #   filename = function() {
    #     # Le fichier s'appellera :
    #     paste(input$tss_uts_select, ".zip", sep = "")
    #   },
    #   content = function(file) {
    #     oldir <- setwd(ltsp_folder_path())
    #     on.exit(setwd(oldir))
    #     zip(zipfile = file, files = input$tss_uts_select) # ! Problème ! (2)
    #   }
    # )
    
    # Si le training set séléctionné est bon on peut le télécharger sinon le bouton est désactivé
    # observe({
    #   if (data_folder_path_rea() != "" && input$tss_uts_select != "No Training Set yet") {
    #     shinyjs::enable("stsp_ts_dl")
    #   } else {
    #     shinyjs::disable("stsp_ts_dl")
    #   }
    # })
    
    # Variable : chemin du dossier TS_Sorted
    stsp_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"Training_Sets")
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
    
    # Variable : liste des training sets triés
    stsp_ts_list <- reactive({
      timer()
      list.files(stsp_folder_path())
    })
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
