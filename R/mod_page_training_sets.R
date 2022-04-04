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
      
# Training Set Preparation UI ------------------------------------------------
      
      tabPanel("Training Set Preparation",
        tags$br(),
        sidebarLayout(
          
          # Création des training sets
          sidebarPanel(
            tags$h4("Preparation"),
            textInput(ns("tsp_name"), "Name of the new Training Set"),
            checkboxGroupInput(ns("tsp_zidbs"), label = "" , choices = NULL),
            selectInput(ns("tsp_template"), "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
            actionButton(ns("tsp_prepare"), "Prepare Training Set"),
          ),
          
          # Affichage des training sets existants
          mainPanel(
            tags$h4("Existing Training Sets"),
            verbatimTextOutput(ns("tsp_existing_show")),
            selectInput(ns("tsp_folder_select"), "Training Set folder's content :", choices = NULL ),
            verbatimTextOutput(ns("tsp_folder_content")),
          )
          
        )
      ),

# Training Set Sorting UI ----------------------------------------------------

      tabPanel("Training Set Sorting",
               
        # Présentation
        tags$br(),
        tags$h4("Server MODE NOTE :"),
        tags$p("In this window, you can download the unsorted 
               training set that you want to use, unzip it, 
               and then manually sort the vignettes inside the 
               good folders. You can then zip the training set 
               folder (and only this folder), and upload it to 
               continue in the process."),
        tags$h4("LOCAL MODE NOTE :"),
        tags$p("If you are using local mode, you can
                do it directly in your folders, without zipping
                and unzipping, nor downloading or uploading."),
        tags$p("The resulting sorted training set must be putted
                in the TS_Sorted folder. You can then refresh
                the page to see it."),
        tags$br(),
        
        fluidRow(
          
          column(width = 5, offset = 1,
            sidebarPanel(width = 12,
              tags$h4("Unsorted Training Set Download :"),
              selectInput(ns("tss_uts_select"), NULL, choices = NULL, width = "80%"),
              shinyjs::disabled(downloadButton(ns("tss_uts_download"), "Download .zip")),
            ),
          ),
          
          column(width = 5,
            sidebarPanel(width = 12,
              tags$h4("Sorted Training Set Upload :"),
              fileInput(ns("tss_sts_upload"), "Upload .zip", multiple = FALSE),
            ),
          ),
        ),
      ),

# Training Set Visualisation UI ----------------------------------------------

      tabPanel("Visualisation",
        
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

# Training Set Preparation Server ------------------------------------------------
    
    # Variable : Chemin du dossier des ts non triés
    tsp_unsorted_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"TS_Unsorted")
    })
    
    # Variable : liste des training sets non triés existants
    tsp_unsorted_list <- reactive({
      timer()
      list.files(tsp_unsorted_folder_path())
    })
    
    # Mise à jour de la sélection des ZIDB pour la création d'un training set
    observe({
      # Si zidb_files() change et qu'il est non vide, alors on affiche les zidb_files()
      if (length(zidb_files()) > 0) {
        updateCheckboxGroupInput(session, "tsp_zidbs", label = "Select samples :", choices = zidb_files())
      # Si pas, alors on affiche rien
      } else {
        updateCheckboxGroupInput(session, "tsp_zidbs", label = "Select samples :", choices = NULL)
      }
    })
    
    # Mise à jour de la sélection du training set non trié pour voir son contenu +++ Training Set Sorting !!!
    observe({
      # Si tsp_unsorted_list() change et qu'il est non vide, alors on affiche les ts non triés
      if (length(tsp_unsorted_list()) > 0) {
        updateSelectInput(session, "tsp_folder_select", "Training Set folder's content :", choices = tsp_unsorted_list())
        updateSelectInput(session, "tss_uts_select", NULL, choices = tsp_unsorted_list())
      # Si pas, alors on affiche rien
      } else {
        updateSelectInput(session, "tsp_folder_select", "Training Set folder's content :", choices = "No unsorted Training Set yet")
        updateSelectInput(session, "tss_uts_select", NULL, choices = "No unsorted Training Set yet")
      }
    })
    
    # Affichage // Liste des training sets non triés existants
    output$tsp_existing_show <- renderPrint ({
      if (length(tsp_unsorted_list()) > 0) {
        tsp_unsorted_list()
      } else {
        "No unsorted Training Set found"
      }
    })
    
    # Création du training set si on clique sur le bouton (et si le nom est correcte, et que des zidb sont sélectionnés)
    observeEvent( input$tsp_prepare, {
      
      # Variable : noms non autorisés car vide ou déjà utilisé
      invalid_names <- append(tsp_unsorted_list(), "")
      
      # Retirer les caractères spéciaux du nom de dossier, et arrêter si le nom 
      # du dossier est vide ou si pas de zidb cochés
      tsp_name <- if (!(input$tsp_name %in% invalid_names)) {
        stringr::str_replace_all( input$tsp_name, "[^[:alnum:]]", "")
      }
      req( tsp_name , length( input$tsp_zidbs ) > 0)
      
      # Variables : Arguments du prepareTrain
      train_dir <- fs::path(tsp_unsorted_folder_path(),tsp_name)
      train_files <- fs::path(Samples_folder_path(), input$tsp_zidbs)
      train_template <- input$tsp_template
      
      # Création du training set
      prepareTrain( traindir = train_dir, zidbfiles = train_files, template = train_template ) # <---------------------------- Régler ce problème : Warning in checkFileExists(zicfile) : file not found: NA
    })
    
    # Affichage // Contenu du training set choisi
    output$tsp_folder_content <- renderPrint({
      if (length(tsp_unsorted_list()) > 0) {
        tsp_selected_path <- fs::path(tsp_unsorted_folder_path(), req(input$tsp_folder_select))
        list.files(tsp_selected_path)
      }
    })
    
# Training Set Sorting Server ---------------------------------------------
    
    # Download : Training Set non trié
    output$tss_uts_download <- downloadHandler(
      filename = function() {
        # Le fichier s'appellera :
        paste(input$tss_uts_select, ".zip", sep = "")
      },
      content = function(file) {
        # Ce qui doit être téléchargé est :
        zip(zipfile = file, files = fs::path(tsp_unsorted_folder_path(), input$tss_uts_select)) # ! Problème !
      }
    )
    
    # Si le training set séléctionné est bon on peut le télécharger sinon le bouton est désactivé
    observe({
      if (data_folder_path_rea() != "" && input$tss_uts_select != "No unsorted Training Set yet") {
        shinyjs::enable("tss_uts_download")
      } else {
        shinyjs::disable("tss_uts_download")
      }
    })
    
    # Variable : chemin du dossier TS_Sorted
    tss_sorted_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"TS_Sorted")
    })
    
    # Upload : Training Set trié
    observeEvent( input$tss_sts_upload, {
      
      # Besoin d'un data_folder_path non nul
      req(data_folder_path_rea())
      
      # Désactive le bouton upload, pour ne pas surcharger
      shinyjs::disable("tss_sts_upload")
      
      # Mise en place d'une variable pour récupérer les données dans la table,
      # et test du contenu de celle-ci
      tss_sts_upload <- input$tss_sts_upload
      if ( is.null(tss_sts_upload) )
        return()
      
      # "Copie" du fichier rentrant, pour le stocker dans le système
      file.copy(tss_sts_upload$datapath, fs::path(tss_sorted_folder_path(), tss_sts_upload$name))
      unzip(fs::path(tss_sorted_folder_path(), tss_sts_upload$name)) # ! Problème !
      
      # Réactive le bouton upload, une fois que tout est fini
      shinyjs::enable("tss_sts_upload")
    })
    
    # Variable : liste des training sets triés
    tss_sorted_list <- reactive({
      timer()
      list.files(tss_sorted_folder_path())
    })
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
