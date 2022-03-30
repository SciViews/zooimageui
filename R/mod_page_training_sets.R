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
            selectInput(ns("tsp_folder_select"), "Train Set to visualize", choices = NULL ),
            verbatimTextOutput(ns("tsp_folder_content")),
          )
          
        )
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
      input$tsp_prepare
      list.files(tsp_unsorted_folder_path())
    })
    
    # Mise à jour de la sélection des ZIDB pour la création d'un training set
    observe({
      if (length(zidb_files()) > 0) {
        updateCheckboxGroupInput(session, "tsp_zidbs", label = "Select samples :", choices = zidb_files())
      } else {
        updateCheckboxGroupInput(session, "tsp_zidbs", label = "Select samples :", choices = NULL)
      }
    })
    
    # Affichage // Liste des training sets non triés existants
    output$tsp_existing_show <- renderPrint({
      if (length(tsp_unsorted_list()) > 0) {
        tsp_unsorted_list()
      } else {
        "No unsorted training set found"
      }
    })
    
    observeEvent( input$tsp_prepare, {
      
      # Retirer les caractères spéciaux du nom de dossier, et arrêter si le nom 
      # du dossier est vide ou si pas de zidb cochés
      tsp_name <- stringr::str_replace_all( input$tsp_name, "[^[:alnum:]]", "")
      req( tsp_name != "", length( input$tsp_zidbs ) > 0)
      
      # Preparation des arguments
      traindir <- fs::path(tsp_unsorted_folder_path(),tsp_name)
      train_files <- fs::path(Samples_folder_path(), input$tsp_zidbs)
      train_template <- input$ts_template
      
      # Création du training set
      prepareTrain( traindir = traindir, zidbfiles = train_files, template = train_template ) # <---------------------------- Régler ce problème : Warning in checkFileExists(zicfile) : file not found: NA
    })
    
  })
}
    
## To be copied in the UI
# mod_page_training_sets_ui("page_training_sets_ui_1")
    
## To be copied in the server
# mod_page_training_sets_server("page_training_sets_ui_1")
