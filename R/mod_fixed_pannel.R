#' fixed_pannel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fixed_pannel_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "fixed_pannel_div",
             tags$h4("Global Informations", id = "fixed_pannel_title"),
             
             tags$hr(),
             tags$h4("-> Global Settings"),
             tags$h5("Data folder path :"),
             textOutput(ns("data_folder_path")),
             
             tags$hr(),
             tags$h4("-> Samples"),
             tags$h5("Visualising :"),
             selectInput(ns("zidb_show"), NULL, choices = NULL),
             textOutput(ns("zidb_show_nrow")),
             
             tags$hr(),
             tags$h4("-> Training Sets"),
             tags$h5("Visualising :"),
             textOutput(ns("ts_sel_show")),
             tags$h5("Progression :"),
             textOutput(ns("ts_prog_show")),
             
             tags$hr(),
             tags$h4("-> Models"),
             tags$h5("Active Classifier :"),
             textOutput(ns("mod_act_clas")),
             
             tags$hr(),
             tags$h4("-> Results"),
             tags$h5("Made Calculation :"),
             textOutput(ns("res_calc_name")),
    )
  )
}

#' fixed_pannel Server Functions
#'
#' @noRd 
mod_fixed_pannel_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Récupération Des Variables ----------------------------------------------
    
    # settings_vars
    data_folder_path <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    zidb_df_nrow <- reactive({ all_vars$samples_vars$zidb_df_nrow })
    
    # training_sets_vars
    ts_folder_path <- reactive({ all_vars$training_sets_vars$ts_folder_path })
    ts_sel <- reactive({ all_vars$training_sets_vars$ts_sel })
    
    # models_vars
    mod_clas_name <- reactive({ all_vars$models_vars$modvis_clas_name })
    
    # results_vars
    calc_name <- reactive({ all_vars$results_vars$calc_name })
    
    # Settings ----------------------------------------------------------------
    
    # Affichage // data_folder_path
    output$data_folder_path <- renderText({ data_folder_path() })
    
    # Samples -----------------------------------------------------------------
    
    # Mise à jour du choix du ZIDB à montrer dans samples
    observe({
      if (length(zidb_files()) > 0) {
        updateSelectInput(session, "zidb_show", NULL, choices = zidb_files())
      } else {
        updateSelectInput(session, "zidb_show", NULL, choices = "No ZIDB file yet")
      }
    })
    
    # Affichage // Nombre de lignes dans le ZIDB choisi
    output$zidb_show_nrow <- renderText({
      if (length(zidb_files()) > 0) {
        paste0(zidb_df_nrow(), " rows in the sample")
      } else {
        "No ZIDB file yet"
      }
    })
    
    # Training Sets -----------------------------------------------------------
    
    # Affichage // Training set choisi pour visualisation
    output$ts_sel_show <- renderText({
      ts_sel()
    })
    
    # Affiche le nombre de vignettes classées par rapport au nombre total du training set.
    # Si tout est en ordre, soit si un training set est sélectionné
    output$ts_prog_show <- renderText({
      if (data_folder_path() != "" && req(ts_sel()) != "No Training Set yet") {
        # Variable : dossier
        dir <- fs::path(ts_folder_path(), ts_sel())
        # Calcul du nombre de vignettes au total dans le dossier
        ts_total_vign <- length(fs::dir_ls(dir, glob = "*.jpg", recurse = TRUE))
        # Calcul du nombre de vignettes dans le sous-dossier des non triés
        ts_unsorted_vign <- try(length(fs::dir_ls(fs::path(dir, "_"), glob = "*.jpg", recurse = TRUE)), silent = TRUE)
        # Dans le cas où il n'y a pas de dossier _ :
        if (inherits(ts_unsorted_vign, "try-error")) { return("Folder not Good") }
        # Calcul du nombre de vignettes triées
        ts_sorted_vign <- ts_total_vign - ts_unsorted_vign
        # classed_rate <- (ts_sorted_vign/ts_total_vign) * 100
        paste("Sorted : ", ts_sorted_vign, " / ",ts_total_vign)
      } else {
        "No Training Set yet"
      }
    })
    
    # Models ------------------------------------------------------------------
    
    # Affichage // Classifieur actif
    output$mod_act_clas <- renderText({
      if (!is.null(mod_clas_name())) {
        mod_clas_name()
      } else {
        "No active Classifier"
      }
    })
    
    # Results -----------------------------------------------------------------
    
    # Affichage // Nom du scrip utilisé pour résultats
    output$res_calc_name <- renderText({
      if (!is.null(calc_name())) {
        calc_name()
      } else {
        "No made Calculation"
      }
    })
    
    # Communication -----------------------------------------------------------
    
    fixed_pannel_vars <- reactiveValues(
      zidb_show = NULL,
    )
    
    observe({
      fixed_pannel_vars$zidb_show <- if (req(input$zidb_show) != "No ZIDB file yet") {
        input$zidb_show
      }
    })
    
    return(fixed_pannel_vars)
    
  })
}

## To be copied in the UI
# mod_fixed_pannel_ui("fixed_pannel_ui_1")

## To be copied in the server
# mod_fixed_pannel_server("fixed_pannel_ui_1")
