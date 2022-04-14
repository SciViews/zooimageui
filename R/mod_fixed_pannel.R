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
             tags$h5("Samples folder path :"),
             textOutput(ns("Samples_folder_path")),
             tags$br(),
             selectInput(ns("zidb_show"), NULL, choices = NULL),
             textOutput(ns("zidb_show_nrow")),
             
             tags$hr(),
             tags$h4("-> Training Sets"),
             selectInput(ns("ts_select"), NULL, choices = "No Training Set yet"),
             actionButton(ns("ts_refresh"), "Refresh"),
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
    Samples_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path})
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    zidb_df_nrow <- reactive({ all_vars$samples_vars$zidb_df_nrow })
    
    # training_sets_vars
    ts_folder_path <- reactive({ all_vars$training_sets_vars$ts_folder_path })
    ts_list <- reactive({ all_vars$training_sets_vars$ts_list })
    
    # models_vars
    mod_clas_name <- reactive({ all_vars$models_vars$modvis_clas_name })
    
    # results_vars
    calc_name <- reactive({ all_vars$results_vars$calc_name })
    
    # Settings ----------------------------------------------------------------
    
    # Affichage // data_folder_path
    output$data_folder_path <- renderText({ data_folder_path() })
    # Affichage // Samples_folder_path
    output$Samples_folder_path <- renderText({ Samples_folder_path() })
    
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
    
    # Mise à jour du sélecteur de Training Set
    observe({
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "ts_select", NULL, choices = ts_list())
      } else {
        updateSelectInput(session, "ts_select", NULL, "No Training Set yet")
      }
    })
    
    # Affiche le nombre de vignettes classées par rapport au nombre total du training set.
    # Si tout est en ordre, soit si un training set est sélectionné
    output$ts_prog_show <- renderText({
      if (data_folder_path() != "" && req(input$ts_select) != "No Training Set yet") {
        # Variable : dossier
        dir <- fs::path(ts_folder_path(), input$ts_select)
        # Calcul du nombre de vignettes au total dans le dossier
        ts_total_vign <- length(fs::dir_ls(dir, glob = "*.jpg", recurse = TRUE))
        # Calcul du nombre de vignettes dans le sous-dossier des non triés
        ts_unsorted_vign <- try(length(fs::dir_ls(fs::path(dir, "_"), glob = "*.jpg", recurse = TRUE)), silent = TRUE)
        # Dans le cas où il n'y a pas de dossier _ :
        if (inherits(ts_unsorted_vign, "try-error")) { return("Folder Incorrect") }
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
      ts_select = NULL,
      ts_refresh = NULL,
    )
    
    observe({
      fixed_pannel_vars$zidb_show <- if (req(input$zidb_show) != "No ZIDB file yet") {
        input$zidb_show
      }
    })
    
    # Envoi du TS choisi
    observe({ fixed_pannel_vars$ts_select <- input$ts_select })
    
    # Envoi du Rafraichissement de la liste
    observe({ fixed_pannel_vars$ts_refresh <- input$ts_refresh })
    
    return(fixed_pannel_vars)
    
  })
}

## To be copied in the UI
# mod_fixed_pannel_ui("fixed_pannel_ui_1")

## To be copied in the server
# mod_fixed_pannel_server("fixed_pannel_ui_1")
