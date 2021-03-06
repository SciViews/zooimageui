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
             tags$h4("-> Working Directory"),
             textOutput(ns("data_folder_path")),
             
             tags$hr(),
             tags$h4("-> Samples"),
             textOutput(ns("zidb_nb")),
             tags$h5("Active Sample"),
             textOutput(ns("zidb_act_show")),
             textOutput(ns("zidb_show_nrow")),
             
             tags$hr(),
             tags$h4("-> Training Sets"),
             tags$h5("Active Training Set"),
             textOutput(ns("ts_sel_show")),
             textOutput(ns("ts_prog_show")),
             
             tags$hr(),
             tags$h4("-> Models"),
             tags$h5("Active Classifier"),
             textOutput(ns("mod_act_clas")),
             textOutput(ns("mod_er_ra")),
             
             tags$hr(),
             tags$h4("-> Results"),
             tags$h5("Active Configuration"),
             textOutput(ns("res_calc_name")),
             textOutput(ns("res_nb_smp")),
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
    smpfiles <- reactive({ all_vars$settings_vars$smps })
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    zidb_df_nrow <- reactive({ all_vars$samples_vars$zidb_df_nrow })
    zidb_show <- reactive({ all_vars$samples_vars$zidb_show })
    
    # training_sets_vars
    ts_folder_path <- reactive({ all_vars$training_sets_vars$ts_folder_path })
    ts_sel <- reactive({ all_vars$training_sets_vars$ts_sel })
    
    # models_vars
    mod_clas_name <- reactive({ all_vars$models_vars$modvis_clas_name })
    mod_err_rate_in_class <- reactive({ all_vars$models_vars$modvis_err_rate_in_class })
    
    # results_vars
    calc_name <- reactive({ all_vars$results_vars$calc_name })
    nb_smp_in_res <- reactive({ all_vars$results_vars$nb_smp_in_res })
    
    # Settings ----------------------------------------------------------------
    
    # Affichage // data_folder_path
    output$data_folder_path <- renderText({ data_folder_path() })
    
    # Samples -----------------------------------------------------------------
    
    # Affichage // Nb de Samples disponibles
    output$zidb_nb <- renderText({
      if (length(smpfiles()) > 0) {
        paste0(length(zidb_files()), " / ", length(smpfiles()), " Samples ready.")
      } else {
        "No Samples found"
      }
    })
    
    # Affichage // ZIDB actif pour la visualisation
    output$zidb_act_show <- renderText({
      data_folder_path()
      if (req(zidb_show()) != "[NONE]") {
        sub("\\.zidb$", "", zidb_show())
      } else {
        "No Active Sample"
      }
    })
    
    # Affichage // Nombre de lignes dans le ZIDB choisi
    output$zidb_show_nrow <- renderText({
      if (length(zidb_files()) > 0 && req(zidb_show() != "[NONE]")) {
        paste0(zidb_df_nrow(), " items in the sample")
      } else {
        NULL
      }
    })
    
    # Training Sets -----------------------------------------------------------
    
    # Affichage // Training set choisi pour visualisation
    output$ts_sel_show <- renderText({
      if (req(ts_sel()) != "[NONE]") {
        ts_sel()
      } else {
        "No Active Training Set"
      }
    })
    
    # Affiche le nombre de vignettes classées par rapport au nombre total du training set.
    # Si tout est en ordre, soit si un training set est sélectionné
    output$ts_prog_show <- renderText({
      if (data_folder_path() != "" && req(ts_sel()) != "[NONE]") {
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
        NULL
      }
    })
    
    # Models ------------------------------------------------------------------
    
    # Affichage // Classifieur actif
    output$mod_act_clas <- renderText({
      data_folder_path()
      if (!is.null(mod_clas_name())) {
        mod_clas_name()
      } else {
        "No Active Classifier"
      }
    })
    
    # Affichage // Error Rate du Classif
    output$mod_er_ra <- renderText({
      data_folder_path()
      if (!is.null(mod_err_rate_in_class())) {
        mod_err_rate_in_class()
      } else {
        NULL
      }
    })
    
    # Results -----------------------------------------------------------------
    
    # Affichage // Nom du scrip utilisé pour résultats
    output$res_calc_name <- renderText({
      if (!is.null(calc_name())) {
        calc_name()
      } else {
        "No Active Configuration"
      }
    })
    
    # Affichage // Nombre d'échantillons dans les résultats
    output$res_nb_smp <- renderText({
      if (!is.null(nb_smp_in_res())) {
        nb_smp_in_res()
      }
    })
    
    # Communication -----------------------------------------------------------
    
    
  })
}

## To be copied in the UI
# mod_fixed_pannel_ui("fixed_pannel_ui_1")

## To be copied in the server
# mod_fixed_pannel_server("fixed_pannel_ui_1")
