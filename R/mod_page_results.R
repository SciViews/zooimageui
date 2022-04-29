#' page_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

# Configurations UI ---------------------------------------------------------

      tabPanel("Configurations",
        tags$br(),
        fluidRow(
          
          sidebarPanel( width = 4,
            # Choix du script
            tags$h4("Configuration"),
            fluidRow(
              column( width = 7,
                # Choix du script
                selectInput(ns("calc_selected_script"), NULL, choices = NULL),
              ),
              column( width = 5,
                # Boutons pour rafraichir la liste de scripts et pour faire les calculs
                actionButton(ns("calc_refresh"), "Refresh"),
              ),
            ),
            # Messages d'aide
            textOutput(ns("calc_res_message")),
            uiOutput(ns("calc_comment_hr")),
            textOutput(ns("calc_res_comment")),
            tags$hr(),
            
            # Montre le Sample choisi
            tags$h4("Samples ready ?"),
            textOutput(ns("calc_smp_ok")),
            tags$br(),
            selectInput(ns("calc_smps_selector"), "Chose samples (one or more)", choices = NULL, multiple = TRUE),
            tags$hr(),
            # Montre le Classifieur actif
            tags$h4("Active Classifier ?"),
            textOutput(ns("calc_act_clas")),
            tags$hr(),
            # Indique si on peut passer à la suite
            tags$h4("Data ready ?"),
            textOutput(ns("calc_is_data_ready")),
            
            tags$hr(),
            shinyjs::disabled(actionButton(ns("calc_use_script"), "Calculate")),
            tags$br(),
            # Affichage d'un message d'erreur si le script plante
            textOutput(ns("calc_error")),
          ),
          
          mainPanel(
            # Visualisation des résultats
            tags$h4("Results"),
            uiOutput(ns("abd_tit")),
            verbatimTextOutput(ns("vis_res_abd")),
            uiOutput(ns("bio_tit")),
            verbatimTextOutput(ns("vis_res_bio")),
            uiOutput(ns("spect_tit")),
            uiOutput(ns("spect_smp_sel")),
            verbatimTextOutput(ns("vis_res_spect")),
          ),
          
        ),
      ),

# Save UI ---------------------------------------------------------------

      tabPanel("Save",
               
        # Sauvegarde des résultats
        tags$br(),
        sidebarPanel( width = 6,
          tags$h4("Save Results"),
          textInput(ns("vis_res_name"), "Name", width = "70%"),
          shinyjs::disabled(actionButton(ns("vis_res_save"), "Local Save")),
          # Téléchargement des résultats
          shinyjs::disabled(downloadButton(ns("vis_res_dl"), "Download")),
          tags$br(),
          tags$br(),
          # Messages lié à la sauvegarde et au téléchargement
          verbatimTextOutput(ns("vis_dl_name_good")),
          uiOutput(ns("vis_title")),
          textOutput(ns("vis_res_save_worked")),
        ),
      ),
      
    )
  )
}
    
#' page_results Server Functions
#'
#' @noRd 
mod_page_results_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Récupération de Variables -----------------------------------------------

    # Settings Vars
    data_folder_path_rea <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    
    # Samples Vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    smp_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path })
    
    # Models Vars
    mod_classif <- reactive({ all_vars$models_vars$modcre_classif })
    mod_clas_name <- reactive({ all_vars$models_vars$modvis_clas_name })
    
# Variables Globales ------------------------------------------------------
    
    # Variable : Chemin d'accès aux scripts pour les résultats
    results_folder_path <- reactive({
      fs::path(data_folder_path_rea(), "Results")
    })
    
    # Variable : Liste de scripts pour results
    calc_scripts_list <- reactive({
      input$calc_refresh
      list.files(results_folder_path())
    })
    
# Configuration Server ------------------------------------------------------
    
    # --- sidePanel ---
    
    # Variable : Base sample choisi
    selected_zidb <- reactive({
      if (length(zidb_files()) > 0) {
        # On prend d'office le premier
        zidb_files()[1]
      }
    })
    
    # Affichage // Samples ready ?
    output$calc_smp_ok <- renderText({
      if (!is.null(selected_zidb())) {
        paste0("Samples Ready (",length(zidb_files()),")")
      } else {
        "No Samples yet"
      }
    })
    
    # Mise à jour du sélecteur d'échantillons pour le processSampleAll
    observe({
      data_folder_path_rea()
      if (length(zidb_files()) > 0) {
        updateSelectInput(session, "calc_smps_selector", "Chose samples (one or more)", choices = c("All", sub("\\.zidb$", "", zidb_files())), selected = "All")
      } else {
        updateSelectInput(session, "calc_smps_selector", "Chose samples (one or more)", choices = NULL)
      }
    })
    
    # Variable : Sélection précise des échantillons pour le processSampleAll
    multiple_samples <- reactive({
      req(data_folder_path_rea())
      if (!is.null(input$calc_smps_selector) && !("All" %in% input$calc_smps_selector)) {
        paste0(input$calc_smps_selector,".zidb")
      } else {
        NULL
      }
    })
    
    # Affichage // classifieur actif
    output$calc_act_clas <- renderText({
      if (!is.null(mod_clas_name())) {
        mod_clas_name()
      } else {
        "No active Classifier yet"
      }
    })
    
    # Variable : Test du sample et du classifieur
    calc_are_smp_clas_correct <- reactive({
      # Classifieur actif ?
      if (is.null(mod_classif())) {
        res <- FALSE
        attr(res, "message") <- "Not Ready for Calculations"
        return(res)
      # Sample sélectionné ?
      } else if (is.null(selected_zidb())) {
        res <- FALSE
        attr(res, "message") <- "Not Ready for Calculations"
        return(res)
      # Ok
      } else {
        res <- TRUE
        attr(res, "message") <- "Ready for Calculations"
        return(res)
      }
    })
    
    # Affichage // Données prête à être utilisées
    output$calc_is_data_ready <- renderText({
      attr(calc_are_smp_clas_correct(), "message")
    })
      
    # Variable : données du zidb sélectionné (si sélectionné)
    calc_dat <- reactive({
      if (req(calc_are_smp_clas_correct())) {
        # Chemin d'accès au sample
        smp_path <- fs::path(data_folder_path_rea(), "Samples", selected_zidb())
        
        # Lecture du sample
        res <- zidbDatRead(smp_path)
        # Ajout d'une colonne prédiction par rapport au classifieur
        res <- predict(mod_classif(), res, class.only = FALSE)
      }
    })
    
    # Mise à jour du sélecteur de script
    observe({
      if (length(calc_scripts_list()) > 0) {
        updateSelectInput(session, "calc_selected_script", NULL, choices = c("[NONE]", sub("\\.R$", "", calc_scripts_list()[grepl("\\.R$", calc_scripts_list())])), selected = "[NONE]")
      } else {
        updateSelectInput(session, "calc_selected_script", NULL, choices = "[NONE]")
      }
    })
    
    # Test si le script est correct
    calc_is_script_good <- reactive({
      req(input$calc_selected_script, data_folder_path_rea())
      is_script_good_results(results_folder_path(), paste0(input$calc_selected_script, ".R"))
    })
    
    # Affichage // Script message
    output$calc_res_message <- renderText({
      attr(calc_is_script_good(), "message")
    })
    
    # Affichage // barre qui va avec le commentaire
    output$calc_comment_hr <- renderUI({
      if (!is.null(comment(calc_results()))) {
        tags$hr()
      }
    })
    
    # Affichage // Script commentaire
    output$calc_res_comment <- renderText({
      paste0("Description : ",comment(calc_results()))
    })
    
    # Variable : fonction du modèle
    calc_results <- reactive({
      if (req(calc_is_script_good())) {
        # Chemin du script
        script_path <- fs::path(results_folder_path(), paste0(input$calc_selected_script, ".R"))
        # Source du script pour récupérer la fonction
        source(script_path , local = TRUE)
        return(get_results)
      }
    })
    
    # Mise à jour du bouton pour faire les calculs
    observe({
      res_made()
      if (calc_is_script_good() && calc_are_smp_clas_correct()) {
        shinyjs::enable("calc_use_script")
      } else {
        shinyjs::disable("calc_use_script")
      }
    })
    
    # --- Global ---
    
    # Variable : Réactiver le bouton
    res_made <- reactiveVal(0)
    
    # Variable : Résultat des calculs
    results <- eventReactive({
      input$calc_use_script
      data_folder_path_rea()
      }, {
      
      if (data_folder_path_rea() == "") {
        res <- "Error : No Working Directory set. Please set it in settings"
        class(res) <- "try-error"
        res_made(res_made()+1)
        return(res)
      }
      
      req(calc_dat())
      
      # Désactivation du bouton
      shinyjs::disable("calc_use_script")
      res <- try(calc_results()(calc_dat(), smp_folder_path(), mod_classif(), multiple_samples()), silent = TRUE)
      if (inherits(res, "try-error")) {
        res <- paste0("Error in Calculation script : ", attr(res, "condition"))
        class(res) <- "try-error"
        # Actualisation du bouton
        res_made(res_made()+1)
        return(res)
      } else {
        # Actualisation du bouton
        res_made(res_made()+1)
        return(res)
      }
    })
    is_results_error <- reactive({
      inherits(results(), "try-error")
    })
    
    # Affichage // Erreur éventuelle lors de l'utilisation du script
    output$calc_error <- renderText({
      if (is_results_error()) {
        results()
      } else {
        NULL
      }
    })
    
    # Variable : Nom du script choisi
    calc_name <- eventReactive({
      input$calc_use_script
      data_folder_path_rea()
      }, {
      if (!is.null(results()) && !is_results_error()) {
        input$calc_selected_script
      } else {
        "No Configurations yet"
      }
    })
    
    # Variable : Nombre d'échantillons dans les résultats :
    nb_smp_in_res <- eventReactive(results(), {
      if (!is.null(results()) && !is_results_error()) {
        paste0("Calculated Samples : ",length(attr(results(), "row.names")))
      } else {
        NULL
      }
    })
    
    # --- Visualisation (mainPanel) ---
    
    # Affichage // Titre Abondance
    output$abd_tit <- renderUI({
      if (!is_results_error()) {
        nm <- names(results())
        if (length(nm[grepl("^Abd ", nm)]) > 0) {
          tagList(
            tags$h5("Abundance"),
          )
        }
      }
    })
    
    # Affichage // Abondance
    output$vis_res_abd <- renderPrint({
      if (!is_results_error()) {
        nm <- names(results())
        nm_abd <- nm[grepl("^Abd ", nm)]
        if (length(nm_abd) > 0) {
          return(results()[,c("Id", nm_abd)])
        }
      }
    })
    
    # Affichage // Titre Biomass
    output$bio_tit <- renderUI({
      if (!is_results_error()) {
        nm <- names(results())
        nm_bio <- nm[grepl("^Bio ", nm)]
        if (length(nm_bio) > 0) {
          tagList(
            tags$h5("Biomass"),
          )
        }
      }
    })
    
    # Affichage // Biomass
    output$vis_res_bio <- renderPrint({
      if (!is_results_error()) {
        nm <- names(results())
        nm_bio <- nm[grepl("^Bio ", nm)]
        if (length(nm_bio) > 0) {
          return(results()[,c("Id", nm_bio)])
        }
      }
    })
    
    # Affichage // Titre Spectre de tailles
    output$spect_tit <- renderUI({
      if (!is_results_error()) {
        if ("spectrum" %in% names(attributes(results()))) {
          tagList(
            tags$h5("Size Spectrum"),
          )
        }
      }
    })
    
    # Affichage // Sélection de l'échantillon à afficher
    output$spect_smp_sel <- renderUI({
      if (!is_results_error()) {
        if ("spectrum" %in% names(attributes(results()))) {
          tagList(
            selectInput(ns("spe_sel_smp"), NULL, choices = results()$Id),
          )
        }
      }
    })
    
    # Affichage // Spectre de tailles
    output$vis_res_spect <- renderPrint({
      if (!is_results_error()) {
        if ("spectrum" %in% names(attributes(results()))) {
          spec <- attr(results(), "spectrum")
          return(spec[[req(input$spe_sel_smp)]])
        }
      }
    })
    
# Save Server -------------------------------------------------------------
    
    # Variable : Est-ce que le nom pour sauvegarder est correct ?
    vis_is_name_good <- reactive({
      # Enlèvement des caractères spéciaux
      name <- stringr::str_replace_all( input$vis_res_name, "[^[:alnum:]]", "")
      # Test que le nom ne soit pas vide
      if (name == "") {
        res <- FALSE
        attr(res, "error") <- "Name is empty or only contains specials characters."
        return(res)
      } else {
        res <- TRUE
        attr(res, "error") <- "Name is correct."
        return(res)
      }
    })
    
    # Affichage // Est-ce que le nom est correct
    output$vis_dl_name_good <- renderText({
      attr(vis_is_name_good(), "error")
    })
    
    # Mise à jour du bouton pour enregistrer le résultat
    observe({
      if (!is.null(results()) && !is_results_error() && vis_is_name_good()) {
        shinyjs::enable("vis_res_save")
      } else {
        shinyjs::disable("vis_res_save")
      }
    })
    
    # Variable : Est-ce que la sauvegarde a fonctionné ?
    vis_save <- eventReactive(input$vis_res_save, {
      res_save(req(data_folder_path_rea()), req(input$vis_res_name), req(results()))
    })
    
    # Affichage // Titre est-ce que ça a fonctionné
    output$vis_title <- renderUI({
      vis_save()
      return(tags$h5("Save Worked :"))
    })
    
    # Affichage // Est-ce que la sauvegarde a fonctionné ?
    output$vis_res_save_worked <- renderText({
      if (!vis_save()) {
        attr(vis_save(), "error")
      } else {
        "Saved !"
      }
    })
    
    # Mise à jour du bouton pour télécharger le résultat
    observe({
      if (!is.null(results()) && !is_results_error() && vis_is_name_good()) {
        shinyjs::enable("vis_res_dl")
      } else {
        shinyjs::disable("vis_res_dl")
      }
    })
    
    # Mise au point du téléchargement des résultats
    output$vis_res_dl <- downloadHandler(
      filename = function() {
        # Le fichier s'appellera :
        paste(stringr::str_replace_all( input$vis_res_name, "[^[:alnum:]]", ""), ".zip", sep = "")
      },
      # Contenu du fichier
      content = function(file) {
        # fonction qui crée le zip des résultats
        res_dl(data_folder_path_rea(),input$vis_res_name, results(), file = file)
      }
    )

# Communication -----------------------------------------------------------
    
    # Préparation des variables dans un paquet
    results_vars <- reactiveValues(
      calc_name = NULL,
      nb_smp_in_res = NULL,
    )
    
    # Mise à jour des variables dans le paquet
    observe({
      results_vars$calc_name <- calc_name()
      results_vars$nb_smp_in_res <- nb_smp_in_res()
    })
    
    # Envoi du packet qui contient toutes les variables
    return(results_vars)
    
  })
}
    
## To be copied in the UI
# mod_page_results_ui("page_results_ui_1")
    
## To be copied in the server
# mod_page_results_server("page_results_ui_1")
