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

# Calculations UI ---------------------------------------------------------

      tabPanel("Calculation of Statistics",
        tags$br(),
        sidebarLayout(
          
          sidebarPanel(
            # Montre le Sample choisi
            tags$h4("Selected Sample :"),
            textOutput(ns("calc_sel_smp")),
            # Montre le Classifieur actif
            tags$h4("Active Classifier :"),
            textOutput(ns("calc_act_clas")),
            # Indique si on peut passer à la suite
            tags$h4("Data ready ?"),
            textOutput(ns("calc_is_data_ready")),
          ),
          
          sidebarPanel(
            # Choix du script
            tags$h4("Chose Calculations :"),
            selectInput(ns("calc_selected_script"), NULL, choices = NULL),
            # Messages d'aide
            verbatimTextOutput(ns("calc_res_message")),
            textOutput(ns("calc_res_comment")),
            tags$br(),
            # Boutons pour rafraichir la liste de scripts et pour faire les calculs
            actionButton(ns("calc_refresh"), "Refresh"),
            shinyjs::disabled(actionButton(ns("calc_use_script"), "Calculate"))
          ),
        ),
      ),

# Visualisation UI --------------------------------------------------------

      tabPanel("Visualisation of Statistics"

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
    
    # Fixed Pannel Vars
    selected_zidb <- reactive({ all_vars$fixed_pannel_vars$zidb_show })
    
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
    
# Calculation Server ------------------------------------------------------
    
    # --- SidebarPanel ---
    
    # Affichage // l'échantillon sélectionné
    output$calc_sel_smp <- renderText({
      if (!is.null(selected_zidb())) {
        selected_zidb()
      } else {
        "No Selected Sample yet"
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
        res <- predict(classif, res, class.only = FALSE)
      }
    })
    
    # --- MainPanel ---
    
    # Mise à jour du sélecteur de script
    observe({
      if (length(calc_scripts_list()) > 0) {
        updateSelectInput(session, "calc_selected_script", NULL, choices = calc_scripts_list())
      } else {
        updateSelectInput(session, "calc_selected_script", NULL, choices = "No Calculations yet")
      }
    })
    
    # Test si le script est correct
    calc_is_script_good <- reactive({
      req(input$calc_selected_script, data_folder_path_rea())
      is_script_good_results(results_folder_path(), input$calc_selected_script)
    })
    
    # Affichage // Script message
    output$calc_res_message <- renderText({
      attr(calc_is_script_good, "message")
    })
    
    # Affichage // Script commentaire
    output$calc_res_comment <- renderText({
      comment(calc_results())
    })
    
    # Variable : fonction du modèle
    calc_results <- reactive({
      if (req(calc_is_script_good())) {
        # Chemin du script
        script_path <- fs::path(results_folder_path(), input$calc_selected_script)
        # Source du script pour récupérer la fonction
        source(script_path , local = TRUE)
        return(get_results)
      }
    })
    
    # Mise à jour du bouton pour faire les calculs
    observe({
      if (calc_is_script_good() && calc_are_smp_clas_correct()) {
        shinyjs::enable("calc_use_script")
      } else {
        shinyjs::disable("calc_use_script")
      }
    })
    
    # Variable : Résultat des calculs
    results <- eventReactive(input$calc_use_script, {
      req(data_folder_path_rea())
    })
    
# Visualisation Server ----------------------------------------------------
    
    
  })
}
    
## To be copied in the UI
# mod_page_results_ui("page_results_ui_1")
    
## To be copied in the server
# mod_page_results_server("page_results_ui_1")
