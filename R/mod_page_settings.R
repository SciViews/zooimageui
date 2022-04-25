#' page_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # ===== Si data_folder_path défini :
    conditionalPanel(
      # --- Tests ---
      # condition = paste0("output['",ns("affichage"),"'] == true"), 
      # Pas utile écrire ainsi grâce au ns
      # -------------
      
      condition = "output['is_folder_defined'] == true",
      
      sidebarLayout(
        
        # Dossier de data défini, on le montre :
        sidebarPanel(id = "dsf_sidebar",
          tags$h4("Data storage folder"),
          textOutput(ns("data_folder_path_show")),
          tags$br(),
          
          # Si on veut changer le dossier :
          actionButton(ns("rm_data_folder_path"), "Change data folder"),
        ),
        
        # Montre le contenu
        mainPanel(
          tags$h4("Data folder content"),
          verbatimTextOutput(ns("data_folder_content"))
        )
      ),
      
      # Affichage du contenu du dossier Samples si il existe
      tags$hr(),
      tags$br(),
      tags$h4("Content of your \"Samples\" folder"),
      verbatimTextOutput(ns("Samples_show")),
      
      # Affichage du contenu du dossier Samples si il existe
      tags$br(),
      tags$h4("Content of your \"Training_Sets\" folder"),
      verbatimTextOutput(ns("ts_show")),
      
      # Affichage du contenu du dossier Samples si il existe
      tags$br(),
      tags$h4("Content of your \"Models\" folder"),
      verbatimTextOutput(ns("mod_show")),
      
      # Affichage du contenu du dossier Samples si il existe
      tags$br(),
      tags$h4("Content of your \"Results\" folder"),
      verbatimTextOutput(ns("res_show")),
      
      ns = ns,
    ),
    
    # ===== Si data_folder_path non défini :
    conditionalPanel(
      condition = "output['is_folder_defined'] == false",
      
      sidebarLayout(
        
        # Dossier de data à choisir :
        sidebarPanel(id = "dsf_sidebar",
          tags$h4("Data storage folder"),
          textInput(ns("new_data_folder_path"), "", value = "~/"),
          tags$br(),
          
          # Enregistrer le new_data_folder_path dans data_folder_path_rea()
          actionButton(ns("set_new_data_folder_path"), "Set new path"),
          actionButton(ns("get_server_folder_back"), "Cancel"),
          tags$br(),
        ),
        
        # Affichage du contenu du dossier choisi en direct.
        mainPanel(
          tags$h4("Folder content"),
          verbatimTextOutput(ns("choosing_folder_content")),
        )
      ),
      
      ns = ns,
    )
  )
}
    
#' page_settings Server Functions
#'
#' @noRd 
mod_page_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
    # --- Tests ---
    # output$affichage <- reactive({
    # FALSE
    # })
    # outputOptions( output, "affichage", suspendWhenHidden = FALSE )
    # -------------
    
    # ===== GENERAL =====
    # Création d'une variable réactive qui contient le nom du "data_folder"
    data_folder_path_rea <- reactiveVal({
      getOption("data_folder_path")
    })
    
    # Création d'une variable pour tester si on a un dossier de data défini
    output$is_folder_defined <- reactive({
      data_folder_path_rea() != ""
    })
    # Chargement de la variable par le browser (Dynamic UI)
    outputOptions( output, "is_folder_defined", suspendWhenHidden = FALSE )
    
    
    # ===== PREMIER PANNEAU CONDITIONNEL : si le folder path existe =====
    # Création de l'output affichant le chemin du dossier de data
    output$data_folder_path_show <- renderText({
      data_folder_path_rea()
    })
    
    old_path <- ""
    
    # Si on appuie sur le bouton "Change data folder" : On efface le chemin
    observeEvent(input$rm_data_folder_path, {
      
      old_path <<- isolate(data_folder_path_rea()) # Sauvegarde de l'ancien chemin
      
      data_folder_path_rea("") # change la var en "" et fait réagir le reste
      print(data_folder_path_rea()) # pour imprimer dans la console le résultat (contrôle)
    })
    
    # Montrer le contenu du data_folder
    output$data_folder_content <- renderPrint({
      
      # si le chemin est différent de "" alors le montre
      if ( length(list.files(data_folder_path_rea())) > 0) {
        noquote(list.files(data_folder_path_rea()))
        # si non, message d'erreur
      } else {
        "Error, wrong path or the folder is empty !"
      }
    })
    
    # Set up du chemin du dossier Samples
    Samples_folder_path <- reactive({
      fs::path(data_folder_path_rea(),"/Samples")
    })
    
    smpfiles <- reactive({
      list.files(Samples_folder_path())
    })
    
    smps <- reactive ({
      find_samples(smpfiles())
    })
    
    # Affichage // Contenu du dossier Samples
    output$Samples_show <- renderPrint({
      if (length(smps()) > 0) {
        smps()
      } else {
        "Folder Empty"
      }
    })
    
    # Affichage // Contenu du dossier Training_Sets
    output$ts_show <- renderPrint({
      path <- fs::path(data_folder_path_rea(),"Training_Sets")
      tss <- list.files(path)
      if (length(tss) > 0) {
        tss
      } else {
        "Folder Empty"
      }
    })
    
    # Affichage // Contenu du dossier Models
    output$mod_show <- renderPrint({
      path <- fs::path(data_folder_path_rea(),"Models")
      mods <- list.files(path)
      if (length(mods) > 0) {
        mods
      } else {
        "Folder Empty"
      }
    })
    
    # Affichage // Contenu du dossier Results
    output$res_show <- renderPrint({
      path <- fs::path(data_folder_path_rea(),"Results")
      ress <- list.files(path)
      if (length(ress) > 0) {
        ress
      } else {
        "Folder Empty"
      }
    })
    
    
    # ===== DEUXIEME PANNEAU CONDITIONNEL : si le folder path n'existe pas =====
    # Si on appuie sur le bouton "Save new path" : On sauvegarde le nouveau chemin
    observeEvent(input$set_new_data_folder_path, {
      data_folder_path_rea(input$new_data_folder_path) # change la var en le nouveau chemin et fait réagir le reste
      print(data_folder_path_rea()) # pour imprimer dans la console le résultat (contrôle)
    })
    
    # Si on veut reprendre le chemin précédant
    observeEvent(input$get_server_folder_back, {
      data_folder_path_rea(old_path)
      print(data_folder_path_rea())
    })
    
    # Affichage du contenu du dossier en cours de choix
    output$choosing_folder_content <- renderPrint({
      
      # si le chemin est différent de "" alors le montre
      if ( length(list.files(input$new_data_folder_path)) > 0 ) {
        result <- list.files(input$new_data_folder_path)
        # si non, message d'erreur
      } else {
        result <- "Error, wrong path or the folder is empty !"
      }
      
      return(result)
    })
    
# Communication -----------------------------------------------------------
    
    # Préparation des variables dans un paquet
    vars <- reactiveValues(
      data_folder_path_rea = NULL,
      Samples_folder_path = NULL,
      smpfiles = NULL,
      smps = NULL,
    )
    
    # Mise à jour des variables dans le paquet
    observe({
      vars$data_folder_path_rea <- data_folder_path_rea()
      vars$Samples_folder_path <- Samples_folder_path()
      vars$smpfiles <- smpfiles()
      vars$smps <- smps()
    })
    
    # Envoi du packet qui contient toutes les variables
    return(vars)
  })
}
    
## To be copied in the UI
# mod_page_settings_ui("page_settings_ui_1")
    
## To be copied in the server
# mod_page_settings_server("page_settings_ui_1")
