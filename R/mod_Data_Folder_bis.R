#' Data_Folder_bis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_Folder_bis_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Page test"),
    
    # ===== Si data_folder_path défini :
    conditionalPanel(
      # --- Tests ---
      # condition = paste0("output['",ns("affichage"),"'] == true"), 
      # Pas utile écrire ainsi grâce au ns
      # -------------
      
      condition = "output['is_folder_defined'] == true",
      
      sidebarLayout(
        
        # Dossier de data défini, on le montre :
        sidebarPanel(
          tags$h2("Data storage folder :"),
          textOutput(ns("data_folder_path_show")),
          tags$br(),
          
          # Si on veut changer le dossier :
          actionButton(ns("rm_data_folder_path"), "Change data folder"),
        ),
        
        mainPanel(
          tags$h2("Data folder content"),
          verbatimTextOutput(ns("data_folder_contents"))
        )
      ),
      
      ns = ns,
    ),
    
    # ===== Si data_folder_path non défini :
    conditionalPanel(
      condition = "output['is_folder_defined'] == false",
      
      sidebarLayout(
        
        sidebarPanel(
          # Dossier de data à choisir :
          tags$h2("Data storage folder :"),
          textInput(ns("new_data_folder_path"), "Path to data folder :"),
          tags$br(),
          
          # Enregistrer le new_data_folder_path dans data_folder_path_rea()
          actionButton(ns("save_new_data_folder_path"), "Save new path"),
        ),
        
        mainPanel(
          tags$h2("Folder content"),
          verbatimTextOutput(ns("choosing_folder_content")),
        )
      ),
      
      ns = ns,
    )
  )
}
    
#' Data_Folder_bis Server Functions
#'
#' @noRd 
mod_Data_Folder_bis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      
      # --- Tests ---
      # output$affichage <- reactive({
        # FALSE
      # })
      # outputOptions( output, "affichage", suspendWhenHidden = FALSE )
      # -------------
      
      # ===== Variable globale =====
      # Création d'une variable réactive qui contient le nom du "data_folder"
      data_folder_path_rea <- reactiveVal({
        data_folder_path
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
      
      # Si on appuie sur le bouton "Change data folder" : On efface le chemin
      observeEvent(input$rm_data_folder_path, {
        data_folder_path_rea("") # change la var en "" et fait réagir le reste
        print(data_folder_path_rea()) # juste pour la voir
      })
      
      # Montrer le contenu du data_folder
      output$data_folder_contents <- renderPrint({
        list.files(data_folder_path_rea())
      })
      
      
      # ===== DEUXIEME PANNEAU CONDITIONNEL : si le folder path n'existe pas =====
      # Si on appuie sur le bouton "Save new path" : On sauvegarde le nouveau chemin
      observeEvent(input$save_new_data_folder_path, {
        data_folder_path_rea(input$new_data_folder_path) # change la var en le nouveau chemin et fait réagir le reste
        print(data_folder_path_rea())
      })
      
      # Affichage du contenu du dossier en cours de choix
      output$choosing_folder_content <- renderPrint({
        if ( length(list.files(input$new_data_folder_path)) > 0 ) {
          list.files(input$new_data_folder_path)
        } else {
          "Erreur, mauvais chemin ou dossier vide !"
        }
      })
      
      
  })
}
    
## To be copied in the UI
# mod_Data_Folder_bis_ui("Data_Folder_bis_ui_1")
    
## To be copied in the server
# mod_Data_Folder_bis_server("Data_Folder_bis_ui_1")
