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
    conditionalPanel(
      # --- Tests ---
      # condition = paste0("output['",ns("affichage"),"'] == true"), 
      # Pas utile écrire ainsi grâce au ns
      # -------------
      
      condition = "output['is_folder_defined'] == true",
      
      # Dossier de data défini, on le montre :
      tags$h2("Data storage folder :"),
      textOutput(ns("data_folder_path_show")),
      tags$br(),
      
      # Si on veut changer le dossier :
      actionButton(ns("rm_data_folder_path"), "Change data folder"),
      
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
      
    
      # ===== PREMIER PANNEAU CONDITIONNEL : si le folder path existe =====
      # Création d'une variable pour tester si on a un dossier de data défini
      data_folder_path_rea <- reactiveVal({
        data_folder_path
      })
    
      output$is_folder_defined <- reactive({
        data_folder_path_rea() != ""
      })
      # Chargement de la variable par le browser (Dynamic UI)
      outputOptions( output, "is_folder_defined", suspendWhenHidden = FALSE )
      
      # Création de l'output affichant le chemin du dossier de data
      output$data_folder_path_show <- renderText({
        data_folder_path_rea()
      })
      
      # Si on appuie sur le bouton "Change data folder" : On efface le chemin
      observeEvent(input$rm_data_folder_path, {
        print("Bip")
        data_folder_path_rea("")
      })
  })
}
    
## To be copied in the UI
# mod_Data_Folder_bis_ui("Data_Folder_bis_ui_1")
    
## To be copied in the server
# mod_Data_Folder_bis_server("Data_Folder_bis_ui_1")
