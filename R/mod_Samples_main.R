#' Samples_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Samples_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      
      tabPanel("ZIDB Preparation",
        tags$br(),
        sidebarLayout(
          
          # Choix de l'échantillon pour la préparation des ZIDB
          sidebarPanel(
            
            # Choix du Sample
            selectInput(ns("sel_sample_folder"), "Sample folder :",
                choices = "No Samples"),
            
            # Création ZIDB unique
            actionButton(ns("zidb_make"), "Make the ZIDB file"),
            tags$br(),
            tags$br(),
            
            # Création tous les ZIDB
            actionButton(ns("zidb_make_all"), "Make ZIDB file for all of the samples"),
            tags$br(),
            tags$br(),
            
            # Montrer les ZIDB
            tags$h5("Already formated :"),
            textOutput(ns("zidb_existing")),
          ),
          
          mainPanel(
            h3("Selected Sample's content :"),
            verbatimTextOutput(ns("sel_samp_cont")),
          ),
        ),
      ),
      
      tabPanel("ZIDB Visualisation",
        tags$br(),
        selectInput("zidb_to_show", "Select a ZIDB file to show a preview :",
            choices = "!!!To Change!!!"),
         
        tags$hr(),
        tags$h3("Head of the ZIDB's dataframe"),
        tableOutput("sample_head"),
         
        tags$hr(),
        tags$h3("Metadata of the ZIDB's dataframe"),
        verbatimTextOutput("sample_attributes"),
         
        tags$hr(),
        tags$h3("Summary of some of the columns of the ZIDB's dataframe"),
        verbatimTextOutput("sample_summary"),
         
        tags$hr(),
        tags$h3("Test plot of the ZIDB's dataframe"),
        plotOutput("sample_test_plot")
      ),
          
      tabPanel("Vignettes Visualisation",
        tags$br(),
        selectInput("vignettes_file", "ZIDB file to visualize",
            choices = "!!!To Change!!!"),
         
        selectInput("vignettes_vis", "Vignettes to watch", choices = "None"),
        tags$h3("Visualisation of vignettes"),
        plotOutput("vignettes_plot")
      )
    )
  )
}
    
#' Samples_main Server Functions
#'
#' @noRd 
mod_Samples_main_server <- function(id, settings_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Préparation des variables qui viennent de settings
    data_folder_path_rea <- reactive({ settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ settings_vars$Samples_folder_path })
    smps <- reactive({ settings_vars$smps })


# ZIDB Preparation --------------------------------------------------------

    # Variable : smpfiles(pour avoir la version de cette page, qui s'actualise)
    smpfiles <- reactive({
      # Mise à jour si création de zidb
      input$zidb_make
      input$zidb_make_all
      
      # Si le folder_path est non vide
      if ( Samples_folder_path() != "" ) {
        list.files(Samples_folder_path())
      }
    })
    
    # Mise à jour du sélecteur d'échantillons
    observeEvent(smps(), {
      updateSelectInput(session, "sel_sample_folder", "Sample folder :",choices = smps())
    })
    
    # Variable : pour le chemin du Sample choisi
    sample_selected_path <- reactive({
      fs::path(data_folder_path_rea(),"Samples",input$sel_sample_folder)
    })
    
    # Affichage // Contenu de l'échantillon
    output$sel_samp_cont <- renderPrint({
      list.files(sample_selected_path())
    })
    
    # Création d'un ZIDB sélectionné
    observeEvent(input$zidb_make, {
      zidbMake(sample_selected_path())
    })
    
    # Création de tous les ZIDB
    observeEvent(input$zidb_make_all, {
      zidbMakeAll(Samples_folder_path(), delete.source = FALSE, replace = TRUE)
    })
    
    # Variable : qui répertorie les fichiers ZIDB dans le dossier "Samples"
    zidb_files <- reactive({
      smpfiles()[grepl(".zidb",smpfiles())] # On regarde dans tous les fichiers/dossier,
                                            # et on prend ceux qui ont l'extension .zidb
    })
    
    # Affichage // ZIDB existants
    output$zidb_existing <- renderText({
      zidb_files()
    })
  })
}
    
## To be copied in the UI
# mod_Samples_main_ui("Samples_main_ui_1")
    
## To be copied in the server
# mod_Samples_main_server("Samples_main_ui_1")
