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

# ZIDB Preparation UI -----------------------------------------------------

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

# ZIDB Visualisation UI ---------------------------------------------------

      tabPanel("ZIDB Visualisation",
        
        tags$h3("Head of the ZIDB's dataframe"),
        tableOutput(ns("zidb_head")),
         
        tags$hr(),
        tags$h3("Metadata of the ZIDB"),
        verbatimTextOutput(ns("zidb_metadata")),
         
        tags$hr(),
        tags$h3("Summary of the ZIDB"),
        verbatimTextOutput(ns("zidb_summary")),
         
        tags$hr(),
        tags$h3("Plot of the ZIDB"),
        plotOutput(ns("zidb_plot"))
      ),

# Vignettes Visualisation UI ----------------------------------------------

      tabPanel("Vignettes Visualisation",
        tags$br(),
        textOutput(ns("zidb_selected")),
        
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
mod_Samples_main_server <- function(id, settings_vars, fixed_pannel_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Préparation des variables qui viennent de settings
    data_folder_path_rea <- reactive({ settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ settings_vars$Samples_folder_path })
    smps <- reactive({ settings_vars$smps })
    
    zidb_show <- reactive({ fixed_pannel_vars$zidb_show })

# ZIDB Preparation Server --------------------------------------------------------

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

# ZIDB Visualisation Server -----------------------------------------------
    
    # Variable : du chemin vers le ZIDB choisi
    zidb_selected_path <- reactive({
      fs::path(data_folder_path_rea(),"Samples",zidb_show())
    })
    
    # Variable : du dataframe du ZIDB choisi
    zidb_df <- reactive({
      req(zidb_show())
      zidbDatRead(zidb_selected_path())
    })
    
    # Affichage // head du ZIDB choisi
    output$zidb_head <- renderTable({
      head(zidb_df())
    })
    
    # Affichage // metadata du ZIDB choisi
    output$zidb_metadata <- renderPrint({
      attr(zidb_df(), "metadata")
    })
    
    # Affichage // summary du ZIDB choisi
    output$zidb_summary <- renderPrint({
      summary(zidb_df())
    })
    
    # Affichage // plot exemple du ZIDB choisi
    output$zidb_plot <- renderPlot({
      plot(zidb_df()$Area, zidb_df()$Perim., xlab = "Area", ylab = "Perimeter")
    })
    
    
    # - Variables à faire sortir (Pour affichage dans le panneau fixe et choix du ZIDB) :
    zidb_vars <- reactiveValues(
      zidb_files = NULL,
    )
    
    observe({
      zidb_vars$zidb_files <- zidb_files()
    })

# Vignettes Visualisation Server ------------------------------------------

    # Vignettes

# Communication -----------------------------------------------------------

    return(zidb_vars)

  })
}
    
## To be copied in the UI
# mod_Samples_main_ui("Samples_main_ui_1")
    
## To be copied in the server
# mod_Samples_main_server("Samples_main_ui_1")
