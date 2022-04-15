#' page_samples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_samples_ui <- function(id){
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

      tabPanel("Sample Visualisation",
        
        tags$br(),
        navlistPanel(widths = c(2,10),
        
        tabPanel("Dataframe",
          # Affichage dataframe de l'échantillon
          tags$h3("Dataframe of the Sample"),
          dataTableOutput(ns("zidb_datatable")),
        ),
        
        tabPanel("Metadata",
          # Affichage metadata de l'échantillon
          tags$h3("Metadata of the Sample"),
          verbatimTextOutput(ns("zidb_metadata")),
        ),
        
        tabPanel("Summary",
          # Affichage du summary de l'échantillon
          tags$h3("Summary of the Sample"),
          verbatimTextOutput(ns("zidb_summary")),
        ),
        
        tabPanel("Plot",
          # Affichage d'un graphique de l'échantillon
          tags$h3("Plot of the Sample"),
          selectInput(ns("zidb_plot_x"), NULL, choices = NULL),
          selectInput(ns("zidb_plot_y"), NULL, choices = NULL),
          plotOutput(ns("zidb_plot"))
        ),
      
        ),
      ),

# Vignettes Visualisation UI ----------------------------------------------

      tabPanel("Vignettes Visualisation",
        
        # Affichage du ZIDB choisi
        tags$br(),
        tags$h3("Sample (zidb)file selected :"),
        textOutput(ns("zidb_selected")),
        
        # Affichage des vignettes
        tags$br(),
        selectInput(ns("zidb_vign_vis"), "Vignettes to watch", choices = "None"),
        tags$h3("Visualisation of vignettes"),
        plotOutput(ns("zidb_vignettes_plot")),
      )
    )
  )
}
    
#' page_samples Server Functions
#'
#' @noRd 
mod_page_samples_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Préparation des variables qui viennent de settings
    data_folder_path_rea <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path })
    smps <- reactive({ all_vars$settings_vars$smps })
    
    # Préparation des variables qui viennent de fixed_pannel
    zidb_show <- reactive({ all_vars$fixed_pannel_vars$zidb_show })

# ZIDB Preparation Server --------------------------------------------------------

    # Variable : smpfiles(pour avoir la version de cette page, qui s'actualise)
    smpfiles <- reactive({
      # Mise à jour si création de zidb
      input$zidb_make
      input$zidb_make_all
      
      # Si le folder_path est non vide
      list.files(Samples_folder_path())
    })
    
    # Mise à jour du sélecteur d'échantillons
    observeEvent(smps(), {
      updateSelectInput(session, "sel_sample_folder", "Sample folder :",choices = smps())
    })
    
    # Variable : pour le chemin du Sample choisi
    sample_selected_path <- reactive({
      fs::path(data_folder_path_rea(),"Samples",input$sel_sample_folder)
    })
    
    sample_selected_content <- reactive ({
      list.files(sample_selected_path())
    })
    
    # Affichage // Contenu de l'échantillon
    output$sel_samp_cont <- renderPrint({
      # Que si l'échantillon existe et n'est pas vide
      if (length(sample_selected_content()) > 0) {
        sample_selected_content()
      } else {
        "Folder Empty"
      }
    })
    
    # Création d'un ZIDB sélectionné
    observeEvent(input$zidb_make, {
      # Que si l'échantillon existe et n'est pas vide
      if (length(sample_selected_content()) > 0) {
        zidbMake(sample_selected_path())
      }
    })
    
    # Création de tous les ZIDB
    observeEvent(input$zidb_make_all, {
      # Que si l'échantillon existe et n'est pas vide
      if (length(sample_selected_content()) > 0) {
        zidbMakeAll(Samples_folder_path(), delete.source = FALSE, replace = TRUE)
      }
    })
    
    # Variable : qui répertorie les fichiers ZIDB dans le dossier "Samples"
    zidb_files <- reactive({
      smpfiles()[grepl(".zidb",smpfiles())] # On regarde dans tous les fichiers/dossier,
                                            # et on prend ceux qui ont l'extension .zidb
    })
    
    # Affichage // ZIDB existants
    output$zidb_existing <- renderText({
      if (length(zidb_files()) > 0) {
        zidb_files()
      } else {
        "No ZIDB file yet"
      }
    })

# ZIDB Visualisation Server -----------------------------------------------
    
    # Variable : du chemin vers le ZIDB choisi
    zidb_selected_path <- reactive({
      fs::path(data_folder_path_rea(),"Samples",zidb_show())
    })
    
    # Variable : du dataframe du ZIDB choisi
    zidb_df <- reactive({
      req(zidb_show(), data_folder_path_rea())
      zidbDatRead(zidb_selected_path())
    })
    
    zidb_df_nrow <- reactive({
      nrow(zidb_df())
    })
    
    # Affichage // head du ZIDB choisi
    output$zidb_datatable <- renderDataTable({
      zidb_df()
    })
    
    # Affichage // metadata du ZIDB choisi
    output$zidb_metadata <- renderPrint({
      attr(zidb_df(), "metadata")
    })
    
    # Affichage // summary du ZIDB choisi
    output$zidb_summary <- renderPrint({
      summary(zidb_df())
    })
    
    # Mise à jour du choix de la variable en x et y
    observe({
      data_folder_path_rea()
      updateSelectInput(session, "zidb_plot_x", "X :", choices = "")
      updateSelectInput(session, "zidb_plot_y", "Y :", choices = "")
      if (!is.null(req(zidb_df()))) {
        choices = names(zidb_df())
        updateSelectInput(session, "zidb_plot_x", "X :", choices = choices)
        updateSelectInput(session, "zidb_plot_y", "Y :", choices = choices)
      }
    })
    
    # Affichage // plot exemple du ZIDB choisi
    output$zidb_plot <- renderPlot({
      req(zidb_df(), input$zidb_plot_x, input$zidb_plot_y)
      if (input$zidb_plot_x != "" && input$zidb_plot_y != "") {
        x <- zidb_df()[,input$zidb_plot_x]
        y <- zidb_df()[,input$zidb_plot_y]
        try(plot(x, y, xlab = input$zidb_plot_x, ylab = input$zidb_plot_y), silent = TRUE)
      }
    })

# Vignettes Visualisation Server ------------------------------------------

    # Affichage // ZIDB sélectionné
    output$zidb_selected <- renderText({
      if (length(zidb_show()) > 0) {
        zidb_show()
      } else {
        "No ZIDB file yet"
      }
    })
    
    # Variable : vignettes max pour l'affichage
    zidb_nb_vign_max <- reactive({
      # Variable : dataframe pour accès simple
      dataframe_vign <- zidbDatRead(zidb_selected_path())
      return( max( dataframe_vign["Item"] ))
    })
    
    # Mise à jour du sélecteur de vignettes
    observe({
      req(zidb_show(), data_folder_path_rea())
      dataframe_vign <- zidbDatRead(zidb_selected_path())
      zidb_nb_vign_max <- max(dataframe_vign["Item"])
      
      # vignettes de 1-25 ou 26-50, ... 1 -> borne inf / 25 -> borne sup
      # Variable : borne supérieure 
      upper_limit <- (1:ceiling(zidb_nb_vign_max/25))*25
      
      # Variable : borne inférieur
      lower_limit <- upper_limit - 24
      # Mise à niveau de la dernière borne supérieure, pour qu'elle corresponde au max réel
      upper_limit[length(upper_limit)] <- zidb_nb_vign_max
      
      updateSelectInput( session, "zidb_vign_vis", "Vignettes to watch",
                         choices = paste0( lower_limit, " - ", upper_limit ))
    })
    
    # Variable : Chargement du ZIDB choisi
    zidb_loaded <- reactive({
      req(zidb_show(), data_folder_path_rea)
      zidbLink(zidb_selected_path())
    })
    
    # Variable : Noms des images
    zidb_vignettes <- reactive({
      ls( zidb_loaded() )[ !grepl( "_dat1", ls( zidb_loaded() )) ]
    })
    
    # Variable : Récupération des bornes inférieures et supérieures pour l'affichage
    zidb_vignettes_nb <- reactive({
      
      splitted <- strsplit( input$zidb_vign_vis, " - ")
      
      # Récupération de la limite supérieure et inférieure de vignettes souhaitées
      c( as.numeric( splitted[[1]][1] ), as.numeric( splitted[[1]][2] ) )
    })
    
    # Affichage // plot des vignettes
    output$zidb_vignettes_plot <- renderPlot({
      
      req( zidb_show(), zidb_vignettes_nb()) # Besoin de zidb_show et zidb_vignettes_nb pour se faire
      
      from_image_nb <- zidb_vignettes_nb()[1] # Borne inf
      to_image_nb <- zidb_vignettes_nb()[2] # Borne sup
      
      zidbPlotNew("Vignettes") # Création du plot
      
      for (i in from_image_nb:to_image_nb) # les images num i dans l'intervalle choisie
        zidbDrawVignette( zidb_loaded()[[zidb_vignettes()[i]]],
                          item = i - (from_image_nb - 1), nx = 5, ny = 5)
      # A la position i moins le décalage par rapport à 1 (position dans le plot)
      # ainsi que nb d'éléments par lignes et colonnes
    })
    
# Communication -----------------------------------------------------------
    
    # Variables : à faire sortir (Pour affichage dans le panneau fixe et choix du ZIDB) :
    zidb_vars <- reactiveValues(
      zidb_files = NULL,
      zidb_df_nrow = NULL,
    )
    
    # Mise à jour des variables du paquet
    observe({
      zidb_vars$zidb_files <- zidb_files()
      zidb_vars$zidb_df_nrow <- zidb_df_nrow()
    })
    
    # Envoi du paquet qui contient toutes les variables
    return(zidb_vars)

  })
}
    
## To be copied in the UI
# mod_page_samples_ui("page_samples_ui_1")
    
## To be copied in the server
# mod_page_samples_server("page_samples_ui_1")
