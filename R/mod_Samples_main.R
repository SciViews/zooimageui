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
        sidebarLayout(
          
          # Choix de l'échantillon pour la préparation des ZIDB
          sidebarPanel(
            selectInput(ns("sample_folder"), "Sample folder :",
                choices = "No Samples"),
            actionButton("zidbmake", "Make the ZIDB file"),
            tags$br(),
            tags$br(),
            
            actionButton("zidbmakeall", "Make ZIDB file for all of the samples"),
            tags$br(),
            tags$br(),
            
            tags$h5(style = "font-weight: bold;" ,"Already formated :"),
            textOutput("zidb_made"),
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
    data_folder_path <- reactive({ settings_vars$data_folder_path_rea })
    smpfiles <- reactive({ settings_vars$smpfiles })
    smps <- reactive({ settings_vars$smps })


# ZIDB Preparation --------------------------------------------------------

    # Mise à jour du sélecteur d'échantillons
    observeEvent(smps(), {
      updateSelectInput(session, "sample_folder", "Sample folder :",choices = smps())
    })
    
    # Attention, ça marche, mais il faut que je modifie des éléments du CSS qui perturbent le verbatimOutput
    output$sel_samp_cont <- renderPrint({
      list.files(fs::path(data_folder_path(),"Samples",input$sample_folder))
    })
    
  })
}
    
## To be copied in the UI
# mod_Samples_main_ui("Samples_main_ui_1")
    
## To be copied in the server
# mod_Samples_main_server("Samples_main_ui_1")
