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
            selectInput("sample_folder", "Sample folder :",
                choices = "!!!To Change!!!"),
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
            "Main Panel"
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
mod_Samples_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Samples_main_ui("Samples_main_ui_1")
    
## To be copied in the server
# mod_Samples_main_server("Samples_main_ui_1")
