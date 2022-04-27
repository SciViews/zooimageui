#' page_contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$br(),
    fluidRow(
      sidebarPanel(width = 6,
                   
        tags$h4("Creator of the App"),
        tags$p("Lecocq Martin"),
        tags$h4("Email Address"),
        tags$p("martin.lecocq@std.heh.be"),
        tags$h4("School & Study"),
        tags$p("Haute École en Hainaut : Biotechnologie (option Bioinformatique)"),
        tags$h4("Context"),
        tags$p("Internship at University of Mons (UMONS) - Faculty of Science - Unit of NumEco"),
        tags$h4("Supervisor"),
        tags$p("Prof. Philippe Grosjean"),
        tags$p("philippe.grosjean@umons.ac.be"),
      ),
      
      sidebarPanel(width = 6,
        tags$h4("ZooImageUI, interface of ZooImage"),
        tags$div(id = "explanation",
               "This web interface was made to provide a visual and interactive
               interface to the program ZooImage. With this application, any
               biologist that wants to make research about plankton using the 
               ZooImage program can do it without having to learn R language. With
               this simple interface, it's easy to understand how to perform the
               different steps of the process of ZooImage.")
      ),
    ),
  )
}
    
#' page_contact Server Functions
#'
#' @noRd 
mod_page_contact_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_contact_ui("page_contact_ui_1")
    
## To be copied in the server
# mod_page_contact_server("page_contact_ui_1")
