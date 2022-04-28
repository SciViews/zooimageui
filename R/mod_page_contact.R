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
      sidebarPanel(width = 4,
        
        tags$h4("Company"),
        tags$p("University of Mons (UMONS)"),
        tags$p("Faculty of Science"),
        tags$p("Department of Numerical Ecology"),
        tags$h4("Websites"),
        tags$a(href = "https://web.umons.ac.be/econ/en/home/" , "UMONS - Numerical Ecology"),
        tags$br(),
        tags$a(href = "https://www.sciviews.org/", "SciViews"),
        tags$h4("Head of the Department"),
        tags$p("Prof. Philippe Grosjean"),
        tags$p("philippe.grosjean@umons.ac.be"),
        tags$h4("Creator of the App"),
        tags$p("Lecocq Martin"),
        tags$p("martin.lecocq@std.heh.be"),
      ),
      
      sidebarPanel(width = 8,
        tags$h4("ZooImageUI, interface of ZooImage"),
        tags$div(id = "explanation",
               "This web interface was made to provide a visual and interactive
               interface to the program ZooImage. With this application, any
               biologist that wants to make research about plankton using the 
               ZooImage program can do it without having to learn R language. With
               this simple interface, it's easy to understand how to perform the
               different steps of the process of ZooImage."),
        tags$br(),
        tags$div("Licence of ZooImageUI : GPL2"),
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
