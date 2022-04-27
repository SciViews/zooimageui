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
    sidebarPanel(width = 6,
                 
      tags$h4("Creator of the App :"),
      tags$p("Lecocq Martin"),
      tags$h4("Email Address :"),
      tags$p("martin.lecocq@std.heh.be"),
      tags$h4("Context :"),
      tags$p("Internship at UMONS' ECONUM lab, in Mons"),
      tags$h4("Supervisor :"),
      tags$p("Prof. Philippe Grosjean"),
      tags$p("philippe.grosjean@umons.ac.be"),
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
