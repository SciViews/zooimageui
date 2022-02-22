#
# Lecocq Martin   2022
# 
# ZooImage premiers tests d'application Shiny
#


library(shiny)


ui <- fluidPage(

    titlePanel("ZooImage Test"),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Code here
    
}

# Run the application 
shinyApp(ui = ui, server = server)
