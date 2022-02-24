#
# Just a test app for sortable package
#

library(shiny)
library(sortable)

# Pourquoi le vecteur généré par list.files ne veut pas être considéré ? => chemin était mauvais
files <- list.files("/home/rstudio/shared/zooimage-ui/ZI_Test_App/www/TS/test")
names <- c("Marc","Pierre","Paul")

labels <- files

# labels <- list(
#   "one",
#   "two",
#   "three",
#   htmltools::tags$div(
#     htmltools::em("Complex"), " html tag without a name"
#   ),
#   "five" = htmltools::tags$div(
#     htmltools::em("Complex"), " html tag with name: 'five'"
#   )
# )

rank_list_basic <- rank_list(
  text = "Drag the items in any desired order",
  labels = labels,
  input_id = "rank_list_basic"
)

rank_list_swap <- rank_list(
  text = "Notice that dragging causes items to swap",
  labels = labels,
  input_id = "rank_list_swap",
  options = sortable_options(swap = TRUE)
)

rank_list_multi <- rank_list(
  text = "You can select multiple items, then drag as a group",
  labels = labels,
  input_id = "rank_list_multi",
  options = sortable_options(multiDrag = TRUE)
)



ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      tags$h2("Default, multi-drag and swapping behaviour"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Default",
          tags$b("Exercise"),
          rank_list_basic,
          tags$b("Result"),
          verbatimTextOutput("results_basic")
        ),
        tabPanel(
          "Multi-drag",
          tags$b("Exercise"),
          rank_list_multi,
          tags$b("Result"),
          verbatimTextOutput("results_multi")
        ),
        tabPanel(
          "Swap",
          tags$b("Exercise"),
          rank_list_swap,
          tags$b("Result"),
          verbatimTextOutput("results_swap")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
  })
  output$results_multi <- renderPrint({
    input$rank_list_multi # This matches the input_id of the rank list
  })
  output$results_swap <- renderPrint({
    input$rank_list_swap # This matches the input_id of the rank list
  })
}

shinyApp(ui, server)