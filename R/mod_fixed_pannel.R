#' fixed_pannel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fixed_pannel_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "fixed_pannel_div",
             tags$h4("Global Informations", id = "fixed_pannel_title"),
             
             tags$hr(),
             tags$h4("-> Global Settings"),
             tags$h5("Data folder path :"),
             textOutput(ns("data_folder_path")),
             
             tags$hr(),
             tags$h4("-> Samples"),
             tags$h5("Samples folder path :"),
             textOutput(ns("Samples_folder_path")),
             tags$br(),
             selectInput(ns("zidb_show"), NULL, choices = NULL),
             textOutput(ns("zidb_show_nrow")),
             
             tags$hr(),
             tags$h4("-> Training Sets"),
             selectInput(ns("ts_select"), NULL, choices = NULL),
             textOutput(ns("ts_prog_show")),
             actionButton(ns("ts_fp_refresh"), "Refresh"),
             
             tags$hr(),
             tags$h4("-> Models"),
             tags$p("- Empty 1"),
             tags$p("- Empty 2"),
             
             tags$hr(),
             tags$h4("-> Results"),
             tags$p("- Empty 1"),
             tags$p("- Empty 2"),
    )
  )
}

#' fixed_pannel Server Functions
#'
#' @noRd 
mod_fixed_pannel_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Récupération Des Variables ----------------------------------------------
    
    # settings_vars
    data_folder_path <- reactive({ all_vars$settings_vars$data_folder_path_rea })
    Samples_folder_path <- reactive({ all_vars$settings_vars$Samples_folder_path})
    
    # samples_vars
    zidb_files <- reactive({ all_vars$samples_vars$zidb_files })
    zidb_df_nrow <- reactive({ all_vars$samples_vars$zidb_df_nrow })
    
    # training_sets_vars
    ts_folder_path <- reactive({ all_vars$training_sets_vars$ts_folder_path })
    ts_list <- reactive({ all_vars$training_sets_vars$ts_list })
    
    # Settings ----------------------------------------------------------------
    
    # Affichage // data_folder_path
    output$data_folder_path <- renderText({ data_folder_path() })
    # Affichage // Samples_folder_path
    output$Samples_folder_path <- renderText({ Samples_folder_path() })
    
    # Samples -----------------------------------------------------------------
    
    # Mise à jour du choix du ZIDB à montrer dans samples
    observe({
      if (length(zidb_files()) > 0) {
        updateSelectInput(session, "zidb_show", NULL, choices = zidb_files())
      } else {
        updateSelectInput(session, "zidb_show", NULL, choices = "No ZIDB file yet")
      }
    })
    
    # Affichage // Nombre de lignes dans le ZIDB choisi
    output$zidb_show_nrow <- renderText({
      if (length(zidb_files()) > 0) {
        paste0(zidb_df_nrow(), " rows in the sample")
      } else {
        "No ZIDB file yet"
      }
    })
    
    # Training Sets -----------------------------------------------------------
    
    observe({
      if (length(ts_list()) > 0) {
        updateSelectInput(session, "ts_select", NULL, choices = ts_list())
      } else {
        updateSelectInput(session, "ts_select", NULL, choices = "No Training Set yet")
      }
    })
    
    output$ts_prog_show <- renderText({
      if (length(ts_list()) > 0) {
        dir <- fs::path(ts_folder_path(),req(input$ts_select))
        ts_total_vign <- length(fs::dir_ls(dir, glob = "*.jpg", recurse = TRUE))
        ts_unsorted_vign <- length(fs::dir_ls(fs::path(dir, "_"), glob = "*.jpg", recurse = TRUE))
        ts_sorted_vign <- ts_total_vign - ts_unsorted_vign
        classed_rate <- (ts_sorted_vign/ts_total_vign) * 100
        paste("Sorted : ", ts_sorted_vign, " / ",ts_total_vign)
      } else {
        "No Training Set yet"
      }
    })
    
    # Communication -----------------------------------------------------------
    
    fixed_pannel_vars <- reactiveValues(
      zidb_show = NULL,
      ts_fp_refresh = NULL,
      ts_select = NULL,
    )
    
    observe({
      fixed_pannel_vars$zidb_show <- if (req(input$zidb_show) != "No ZIDB file yet") {
        input$zidb_show
      }
    })
    
    observe({
      fixed_pannel_vars$ts_fp_refresh <- input$ts_fp_refresh
      fixed_pannel_vars$ts_select <- if (req(input$ts_select) != "No Training Set yet") {
        input$ts_select
      }
    })
    
    return(fixed_pannel_vars)
    
  })
}

## To be copied in the UI
# mod_fixed_pannel_ui("fixed_pannel_ui_1")

## To be copied in the server
# mod_fixed_pannel_server("fixed_pannel_ui_1")
