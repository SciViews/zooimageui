#
# Lecocq Martin   2022
# 
# ZooImage premiers tests d'application Shiny
#


library(shiny)
library(ggplot2)
library(plotly)
library(zooimage)


# GLOBAL ------------------------------------------------------------------

setwd("/home/rstudio/shared/zooimage-ui/ZI_Test_App/")
smpfiles <- list.files("www/Samples/")
smps <- smpfiles[!grepl("Description.zis", smpfiles)]
smps <- smps[!grepl(".zidb", smps)]
smps_with_path <- paste0("www/Samples/",smps)
# zidb_files <- smpfiles[grep(".zidb", smpfiles)]

old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)


# UI ----------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("ZooImage Test"),
    
    navbarPage("ZooImage UI",
        
        # Pannel dans lequel je fait choisir un échantillon, et je vais l'utiliser pour créer le ZIDB
        tabPanel("Data Extraction and Exploration",
            sidebarLayout(
              
                # Côté choix d'échantillon, création du zidb, affichage des zidb présents
                sidebarPanel(
                    selectInput("sample_folder", "Sample folder :", choices = smps),
                    actionButton("zidbmake", "Make the ZIDB file"),
                    tags$br(),
                    tags$br(),
                    tags$h5(style = "font-weight: bold;" ,"Already formated :"),
                    textOutput("zidb_made"),
                ),
                
                # Coté affichage de ce que on peut voir à partir de ces zidb
                mainPanel(
                    tabsetPanel(
                        tabPanel("Samples", verbatimTextOutput("sample_folder_files")),
                        
                        tabPanel("ZIDB",
                            tags$br(),
                            selectInput("zidb_to_show", "Select a ZIDB file to show a preview :",
                                choices = smpfiles[grepl(".zidb", smpfiles)]),
                            
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
                        
                        tabPanel("ZIDB Vignettes",
                            tags$br(),
                            selectInput("vignettes_file", "ZIDB file to visualize",
                                choices = smpfiles[grepl(".zidb", smpfiles)]),
                            
                            # === OLD ===
                            # sliderInput("vignettes_vis", "Vignettes from n1 to n2",
                                # min = 1, max = 1, value = c(1,1), step = 1),
                            # === OLD ===
                            
                            selectInput("vignettes_vis", "Vignettes to watch", choices = ""),
                            tags$h3("Visualisation of vignettes"),
                            plotOutput("vignettes_plot")
                        )
                    )
                )
            )
        ),
        tabPanel("Data Preprocessing",
            
        ),
    )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # ============ Première page ============
    
    
    # Inutile car les échantillons ne varient pas
    # sample_files <- reactive({
    #     input$zidbmake
    #     smpfiles <- list.files("www/Samples/")
    #     smps <- smpfiles[!grepl("Description.zis", smpfiles)]
    #     smps[!grepl(".zidb", smps)]
    # })
    # 
    
    
    # Préparation de la liste des fichiers zidb réactif à la création d'un nouveau
    zidb_files <- reactive({
      input$zidbmake
      smpfiles <- list.files("www/Samples/")
      smpfiles[grepl(".zidb", smpfiles)]
    })
  
    
    # Affichage des échantillons
    output$sample_folder_files <- renderPrint(
        list.files(paste0("www/Samples/",input$sample_folder,"/"))
    )
    
    
    # Création du fichier zidb de l'échantillon choisi, si click sur le bouton,
    # et mise à jour de la sélection pour la preview zidb
    observeEvent(input$zidbmake, {
        zidbMake(paste0("./www/Samples/",input$sample_folder))
        updateSelectInput(session, "zidb_to_show",
            "Select a ZIDB file to show a preview :", choices = zidb_files())
        updateSelectInput(session, "vignettes_file",
            "ZIDB file to visualize", choices = smpfiles[grepl(".zidb", smpfiles)])
    })

    
    # Affichage des fichiers zidb éxistants
    output$zidb_made <- renderText({
        zidb_files()
    })
    
    
    # Variable réactive pour le choix fichier zidb
    dataframe <- reactive({
      zidbDatRead(paste0("www/Samples/", input$zidb_to_show))
    })
    
    
    # Affichage du header du fichier zidb choisi
    output$sample_head <- renderTable({
        head(dataframe())
    })
    
    
    # Affichage de la métadata du zidb
    output$sample_attributes <- renderPrint({
        attr(dataframe(), "metadata")
    })
    
    
    # Affichage d'un sommaire du zidb
    output$sample_summary <- renderPrint({
        summary(dataframe()[, c("Area", "Perim.", "Skew", "Kurt")])
    })
    
    
    # Affichage d'un plot test du zidb
    output$sample_test_plot <- renderPlot({
        plot(dataframe()$Area, dataframe()$Perim., xlab = "Area", ylab = "Perimeter")
    })
    
    
    # === Affichage des vignettes ===
    # Création de ma var réactive pour avoir la dataframe du fichier choisi
    dataframe_vign <- reactive({
      zidbDatRead(paste0("www/Samples/", input$vignettes_file))
    })
    
    
    # Création d'une variable réactive pour avoir le max d'objets dans le fichier choisi
    nb_vign_max <- reactive({ # OLD : nb_vign_max = nb_vign_max_min
        input$vignettes_file
        return( max( dataframe_vign()["Item"] ))
    })
    
    
    # Update du select input pour choisir les images
    observeEvent( input$vignettes_file,{
        # === OLD ===
        # updateSliderInput(session, "vignettes_vis", "Vignettes from n1 to n2",
            # min = 1, max = nb_vign_max_min(),
            # value = c(1,16), step = 1)
        # === OLD ===
        
        choices_vector <- (1:ceiling(nb_vign_max()/25))*25
        choices_vector[length(choices_vector)] <- choices_vector[length(choices_vector)-1] + nb_vign_max()%%25
        updateSelectInput(session, "vignettes_vis", "Vignettes to watch", choices = paste0(choices_vector-24," - ",choices_vector))
    })
    
    
    # Chargement du fichier ZIDB choisi
    loaded_zidb <- reactive({
        zidbLink(paste0("www/Samples/",input$vignettes_file))
    })
    
    
    # Variable réactive contenant le nom des images
    vignettes <- reactive({
        ls(loaded_zidb())[!grepl("_dat1", ls(loaded_zidb()))]
    })
    
    
    # Variable réactive qui reprend les résultats d'images à sélectionner
    vignettes_nb <- reactive({
        splitted <- strsplit(input$vignettes_vis, " - ")
        # Récupération de la limite supérieure et inférieure
        c(as.numeric(splitted[[1]][1]), as.numeric(splitted[[1]][2]))
    })
    
    
    # Affichage dans un plot de ces images
    output$vignettes_plot <- renderPlot({
        zidbPlotNew("Vignettes") # Création du plot
        for (i in vignettes_nb()[1]:vignettes_nb()[2]) # les images num i dans l'intervalle choisie
            zidbDrawVignette(loaded_zidb()[[vignettes()[i]]],
                item = i-(vignettes_nb()[1]-1), nx = 5, ny = 5)
            # A la position i moins le décalage par rapport à 1 (position dans le plot)
            # ainsi que nb d'éléments par lignes et colonnes
    })
    
    # ============ Deuxième page ============
    
    
}


# APP ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
