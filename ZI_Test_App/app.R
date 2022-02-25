#
# Lecocq Martin   2022
# 
# ZooImage premiers tests d'application Shiny
#


library(shiny)
library(ggplot2)
library(plotly)
library(zooimage)
library(sortable)
library(shinyjs)


# GLOBAL ------------------------------------------------------------------

setwd("/home/rstudio/shared/zooimage-ui/ZI_Test_App/")
smpfiles <- list.files("www/Samples/")
smps <- smpfiles[!grepl("Description.zis", smpfiles)]
smps <- smps[!grepl(".zidb", smps)]

old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)


# UI ----------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("ZooImage Test"),
    useShinyjs(),
    
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
                    
                    actionButton("zidbmakeall", "Make ZIDB file for all of the samples"),
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
                            
                            selectInput("vignettes_vis", "Vignettes to watch", choices = "None"),
                            tags$h3("Visualisation of vignettes"),
                            plotOutput("vignettes_plot")
                        )
                    )
                )
            )
        ),
        tabPanel("Data Preprocessing",
            # ConditionalPanel permettant de faire en sorte que cette UI
            # ne s'affiche que si on a des fichiers ZIDB
            conditionalPanel(
                condition = "output.zidb_files > 0",
                
                tabsetPanel(
                    
                    # Page de préparation du Train Set
                    tabPanel("Preparing Train Set",
                             
                        fluidRow(
                            column(width = 4,
                                tags$h3("Files for Training Set") ,
                                textInput("ts_name", "Name of the Training Set"),
                                checkboxGroupInput("zidb_to_prepare", label = "" ,choices = smpfiles[grepl(".zidb", smpfiles)]),
                                selectInput("ts_template", "Template :", choices = c("[Detailed]", "[Basic]", "[Very detailed]")),
                                actionButton("ts_prepare", "Prepare Training Set"),
                            ),
                            column(width = 8,
                                tags$h3("Existing Train Sets"),
                                verbatimTextOutput("ts_view"),
                                
                                # Panneau : ne s'affiche que si il y a des Training Sets
                                conditionalPanel(
                                    condition = "output.ts_folder_len > 0",
                                    selectInput("ts_folder_to_show", "Train Set to visualize", choices = list.files("www/TS_Unsorted/")),
                                    verbatimTextOutput("ts_content")
                                )
                            )
                        )
                    ),
                    
                    # Page de tri manuel en attendant de trouver mieux
                    tabPanel("Manual Sorting",
                        
                        # Panneau : ne s'affiche que si il y a des Training Sets
                        conditionalPanel(
                            condition = "output.ts_folder_len > 0",
                            fluidRow(
                                
                                # Partie des TS non triés / DOWNLOAD
                                column(width = 6,
                                    tags$h3("Unsorted Training Sets :"),
                                    sidebarLayout(
                                        
                                        sidebarPanel(
                                            selectInput("tsu_folder_to_dl", "Training Set to download", choices = list.files("www/TS_Unsorted/")),
                                            downloadButton("tsu_download", "Download .zip")
                                        ),
                                        
                                        mainPanel(
                                            verbatimTextOutput("tsu_view"),
                                        )
                                    )
                                ),
                                
                                # Partie des TS triés / UPLOAD
                                column(width = 6,
                                    tags$h3("Sorted Training Sets :"),
                                    sidebarLayout(
                                      
                                        sidebarPanel(
                                            tags$h4("Upload your zipped Training Set here :"),
                                            tags$br(),
                                            fileInput("ts_ms_up", "Upload .zip", multiple = FALSE),
                                            tags$p("Please, wait that it appears in the list")
                                        ),
                                        
                                        mainPanel(
                                            conditionalPanel(
                                                condition = "output.tss_folder_len > 0",
                                                verbatimTextOutput("ts_ms_up_view"),
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    
                    # Page de tri manuel si on utilisait sortable // DEMO
                    tabPanel("Manual Sorting (sortable demo)",
                        bucket_list(
                            header = "Drag images",
                            group_name = "bucket_list_group",
                            orientation = "horizontal",
                            add_rank_list(
                                text = "Drag from here",
                                labels = list.files("www/TS_Unsorted/test/_/")[!grepl(".jpg", list.files("www/TS_Unsorted/test/_/"))],
                                input_id = "rank_list_1"
                            ),
                            add_rank_list(
                                text = "To here",
                                labels = NULL,
                                input_id = "rank_list_2"
                            )
                        )
                    )
                )
            )
        ),
    )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Taille max pour upload :
    options(shiny.maxRequestSize=30*1024^2)
    # Timer pour enclencer des choses réactive toutes les t millisecondes
    timer <- reactiveTimer(3000)


# SERVER : Premiere page --------------------------------------------------


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
      input$zidbmakeall
      smpfiles <- list.files("www/Samples/")
      smpfiles[grepl(".zidb", smpfiles)]
    })
  
    
    # Affichage des échantillons
    output$sample_folder_files <- renderPrint({
        list.files(paste0("www/Samples/",input$sample_folder,"/"))
    })
    
    
    # Création du fichier zidb de l'échantillon choisi, si click sur le bouton,
    # et mise à jour de la sélection pour la preview zidb
    observeEvent(input$zidbmake, {
        zidbMake(paste0("./www/Samples/",input$sample_folder))
        updateSelectInput(session, "zidb_to_show",
            "Select a ZIDB file to show a preview :", choices = zidb_files())
        updateSelectInput(session, "vignettes_file",
            "ZIDB file to visualize", choices = zidb_files())
        updateCheckboxGroupInput(session, "zidb_to_prepare", "", choices = zidb_files())
    })
    
    
    # Création du zidb pour tous les échantillons, si click, et mises à jour sélec
    observeEvent(input$zidbmakeall, {
        zidbMakeAll("./www/Samples/", delete.source = FALSE, replace = TRUE)
        updateSelectInput(session, "zidb_to_show",
            "Select a ZIDB file to show a preview :", choices = zidb_files())
        updateSelectInput(session, "vignettes_file",
            "ZIDB file to visualize", choices = zidb_files())
        updateCheckboxGroupInput(session, "zidb_to_prepare", "", choices = zidb_files())
    })
    
    
    # Affichage des fichiers zidb éxistants
    output$zidb_made <- renderText({
        zidb_files()
    })
    
    
    # Variable réactive pour le choix fichier zidb
    dataframe <- reactive({
      req(input$zidb_to_show)
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
    # Utilisation d'une variable réactive pour $vignettes_file car utilisé partout, et est nécessaire
    vignettes_file <- reactive({
      req(input$vignettes_file) # Indiquer qu'il a besoin d'avoir une valeur, soit un nom de fichier
      input$vignettes_file
    })
    
    
    # Création d'une variable réactive pour avoir le max d'objets dans le fichier choisi (+clarté)
    nb_vign_max <- reactive({ # OLD : nb_vign_max = nb_vign_max_min
        
        # Création d'une var pour accéder à ma dataframe facilement
        dataframe_vign <- zidbDatRead(paste0("www/Samples/", vignettes_file()))
      
        return( max( dataframe_vign["Item"] ))
    })
    
    
    # Update du select input pour choisir les images
    observeEvent( vignettes_file(),{
        
        choices_vector_sup <- (1:ceiling(nb_vign_max()/25))*25 # Divise le nombre total de
        # vignettes par 25, on en prend l'arrondis supérieur, et on crée un vecteur allant de
        # 1 à cette valeur, le tout multiplié par 25, pour avoir les bornes supérieurers des
        # classes d'images (images de 1 à 25, ou de 26 à 50)
        
        # Création d'un vecteur avec les bornes inférieures
        choices_vector_inf <- choices_vector_sup - 24
        # Mise à niveau de la dernière borne supérieure, pour qu'elle corresponde au max réel
        choices_vector_sup[length(choices_vector_sup)] <- nb_vign_max()
        
        # Ensuite on change la dernière valeur pour qu'elle corresponde au maximum possible
        # choices_vector[length(choices_vector)] <- choices_vector[length(choices_vector)-1] + nb_vign_max()%%25 (Ancien code)
        
        updateSelectInput( session, "vignettes_vis", "Vignettes to watch",
            choices = paste0( choices_vector_inf, " - ", choices_vector_sup ))
    })
    
    
    # Chargement du fichier ZIDB choisi
    loaded_zidb <- reactive({
        zidbLink( paste0("www/Samples/", vignettes_file()) )
    })
    
    
    # Variable réactive contenant le nom des images
    vignettes <- reactive({
        ls( loaded_zidb() )[ !grepl( "_dat1", ls( loaded_zidb() )) ]
    })
    
    
    # Variable réactive qui reprend les résultats d'images à sélectionner
    vignettes_nb <- reactive({
        
        splitted <- strsplit( input$vignettes_vis, " - ")
        
        # Récupération de la limite supérieure et inférieure de vignettes souhaitées
        c( as.numeric( splitted[[1]][1] ), as.numeric( splitted[[1]][2] ) )
    })
    
    
    # Affichage dans un plot de ces images
    output$vignettes_plot <- renderPlot({
        
        req(vignettes_file(), vignettes_nb()) # Besoin de vignettes_file et vignettes_nb pour se faire
        
        from_image_nb <- vignettes_nb()[1]
        to_image_nb <- vignettes_nb()[2]
        
        zidbPlotNew("Vignettes") # Création du plot
        
        for (i in from_image_nb:to_image_nb) # les images num i dans l'intervalle choisie
            zidbDrawVignette( loaded_zidb()[[vignettes()[i]]],
                item = i - (from_image_nb - 1), nx = 5, ny = 5)
            # A la position i moins le décalage par rapport à 1 (position dans le plot)
            # ainsi que nb d'éléments par lignes et colonnes
    })


# SERVER : Deuxieme page --------------------------------------------------


# === Partie Preparation du Training Set ===
    
    # output pour créer un panneau conditionnel dans mon UI   (Dynamic UI)
    # (apparait que si des fichiers ZIDB sont présents)
    output$zidb_files <- reactive({
        input$zidbmake
        input$zidbmakeall
        smpfiles <- list.files("www/Samples/")
        length( smpfiles[grepl( ".zidb", smpfiles)] )
    })
    # Nécessaire pour le que browser charge la valeur la plus récente   (Dynamic UI)
    # d'output pour évaluer correctement la conditon
    outputOptions( output, "zidb_files", suspendWhenHidden = FALSE )
    
    
    # Définition d'une var utile réactive car varie par rapport à l'input actionbutton
    ts_folders <- reactive({
        input$ts_prepare
        timer()
        list.files("www/TS_Unsorted/")
    })
    
    
    # Création du Train Set
    observeEvent( input$ts_prepare, {
      
        # Retirer les caractères spéciaux du nom de dossier, et arrêter si le nom 
        # du dossier est vide ou si pas de zidb cochés
        ts_name <- stringr::str_replace_all( input$ts_name, "[^[:alnum:]]", "")
        req( ts_name != "", length( input$zidb_to_prepare ) > 0)
        
        # Preparation des arguments
        traindir <- paste0("www/TS_Unsorted/", ts_name)
        zidb_files <- paste0("www/Samples/", input$zidb_to_prepare)
        
        # Fonction principale
        prepareTrain( traindir = traindir, zidbfiles = zidb_files, template = input$ts_template )
        
        # Actualisation des dossiers et fichiers
        updateSelectInput( session, "ts_folder_to_show", "Train Set to visualize", choices = ts_folders() )
        updateSelectInput( session, "tsu_folder_to_dl", "Training Set to download", choices = ts_folders() )
    })
    
    
    # Output pour montrer les Train Sets existants
    output$ts_view <- renderPrint({
        if( length( ts_folders() ) > 0) {ts_folders()} else {"No Training Set Yet"}
    })
    
    
    # Création d'une variable output (Dynamic UI)
    output$ts_folder_len <- reactive({
        # input$ts_prepare => Remettre si jamais problème d'affichage
        length(ts_folders())
    })
    # chargement de la variable pour le browser (Dynamic UI)
    outputOptions(output, "ts_folder_len", suspendWhenHidden = FALSE)
    
    
    # Affichage du contenu du dossier TS
    output$ts_content <- renderPrint({
        list.files(paste0("www/TS_Unsorted/",input$ts_folder_to_show))
    })
    
    
# === Partie manual sorting ===
    
    # 1) Partie des Training Sets non triés
    
    # Montrer les Training Sets non triés disponibles
    output$tsu_view <- renderPrint({
        ts_folders()
    })
    
    
    # Bouton pour télécharger le Training Set
    output$tsu_download <- downloadHandler(
        filename = function() {
            paste(input$tsu_folder_to_dl, ".zip", sep = "")
        },
        content = function(file) {
            zip(zipfile = file, files = paste0("www/TS_Unsorted/", input$tsu_folder_to_dl))
        }
    )
    
    
    # 2) Partie des Training Sets triés
    
    # Variable réactive pour afficher les TS triés
    tss_folders <- reactive({
        input$ts_ms_up
        timer()
        list.files("www/TS_Sorted/")
    })
    
    
    # Quand on upload => Récuperer le fichier
    observeEvent( input$ts_ms_up, {
      
        # Désactive le bouton upload, pour ne pas surcharger
        shinyjs::disable("ts_ms_up")
        
        # Mise en place d'une variable pour récupérer les données dans la table,
        # et test du contenu de celle-ci
        ts_ms_up <- input$ts_ms_up
            if ( is.null(ts_ms_up) )
            return()
        
        # "Copie" du fichier rentrant, pour le stocker dans le système
        file.copy(ts_ms_up$datapath, file.path("www/TS_Sorted/", ts_ms_up$name))
        
        # Unzip et supression du zip
        setwd("www/TS_Sorted/")
        unzip( ts_ms_up$name )
        unlink( ts_ms_up$name )
        setwd("../../")
        
        # Réactive le bouton upload, une fois que tout est fini
        shinyjs::enable("ts_ms_up")
    })

    
    # Output pour panneau conditionnel pour afficher les TS triés   (dynamic UI)
    output$tss_folder_len <- reactive({
        length(tss_folders())
    })
    # Chargement de la variable pour le browser   (dynamic UI)
    outputOptions(output, "tss_folder_len", suspendWhenHidden = FALSE)
    
    
    # Montrer les Training Sets triés disponibles
    output$ts_ms_up_view <- renderPrint({
        tss_folders()
    })

}


# APP ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
