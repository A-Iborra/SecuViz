### Chargement des librairies

library(visNetwork)
library(visTree)
library(shiny)
library(rpart)
library(readxl)
library(dplyr)
library(factoextra)
library(ggpubr)
library(ggplot2)
library(sqldf)
library(cluster)
library(Rtsne)
library(stringr)
library(shinydashboard)
library(GGally)
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "colourpicker"))


options(shiny.maxRequestSize=30*1024^3)
source("fonctions.R")


### Définition des éléments de l'interface
ui <- dashboardPage(
  dashboardHeader(title = "Sécurité et visualisation des données"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "PagePrinc", icon = icon("home")),
                menuItem("Analyse des Flux", tabName = "flux", icon = icon("poll")),
                menuItem("t-SNE", tabName = "clustering", icon = icon("project-diagram")),
                menuItem("K-means", tabName = "kmeans", icon = icon("spinner")),
                menuItem("Arbre de décision", tabName = "arbre", icon = icon("tree")))
  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PagePrinc",
              h1("Description du Projet"),
              h1("Lecture des données"),
              fileInput("FileInput",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              h1("Données"),
              dataTableOutput('table')
      ),
      
      # visualization
      tabItem(tabName = "flux",
              h1("Interactive Chord Diagrams"),
              #chorddiagOutput("distPlot", height = 600),
              h1("TOP 10/5")
              ),
      tabItem(tabName = "clustering",
              h1("t-SNE"),
              sliderInput("clusters", "Nombre de cluster:", 
                          min = 2,        # 1 cluster is pointless
                          max = 10,       # too many is too crowded
                          value = 4),    # sensible start
              plotlyOutput("distPlot",width="800px",height = "800px"),
              verbatimTextOutput("summary")
      ),
      tabItem(tabName = "kmeans",
              h1("Visualisation des variables selon cluster Kmeans"),
              sliderInput("clusters2", "Nombre de cluster:", 
                          min = 2,        # 1 cluster is pointless
                          max = 10,       # too many is too crowded
                          value = 3),    # sensible start
              plotlyOutput("kgraph",width="500px",height = "500px")
      ),
      tabItem(tabName = "arbre",
              h1("Arbre de décision"),
              varSelectInput("cible", "Variables à expliquer:", data, multiple = FALSE),
              varSelectInput("expli", "Variables explicatives:", data, multiple = TRUE),
              actionButton("go", "Valider"),
              visTreeModuleUI(id = "id1", rpartParams = FALSE, visTreeParams = FALSE)
      )
    )
  )
)


### Back-end
server <- function(input, output, session){
  
  datasetInput <- reactive({
    infile <- input$FileInput
    if(is.null(infile))
      return(NULL)
    L <- readLines(infile$datapath, n = 1)
    numfields <- count.fields(textConnection(L), sep = ";")
    if (numfields == 1) read.csv(infile$datapath,header = TRUE,fileEncoding="UTF-8-BOM") else read.csv2(infile$datapath,header = TRUE,fileEncoding="UTF-8-BOM")
  })
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "cible", data = datasetInput()))
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "expli", data = datasetInput()))  
  

  output$table = DT::renderDataTable(datasetInput())
  
  
  output$distPlot <- renderPlotly({
    data_clus <- datasetInput()[,c(3,6,8:11)]
    k <- input$clusters
    res <- tsne_base(data_clus,k)
    res[[1]]
  })
  

  output$kgraph <- renderPlotly({
    k2 <- input$clusters2
    data <- datasetInput()
    data_bis <- databis(data)
    res.km <- kmeans(data_bis[,-c(3,7)],k2)

    data_bis$cluster <- factor(res.km$cluster)
    var <- c("nombre","cndstport","cluster")
    data_graph <- data_bis %>% select(var)
    
    km_graph1(data_graph)

    #ggplot(aes(x = nombre, y = cndstport), data = data_graph) +
      #geom_point(aes(color = cluster))+ coord_cartesian(xlim =c(0, 40), ylim = c(0, 40))
  })
  
   output$summary <- renderPrint({
     data_2 <- datasetInput()[,c(3,6,8:11)]
     k2 <- input$clusters
     res <- tsne_base(data_2,k2)
     res[[2]]

  })

  observeEvent(input$go,{
    data = datasetInput()
    data = data[1:30000,]
    #var = list()
    var = input$expli
    cib = as.character(input$cible)
    #temp <- data[,input$expli[]]
    var = unlist(as.character(input$expli))
    temp <- data %>% select(var,cib)
    #temp <- data[,input$expli,drop=FALSE]
    mod <- as.formula(paste0(cib, " ~ ."))
    res <- rpart(mod,data=temp,method = "class", control = rpart.control(cp = 0.05, minsplit  = 10,maxdepth=30),model =T)
    shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(res))
  })
  
  
}

### Lancement de l'application
shinyApp(ui, server)

