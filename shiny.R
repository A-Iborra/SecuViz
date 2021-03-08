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
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Sécurité et visualisation des données",titleWidth = 450),
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
              p("Le projet 2021 aborde des problématiques classiques et surtout connues reposant généralement sur
              la fouille de données et la détection des intrusions. Cette année, nous ajoutons une revue d’utilisation
              des règles d’un Firewall généralement nécessaire pour une satisfaction normative. Nous intégrons
              également une migration de ces règles vers un autre Firewall de même technologie. En résumé une
              analyse de règles sera faite à partir du log de Firewall IPTABLES de type on cloud et une migration de
              ces dernières sera à réaliser sur un Firewall de type IPTABLES stand alone. La partie fouille de données
              sera à réaliser par la suite."),
              
              h1("Lecture des données"),
              p("Le chargement des données supporte uniquement des fichiers txt ou csv avec comme séparateur la ',' ou le ';'"),
              fileInput("FileInput",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              h1("Visualisation des données"),
              dataTableOutput('table')
      ),
      
      # visualization
      tabItem(tabName = "flux",
              h1("Interactive Chord Diagrams"),
              #chorddiagOutput("distPlot", height = 600),
              h1("TOP 10/5")
              ),
      tabItem(tabName = "clustering",
              h1("Réduction de dimension t-SNE"),
              p("T-SNE : L’algorithme de réduction de dimensionnalité appelé t-distributed 
              stochastic neighbor embedding (t-SNE) est un algorithme d’apprentissage non supervisé. 
              Développé par Laurens van der Maaten et Geoffrey Hinton, il permet d’analyser des données 
              décrites dans des espaces à forte dimensionnalité (via un grand nombre de descripteurs) 
              pour les représenter dans des espaces à deux ou trois dimensions. Cet algorithme est très 
              utilisé car il facilite la visualisation de données ayant beaucoup de descripteurs.
              T-SNE est un algorithme non-linéaire de “feature extraction” qui construit une nouvelle 
              représentation des données de telle sorte que les données proches dans l’espace 
              original aient une probabilité élevée d’avoir des représentations proches dans le 
              nouvel espace. A l’inverse, les données qui sont éloignées dans l’espace original, 
              ont une probabilité faible d’avoir des représentations proches dans le nouvel espace. 
              En pratique la similarité entre chaque paire de données, dans les deux espaces, est 
              mesurée par le biais de calculs probabilistes basés sur des hypothèses de distribution. 
              Et les nouvelles représentations se construisent de telle sorte à minimiser la 
              différence entre les distributions de probabilités mesurées dans l’espace original et 
              celles du nouvel espace."),
              sliderInput("clusters", "Nombre de cluster:", 
                          min = 2,        # 1 cluster is pointless
                          max = 10,       # too many is too crowded
                          value = 4),    # sensible start
              plotlyOutput("distPlot",width="800px",height = "800px"),
              verbatimTextOutput("summary")
      ),
      tabItem(tabName = "kmeans",
              h1("Visualisation des variables selon cluster Kmeans"),
              p("K-means (ou K-moyennes) : C’est l’un des algorithmes de clustering les plus répandus.
              Il permet d’analyser un jeu de données caractérisées par un ensemble de descripteurs, 
              afin de regrouper les données “similaires” en groupes (ou clusters).La similarité entre 
              deux données peut être inférée grâce à la “distance” séparant leurs descripteurs ; ainsi 
              deux données très similaires sont deux données dont les descripteurs sont très proches. 
              Cette définition permet de formuler le problème de partitionnement des données comme la 
              recherche de K “données prototypes”, autour desquelles peuvent être regroupées les autres 
              données."),
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

