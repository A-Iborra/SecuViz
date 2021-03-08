### Chargement des librairies

library(visNetwork)
library(visTree)
library(shiny)
library(rpart)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinydashboard)
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "colourpicker"))

### Chargement et manipulation des données
#data <- read.table("Jeu-1-projet.csv",sep=",",header = T)
#colnames(data) <- c("ipsrc","ipdst","portdst","proto","action","date","regle")

### Définition des éléments de l'interface
ui <- dashboardPage(
  dashboardHeader(title = "Sécurité et visualisation des données"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "PagePrinc", icon = icon("home")),
                menuItem("Analyse des Flux", tabName = "flux", icon = icon("poll")),
                menuItem("CAH", tabName = "clustering", icon = icon("project-diagram")),
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
              h1("CAH"),
              sliderInput("clusters", "Nombre de cluster:", 
                          min = 2,        # 1 cluster is pointless
                          max = 10,       # too many is too crowded
                          value = 4),    # sensible start
              # radioButtons("proto", "Protocole :",
              #              c("TCP" = "tcp",
              #                "UDP" = "udp",
              #                "TCP & UDP" = "tcpudp")),
              # radioButtons("ports", "Ports :",
              #              c("Inférieur 1024" = "inf",
              #                "Supérieur 1024" = "sup",
              #                "Tous les ports" = "touslespo")),
              plotlyOutput("distPlot"),
              verbatimTextOutput("summary")
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
    read.csv(infile$datapath, header = TRUE)
  })
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "cible", data = datasetInput()))
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "expli", data = datasetInput()))  
  

  output$table = DT::renderDataTable(datasetInput())
  
  
  output$distPlot <- renderPlotly({
    data_clus <- datasetInput()
    data_clus[,c(1,2,4:6)] <- lapply(data_clus[,c(1,2,4:6)],as.factor)
    gower_dist <- daisy(data_clus, metric = "gower")
    gower_mat <- as.matrix(gower_dist)
    k <- input$clusters
    
    pam_fit <- pam(gower_dist, diss = TRUE, k)
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
    
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering)) %>% 
      mutate(ipsource = data_clus$ipsrc)
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster))
  })
  
   output$summary <- renderPrint({
     data_2 <- datasetInput()
     data_2[,c(1,2,4:6)] <- lapply(data_2[,c(1,2,4:6)],as.factor)
     gower_dist2 <- daisy(data_2, metric = "gower")
     gower_mat2 <- as.matrix(gower_dist2)
     k2 <- input$clusters
  
     pam_fit2 <- pam(gower_dist2, diss = TRUE, k2)
     pam_results2 <- data_2 %>%
       mutate(cluster = pam_fit2$clustering) %>%
       group_by(cluster) %>%
       do(the_summary = summary(.))
     pam_results2$the_summary
  

  })

  observeEvent(input$go,{
    data = datasetInput()
    #var = list()
    var = input$expli
    cib = as.character(input$cible)
    #temp <- data[,input$expli[]]
    var = unlist(as.character(input$expli))
    temp <- data %>% select(var,cib)
    #temp <- data[,input$expli,drop=FALSE]
    mod <- as.formula(paste0(cib, " ~ ."))
    res <- rpart(mod,data=temp,method = "class", control = rpart.control(cp = 0.5, minsplit = 2),model =T)
    shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(res))
  })
  
  
}



### Lancement de l'application
shinyApp(ui, server)

