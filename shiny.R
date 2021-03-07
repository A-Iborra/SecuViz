### Chargement des librairies

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinydashboard)
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "googleVis", "colourpicker"))
#install.packages("chorddiag")
#devtools::install_github("mattflor/chorddiag")
#library(chorddiag)

### Chargement et manipulation des données
data <- read.table("Jeu-1-projet.csv",sep=",")
colnames(data) <- c("ipsrc","ipdst","portdst","proto","action","date","regle")

### Définition des éléments de l'interface
ui <- dashboardPage(
  dashboardHeader(title = "Sécurité et visualisation des données"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "PagePrinc", icon = icon("home")),
                menuItem("Analyse des Flux", tabName = "flux", icon = icon("poll")),
                menuItem("CAH", tabName = "clustering", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PagePrinc",
              h1("Description du Projet"),
              h1("Dictionnaire des données"),
              h1("Données ?")
      ),
      
      # visualization
      tabItem(tabName = "flux",
              h1("Interactive Chord Diagrams"),
              #chorddiagOutput("distPlot", height = 600),
              h1("TOP 10/5")
              ),
      tabItem(tabName = "clustering",
              h1("CAH")
      )
    )
  )
)


### Back-end
server <- function(input, output, session){
  # output$distPlot <- renderChorddiag({
  #   temp <- as.data.frame(table(data$ipsrc))
  #   temp = as.matrix(temp)
  #   row.names(temp) = c(colnames(temp))
  #   chorddiag(temp)
  # })
}

### Lancement de l'application
shinyApp(ui, server)

