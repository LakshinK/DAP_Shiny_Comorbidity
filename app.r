library(tidyverse)
library(shiny)
library(igraph)

ui <- fluidPage(
  
  # App title ----
  titlePanel("DAP Comorbidity"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    #Age Group Selection (access using label AgeGroup)
    selectInput("AgeChosen", label = "Age Group", c("Young Adult", "Middle Adult", 
                                           "Mature Adult", "Senior")),
    #Slider for rel risk cutoff (Access using RRCutoff)
    numericInput("RRCutoff", "Relative Risk Cutoff", value = 1, min = 0, max = 150)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(plotOutput("netPlot"))
)

server <- function(input, output){
  #Reactive expression for pulling which age group was selected
  AgeGroup <- reactive(ageToIndex(input$AgeChosen))
  #Reactive expression assigning proper Edge List
  EL <- reactive(ELList[[AgeGroup()]] %>%
                   filter(relativeRisk > input$RRCutoff))
  #Reactive expression assigning proper Vertex List
  VL <- reactive(VLList[[AgeGroup()]] %>%
                   filter(Var1 %in% c(EL()$i, EL()$j)))
  #Reactive expression creating network
  net <- reactive(graph_from_data_frame(EL(), vertices = VL(), directed = FALSE))
  
  #Plot output
  output$netPlot <- renderPlot({
    
    par(mar = c(0,0,0,0))
    
    plot(net(),
                vertex.color = V(net())$type,
                vertex.label.cex = 0.5,
                margin = 0)
  }, width = 1300, height = 1300, res = 300)
}

shinyApp(ui, server)