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
  AgeGroup <- reactive(ageToIndex(input$AgeChosen))
  EL <- reactive(ELList[[AgeGroup()]] %>%
                   filter(relativeRisk > input$RRCutoff))
  VL <- reactive(VLList[[AgeGroup()]] %>%
                   filter(Var1 %in% c(EL()$i, EL()$j)))
  net <- reactive(graph_from_data_frame(EL(), vertices = VL(), directed = FALSE))
  
  output$netPlot <- renderPlot({
    plot.igraph(net())
  }, width = 800, height = 800, res = 96)
}

shinyApp(ui, server)