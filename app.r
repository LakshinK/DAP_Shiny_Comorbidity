library(tidyverse)
library(shiny)
library(igraph)
library(DT)

ui <- fluidPage(
  
  # App title ----
  titlePanel("DAP Comorbidity"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    #Age Group Selection (access using label AgeGroup)
    selectInput("AgeChosen", label = "Age Group", c("Young Adult", "Middle Adult", 
                                           "Mature Adult", "Senior")),
    #Slider for rel risk cutoff (Access using RRCutoff)
    numericInput("RRCutoff", "Relative Risk Cutoff", value = 1, min = 0, max = 150),
    
    #Radio Button for choosing mode
    radioButtons("appMode", "Analysis Mode", choices = c("All Conditions", "By Index Condition")),
    
    conditionalPanel(
      condition = "input.appMode == 'By Index Condition'",
      selectInput("indexCondition", "Index Condition", choices = NULL)
    )
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tabsetPanel(type = "tabs",
      tabPanel("Network", plotOutput("netPlot")),
      tabPanel("Edge List", DT::dataTableOutput("edgeList")),
      tabPanel("Vertex List", DT::dataTableOutput("vertexList")))
    
    )
)

server <- function(input, output){
  
  
  #Reactive expression for pulling which age group was selected
  AgeGroup <- reactive(ageToIndex(input$AgeChosen))
  
  #Input Update for Index condition availability
  eventUpdate <- reactive({list(input$appMode, input$AgeChosen)})
  observeEvent(eventUpdate(),
               updateSelectInput(inputId = "indexCondition", choices = VL() %>% pull(Var1)))
  
  
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
    
    #Set parameters (Margin of 0)
    par(mar = c(0,0,0,0))
    
    
    plot(net(),
                vertex.color = V(net())$type,
                vertex.label.cex = 0.5,
                vertex.size = log10(V(net())$Freq)*5,
                vertex.frame.width = 2,
                margin = 0)
    }, 
  #Plot parameters
  width = 800, height = 800, res = 200)
  
  #EdgeList output
  output$edgeList <- DT::renderDataTable({EL() %>%
                                    select(i,j,iCount,jCount,combinedCount,relativeRisk,ChiQ) %>%
                                    rename("Condition 1" = i,
                                           "Condition 2" = j,
                                           "Freq. 1" = iCount,
                                           "Freq. 2" = jCount,
                                           "Comorbid Count" = combinedCount,
                                           "Relative Risk" = relativeRisk,
                                           "Q value" = ChiQ)
    })
  
  #VertexList output
  output$vertexList <- DT::renderDataTable({VL() %>%
                                      select(Var1, Freq, typeName) %>%
                                      mutate(CMI = sapply(1:nrow(VL()),
                                                          function(x) sum(EL() %>%
                                                                            filter(i == VL()$Var1[x] | j == VL()$Var1[x]) %>%
                                                                            pull(relativeRisk)))) %>%
                                      mutate(deg = sapply(1:nrow(VL()),
                                                          function(x) nrow(EL() %>%
                                                                            filter(i == VL()$Var1[x] | j == VL()$Var1[x])))) %>%
                                      rename("Condition" = Var1,
                                             "Frequency" = Freq,
                                             "Disease Type" = typeName,
                                             "Comorbidity Index" = CMI)
    })
  
  
}

shinyApp(ui, server)