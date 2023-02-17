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
    selectInput("AgeChosen", label = "Age Group", c("All Dogs","Young Adult", "Middle Adult", 
                                           "Mature Adult", "Senior")),
    #Slider for rel risk cutoff (Access using RRCutoff)
    numericInput("RRCutoff", "Relative Risk Cutoff", value = 1, min = 0, max = 150),
    
    
    
    #Radio Button for choosing mode
    radioButtons("appMode", "Analysis Mode", choices = c("All Conditions", "By Index Condition")),
    
    conditionalPanel(
      condition = "input.appMode == 'By Index Condition'",
      selectInput("indexCondition", "Index Condition", choices = NULL)
    ),
    
    #Type Select
    checkboxInput("typeStratOnOff", "Stratify By Type", FALSE),
    
    conditionalPanel(
      condition = "input.typeStratOnOff == 1",
      checkboxGroupInput("typeStrat", "Types to include", choices = c("Yessir"))
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
  switchAgeOrMode <- reactive({list(input$appMode, input$AgeChosen)})
  observeEvent(switchAgeOrMode(),{
    #freezeReactiveValue(input, "indexCondition")
    updateSelectInput(inputId = "indexCondition", choices = VL() %>%
                        arrange(Var1) %>%
                        pull(Var1))
    }
  )
  
  #Input update for types that can be checked off
  typeChecked <- reactive({list(input$typeStratOnOff, input$indexCondition)})
  observeEvent(typeChecked(),{
    if(input$appMode == "All Conditions"){
      updateCheckboxGroupInput(inputId = "typeStrat", choices = unique(VL() %>%
                                                                         pull(typeName)))
    }else{
      updateCheckboxGroupInput(inputId = "typeStrat", choices = unique(indexVL() %>%
                                                                         pull(typeName)))
    }
    
  })
  
  #Reactive expression for type picking
  typeList <- reactive(input$typeStrat)
  
  #Reactive expression assigning proper Edge List
  EL <- reactive(ELList[[AgeGroup()]] %>%
                   filter(relativeRisk > input$RRCutoff)) 
  #Reactive expression assigning proper Vertex List
  VL <- reactive(VLList[[AgeGroup()]] %>%
                   filter(Var1 %in% c(EL()$i, EL()$j)))
  #Reactive expression creating network
  net <- reactive(graph_from_data_frame(EL(), vertices = VL(), directed = FALSE))
  
  #################new code
  #Making EL, VL and net for index condition net
  
    #Find vertices that are connected to index condition - Messy!
  indexConditionVertices <- reactive(unique(c(EL() %>% 
                                filter(i == input$indexCondition | j == input$indexCondition) %>%
                                pull(i),
                              EL() %>% 
                                filter(i == input$indexCondition | j == input$indexCondition) %>%
                                pull(j))))
  
    #Make EL of edges that connect two vertices in the above group
  indexEL <- reactive(EL() %>% filter(i %in% indexConditionVertices() & j %in% indexConditionVertices()))
  indexVL <- reactive(VL() %>% filter(Var1 %in% c(indexEL()$i,indexEL()$j)))
  indexNet <- reactive(graph_from_data_frame(indexEL(), vertices = indexVL(), directed = FALSE))
  
  #Plot output
  output$netPlot <- renderPlot({
    
    #Set parameters (Margin of 0)
    par(mar = c(0,3.5,0,3.5))
    
    #If statement where first condition is for all conditions, second is for index conditions
    if(input$appMode == "All Conditions"){
      plot(net(),
           vertex.color = V(net())$type,
           vertex.label.cex = 0.5,
           vertex.size = log10(V(net())$Freq)*5,
           vertex.frame.width = 2,
           edge.width = log10(E(net())$relativeRisk))
    }else{
      plot(indexNet(),
           vertex.color = V(indexNet())$type,
           vertex.label.cex = 0.5,
           vertex.size = log10(V(indexNet())$Freq)*5,
           vertex.frame.width = 2,
           layout = layout_as_star(indexNet(), center = V(indexNet())[V(indexNet())$name == input$indexCondition]),
           vertex.label.degree = pi / 2,
           vertex.label.dist = 1.25,
           edge.width = log10(E(indexNet())$relativeRisk))
    }
  },
  #Plot parameters
  width = 800, height = 800, res = 200)
  
  #EdgeList output
  output$edgeList <- DT::renderDataTable({
    
    if(input$appMode == "All Conditions"){
      EL() %>%
        select(i,j,iCount,jCount,combinedCount,relativeRisk,ChiQ) %>%
        rename("Condition 1" = i,
               "Condition 2" = j,
               "Freq. 1" = iCount,
               "Freq. 2" = jCount,
               "Comorbid Count" = combinedCount,
               "Relative Risk" = relativeRisk,
               "Q value" = ChiQ)
    }else{
      indexEL() %>%
        select(i,j,iCount,jCount,combinedCount,relativeRisk,ChiQ) %>%
        rename("Condition 1" = i,
               "Condition 2" = j,
               "Freq. 1" = iCount,
               "Freq. 2" = jCount,
               "Comorbid Count" = combinedCount,
               "Relative Risk" = relativeRisk,
               "Q value" = ChiQ)
         }
    
  })
  
  #VertexList output
  output$vertexList <- DT::renderDataTable({
    if(input$appMode == "All Conditions"){
      VL() %>%
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
    }else{
      indexVL() %>%
        select(Var1, Freq, typeName) %>%
        mutate(CMI = sapply(1:nrow(indexVL()),
                            function(x) sum(indexEL() %>%
                                              filter(i == indexVL()$Var1[x] | j == indexVL()$Var1[x]) %>%
                                              pull(relativeRisk)))) %>%
        mutate(deg = sapply(1:nrow(indexVL()),
                            function(x) nrow(indexEL() %>%
                                               filter(i == indexVL()$Var1[x] | j == indexVL()$Var1[x])))) %>%
        rename("Condition" = Var1,
               "Frequency" = Freq,
               "Disease Type" = typeName,
               "Comorbidity Index" = CMI)
    }
  }
  )
  
  
}

shinyApp(ui, server)