library(visNetwork)
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  h4("Siwu ideophones"),
  div(id="TopNetwork",
      h5("Hover over an image to see the ideophones in that domain."),
      visNetworkOutput("bignetwork")),
  hidden(div(id="HandNetwork",
             h5("Double click on a node to expand or contract it"),
             actionButton("back","Back to mainscreen"),
             visNetworkOutput("hand"))),
  h6("Based on analysis in Dingemanse, M. & Majid, A. 2012. The semantic structure of sensory vocabulary in an African Language. Proceedings of the 34th Annual Conference of the Cognitive Science Society."))

server <- function(input,output){
  output$bignetwork <- renderVisNetwork({
    # minimal example
    nodes <- read.csv("Figure10_nodes.csv",encoding = "UTF-8")
    edges <- read.csv("Figure10_edges.csv",encoding = "UTF-8")
    
    visNetwork(nodes, edges) %>%
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -160))
  })
  
  output$hand <- renderVisNetwork({
    nodes <- read.csv("Figure11_nodes.csv",encoding = "UTF-8")
    edges <- read.csv("Figure11_edges.csv",encoding = "UTF-8")
    
    visNetwork(nodes, edges)%>%
      visOptions(collapse = TRUE)%>%
      visEdges(arrows="to")
  })
  
  observeEvent(input$test,
            {
              hide("TopNetwork")
              show("HandNetwork")
            })
  
  observeEvent(input$back,
               {
                 hide("HandNetwork")
                 show("TopNetwork")
               })
  
}
shinyApp(ui = ui, server = server)

