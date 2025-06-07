library(shiny)

shinyUI(fluidPage(
  titlePanel("Wikispeedia Community Detection Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Project Goals"),
      p("Explore Wikipedia article network through community detection."),
      p("Use Louvain algorithm on top N most-connected pages."),
      selectInput("top_n_nodes", "Number of nodes to visualize:",
                  choices = c(100, 200, 300, 500), selected = 200)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network Visualization", plotOutput("networkPlot")),
        tabPanel("Community Stats", tableOutput("communityTable")),
        tabPanel("About", 
                 h4("Questions"),
                 p("• What communities or clusters of articles form?"),
                 p("• Which nodes are most central in the hyperlink structure?"))
      )
    )
  )
))
