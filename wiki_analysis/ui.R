library(shiny)

shinyUI(fluidPage(
  titlePanel("Wikispeedia Community Detection Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Project Goals"),
      p("Explore Wikipedia's hyperlink network using community detection."),
      p("We apply the Louvain algorithm to reveal clusters of related articles."),
      selectInput("top_n_nodes", "Number of nodes to visualize:",
                  choices = c(100, 200, 300, 500), selected = 200)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network Visualization", 
                 plotOutput("networkPlot", height = "650px")),
        tabPanel("Community Stats", 
                 tableOutput("communityTable")),
        tabPanel("About", 
                 h4("Project Questions"),
                 p("• What communities or clusters of articles form based on hyperlink structure?"),
                 p("• What types of topics dominate each community?"),
                 p("• Which nodes are most central within each community?"))
      )
    )
  )
))
