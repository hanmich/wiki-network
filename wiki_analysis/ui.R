library(shiny)

shinyUI(fluidPage(
  titlePanel("Wikispeedia Community Detection Project"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Project Settings"),
      selectInput("top_n_nodes", "Number of Nodes:", choices = c(100, 200, 300, 500), selected = 200),
      selectInput("comm_algo", "Community Detection Method:",
                  choices = c("Louvain", "Infomap", "Edge Betweenness"),
                  selected = "Louvain"),
      hr(),
      h4("About this Dashboard"),
      p("Built with R Shiny | Data from Stanford SNAP Wikispeedia dataset"),
      tags$a(href = "https://snap.stanford.edu/data/wikispeedia.html", "Dataset Link", target = "_blank")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. Summary",
                 h3("Overview"),
                 p("This dashboard explores Wikipedia’s hyperlink network using community detection algorithms. The goal is to uncover emergent topic clusters."),
                 
                 h4("Data Description"),
                 p("• Nodes represent Wikipedia articles"),
                 p("• Edges represent user-clickable hyperlinks between articles"),
                 p("• Categories reflect thematic labels assigned to articles"),
                 p("• Data was collected by the Stanford SNAP Group from a gamified Wikipedia navigation task"),
                 
                 h4("Setup & Libraries"),
                 verbatimTextOutput("codeChunk")
        ),
        
        tabPanel("2. Network Visualization",
                 h3("Community Detection Visualization"),
                 p("This plot displays the top N most connected nodes in the article graph. Nodes are colored by their community (inferred via the selected algorithm), and communities are labeled with their dominant article category."),
                 plotOutput("networkPlot", height = "700px")
        ),
        
        tabPanel("3. Community Stats",
                 h3("Top Categories by Community"),
                 p("This table summarizes the most common article category within each detected community."),
                 tableOutput("communityTable")
        ),
        
        tabPanel("4. Takeaways",
                 h3("Conclusion"),
                 p("The structure of Wikipedia reveals strong clustering by subject matter, even in user-navigation data."),
                 p("Different algorithms reveal variations in cluster size and granularity."),
                 p("This approach could extend to user-behavior modeling or educational topic mapping in hyperlink networks.")
        )
      )
    )
  )
))
