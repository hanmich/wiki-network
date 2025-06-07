library(shiny)
library(igraph)
library(dplyr)
library(readr)
library(ggraph)
library(tidygraph)

# Load and clean data
links <- read_tsv("network_data/wikispeedia_paths-and-graph/links.tsv", comment = "#", col_names = FALSE)
colnames(links) <- c("from", "to")

articles <- read_tsv("network_data/wikispeedia_paths-and-graph/articles.tsv", comment = "#", col_names = FALSE)
colnames(articles) <- c("title")

links <- links %>% filter(!is.na(from), !is.na(to))

# Create undirected graph
g <- graph_from_data_frame(links, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Louvain clustering
communities <- cluster_louvain(g)
V(g)$community <- communities$membership

# SHINY SERVER FUNCTION
shinyServer(function(input, output) {
  
  # Reactive filtered graph based on user-selected top-N
  filtered_graph <- reactive({
    top_n <- input$top_n_nodes
    top_nodes <- names(sort(degree(g), decreasing = TRUE))[1:top_n]
    induced_subgraph(g, vids = top_nodes)
  })
  
  # Render graph plot
  output$networkPlot <- renderPlot({
    subgraph <- filtered_graph()
    g_tbl <- as_tbl_graph(subgraph)
    
    # Get community membership again just in case node subset changed
    community_membership <- V(subgraph)$community
    community_df <- data.frame(community = community_membership) %>%
      count(community, name = "size") %>%
      arrange(desc(size))
    
    largest_community <- community_df$community[1]
    legend_text <- paste0("Largest community: ", largest_community,
                          " (", community_df$size[1], " nodes)")
    
    ggraph(g_tbl, layout = "lgl") +
      geom_edge_link(alpha = 0.05) +
      geom_node_point(aes(color = as.factor(community)), size = 3) +
      theme_void() +
      labs(title = paste0("Top ", input$top_n_nodes, " Wikispeedia Nodes by Degree"),
           subtitle = legend_text,
           color = "Community ID") +
      guides(color = guide_legend(override.aes = list(size = 4)))
  })
  
  output$communityTable <- renderTable({
  subgraph <- filtered_graph()
  community_df <- data.frame(community = V(subgraph)$community) %>%
    count(community, name = "Nodes") %>%
    arrange(desc(Nodes))
  community_df
  })

})
