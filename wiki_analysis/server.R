library(shiny)
library(igraph)
library(dplyr)
library(readr)
library(ggraph)
library(tidygraph)
library(utils)

# --- Load and prepare data ---
links <- read_tsv("network_data/wikispeedia_paths-and-graph/links.tsv", comment = "#", col_names = FALSE)
colnames(links) <- c("from", "to")
links <- links %>% filter(!is.na(from), !is.na(to))

articles <- read_tsv("network_data/wikispeedia_paths-and-graph/articles.tsv", comment = "#", col_names = FALSE)
colnames(articles) <- c("url_title")
articles$title <- URLdecode(articles$url_title)

categories <- read_tsv("network_data/wikispeedia_paths-and-graph/categories.tsv", comment = "#", col_names = FALSE)
colnames(categories) <- c("url_title", "category")
categories$title <- URLdecode(categories$url_title)
categories$category <- gsub("^subject\\.", "", categories$category)
categories$category <- gsub("[._]", " ", categories$category)

g <- graph_from_data_frame(links, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# --- Shiny Server ---
shinyServer(function(input, output) {
  
  filtered_graph <- reactive({
    top_n <- input$top_n_nodes
    top_nodes <- names(sort(degree(g), decreasing = TRUE))[1:top_n]
    induced_subgraph(g, vids = top_nodes)
  })
  
  output$networkPlot <- renderPlot({
    set.seed(1234)
    subgraph <- filtered_graph()
    
    # Apply algorithm
    algo <- input$comm_algo
    community_struct <- switch(algo,
                               "Louvain" = cluster_louvain(subgraph),
                               "Infomap" = cluster_infomap(subgraph),
                               "Edge Betweenness" = cluster_edge_betweenness(subgraph))
    
    V(subgraph)$community <- membership(community_struct)
    g_tbl <- as_tbl_graph(subgraph)
    
    node_df <- data.frame(
      name = V(subgraph)$name,
      community = V(subgraph)$community
    )
    node_df$title <- URLdecode(node_df$name)
    
    node_cat <- left_join(node_df, categories, by = "title")
    community_labels <- node_cat %>%
      group_by(community, category) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      group_by(community) %>%
      slice(1) %>%
      ungroup()
    
    node_df <- left_join(node_df, community_labels, by = "community")
    node_df$category <- ifelse(is.na(node_df$category), "Unlabeled", node_df$category)
    
    top_cats <- node_df %>%
      count(category) %>%
      arrange(desc(n)) %>%
      slice(1:10) %>%
      pull(category)
    
    node_df$category <- ifelse(node_df$category %in% top_cats, node_df$category, "Other")
    
    g_tbl <- g_tbl %>%
      activate(nodes) %>%
      mutate(community_label = node_df$category)
    
    top_comm <- node_df %>%
      count(category, name = "Nodes") %>%
      arrange(desc(Nodes)) %>%
      slice(1)
    
    legend_text <- paste0("Top ", input$top_n_nodes, " nodes — Largest community: ",
                          top_comm$category, " (", top_comm$Nodes, " nodes)")
    
    ggraph(g_tbl, layout = "lgl") +
      geom_edge_link(alpha = 0.05) +
      geom_node_point(aes(color = factor(community_label)), size = 3) +
      scale_color_brewer(palette = "Set3") +
      theme_void() +
      theme(
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      ) +
      labs(
        title = paste("Wikispeedia Article Clusters by", algo),
        subtitle = legend_text,
        color = "Dominant Category"
      ) +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })
  
  output$communityTable <- renderTable({
    subgraph <- filtered_graph()
    algo <- input$comm_algo
    community_struct <- switch(algo,
                               "Louvain" = cluster_louvain(subgraph),
                               "Infomap" = cluster_infomap(subgraph),
                               "Edge Betweenness" = cluster_edge_betweenness(subgraph))
    
    V(subgraph)$community <- membership(community_struct)
    
    node_df <- data.frame(
      name = V(subgraph)$name,
      community = V(subgraph)$community
    )
    node_df$title <- URLdecode(node_df$name)
    node_df <- left_join(node_df, categories, by = "title")
    
    node_df %>%
      group_by(community) %>%
      count(category, name = "Articles") %>%
      arrange(community, desc(Articles)) %>%
      slice_head(n = 1) %>%
      ungroup()
  })
  
  output$codeChunk <- renderPrint({
    cat("## Libraries and Setup Code\n")
    cat("library(igraph)\nlibrary(readr)\nlibrary(ggraph)\nlibrary(tidygraph)\n\n")
    cat("## Data Files Used:\n")
    cat("• links.tsv — edges between articles\n")
    cat("• articles.tsv — article titles\n")
    cat("• categories.tsv — article categories\n")
    cat("\nSource: https://snap.stanford.edu/data/wikispeedia.html\n")
  })
})
