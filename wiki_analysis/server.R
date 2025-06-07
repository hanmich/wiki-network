library(shiny)
library(igraph)
library(dplyr)
library(readr)
library(ggraph)
library(tidygraph)
library(utils)

# --- Load and clean base data ---
links <- read_tsv("network_data/wikispeedia_paths-and-graph/links.tsv", comment = "#", col_names = FALSE)
colnames(links) <- c("from", "to")
links <- links %>% filter(!is.na(from), !is.na(to))

articles <- read_tsv("network_data/wikispeedia_paths-and-graph/articles.tsv", comment = "#", col_names = FALSE)
colnames(articles) <- c("url_title")
articles$title <- URLdecode(articles$url_title)

categories <- read_tsv("network_data/wikispeedia_paths-and-graph/categories.tsv", comment = "#", col_names = FALSE)
colnames(categories) <- c("url_title", "category")
categories$title <- URLdecode(categories$url_title)

# --- Create graph ---
g <- graph_from_data_frame(links, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Louvain clustering
communities <- cluster_louvain(g)
V(g)$community <- communities$membership

# --- SHINY SERVER FUNCTION ---
shinyServer(function(input, output) {
  
  # Reactive: Top N node subgraph
  filtered_graph <- reactive({
    top_n <- input$top_n_nodes
    top_nodes <- names(sort(degree(g), decreasing = TRUE))[1:top_n]
    induced_subgraph(g, vids = top_nodes)
  })
  
  # --- Plot Output ---
  output$networkPlot <- renderPlot({
    subgraph <- filtered_graph()
    g_tbl <- as_tbl_graph(subgraph)
    
    # Data frame of node info
    node_df <- data.frame(
      name = V(subgraph)$name,
      community = V(subgraph)$community
    )
    node_df$title <- URLdecode(node_df$name)
    
    # Join categories to node titles
    node_cat <- left_join(node_df, categories, by = "title")
    
    # Most common category per community
    community_labels <- node_cat %>%
      group_by(community, category) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      group_by(community) %>%
      slice(1) %>%
      ungroup()
    
    # Merge labels back to node info
    node_df <- left_join(node_df, community_labels, by = "community")
    node_df$category <- ifelse(is.na(node_df$category), "Unlabeled", node_df$category)
    
    # Add labels to g_tbl using tidygraph
    g_tbl <- g_tbl %>%
      activate(nodes) %>%
      mutate(community_label = node_df$category)
    
    # Largest community for subtitle
    top_comm <- node_df %>%
      count(category, name = "Nodes") %>%
      arrange(desc(Nodes)) %>%
      slice(1)
    
    legend_text <- paste0("Largest community: ", top_comm$category, " (", top_comm$Nodes, " nodes)")
    
    # Render network plot
    ggraph(g_tbl, layout = "lgl") +
      geom_edge_link(alpha = 0.05) +
      geom_node_point(aes(color = factor(community_label)), size = 3) +
      theme_void() +
      labs(
        title = paste("Top", input$top_n_nodes, "Wikispeedia Nodes by Category"),
        subtitle = legend_text,
        color = "Community Label"
      ) +
      guides(color = guide_legend(override.aes = list(size = 4)))
  })
  
  # --- Table Output ---
  output$communityTable <- renderTable({
    subgraph <- filtered_graph()
    
    node_df <- data.frame(
      name = V(subgraph)$name,
      community = V(subgraph)$community
    )
    node_df$title <- URLdecode(node_df$name)
    node_df <- left_join(node_df, categories, by = "title")
    
    # Count articles per community label
    node_df %>%
      group_by(community) %>%
      count(category, name = "Articles") %>%
      arrange(community, desc(Articles)) %>%
      slice_head(n = 1) %>%
      ungroup()
  })
})
