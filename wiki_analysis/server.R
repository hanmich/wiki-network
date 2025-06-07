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

categories <- read_tsv("network_data/wikispeedia_paths-and-graph/categories.tsv", comment ="#", col_names = FALSE)
colnames(categories) <- c("url_title", "category")
categories$title <- URLdecode(categories$url_title)
categories$category <- gsub("^subject\\.", "", categories$category)
categories$category <- gsub("[._]", " ", categories$category)

# --- Create graph ---
g <- graph_from_data_frame(links, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# --- Shiny Server Function ---
shinyServer(function(input, output) {
  
  # Reactive: Top N node subgraph
  filtered_graph <- reactive({
    top_n <- input$top_n_nodes
    top_nodes <- names(sort(degree(g), decreasing = TRUE))[1:top_n]
    induced_subgraph(g, vids = top_nodes)
  })
  
  # --- Plot Output ---
  output$networkPlot <- renderPlot({
    set.seed(1234) 
    
    subgraph <- filtered_graph()
    
    # Apply selected community detection algorithm
    algo <- input$comm_algo
    if (algo == "Louvain") {
      community_struct <- cluster_louvain(subgraph)
    } else if (algo == "Infomap") {
      community_struct <- cluster_infomap(subgraph)
    } else if (algo == "Edge Betweenness") {
      community_struct <- cluster_edge_betweenness(subgraph)
    }
    
    V(subgraph)$community <- membership(community_struct)
    g_tbl <- as_tbl_graph(subgraph)
    
    # Node info
    node_df <- data.frame(
      name = V(subgraph)$name,
      community = V(subgraph)$community
    )
    node_df$title <- URLdecode(node_df$name)
    
    # Join with categories
    node_cat <- left_join(node_df, categories, by = "title")
    
    # Determine top category per community
    community_labels <- node_cat %>%
      group_by(community, category) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      group_by(community) %>%
      slice(1) %>%
      ungroup()
    
    node_df <- left_join(node_df, community_labels, by = "community")
    node_df$category <- ifelse(is.na(node_df$category), "Unlabeled", node_df$category)
    
    # Limit categories to top 10 (group rest as "Other")
    top_cats <- node_df %>%
      count(category) %>%
      arrange(desc(n)) %>%
      slice(1:10) %>%
      pull(category)
    
    node_df$category <- ifelse(node_df$category %in% top_cats, node_df$category, "Other")
    
    # Inject into graph
    g_tbl <- g_tbl %>%
      activate(nodes) %>%
      mutate(community_label = node_df$category)
    
    # Largest community info
    top_comm <- node_df %>%
      count(category, name = "Nodes") %>%
      arrange(desc(Nodes)) %>%
      slice(1)
    
    total_nodes <- nrow(node_df)
    legend_text <- paste0("Total nodes: ", total_nodes,
                          " — Largest community: ", top_comm$category,
                          " (", top_comm$Nodes, " nodes)")
    
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
        title = paste("Top", input$top_n_nodes, "Wikispeedia Nodes by Category"),
        subtitle = legend_text,
        color = "Community Label"
      ) +
      guides(
        color = guide_legend(
          override.aes = list(size = 5),
          title.position = "top",
          title.hjust = 0.5,
          ncol = 1
        )
      )
  })
  
  
  # --- Table Output ---
  output$communityTable <- renderTable({
    subgraph <- filtered_graph()
    
    # Apply selected community detection algorithm
    algo <- input$comm_algo
    if (algo == "Louvain") {
      community_struct <- cluster_louvain(subgraph)
    } else if (algo == "Infomap") {
      community_struct <- cluster_infomap(subgraph)
    } else if (algo == "Edge Betweenness") {
      community_struct <- cluster_edge_betweenness(subgraph)
    }
    
    V(subgraph)$community <- membership(community_struct)
    
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
