---
title: "Wiki Community Detection Project"
author: "Michael Han"
date: "2025-06-06"
output: 
  html_document:
    theme: cosmo
    highlight: tango
    toc: true
    toc_float: true
    code_folding: show
---

## Narrative Summary 

The project explores the hyperlink network of Wikipedia's articles using the [Wikispeedia Dataset](https://snap.stanford.edu/data/wikispeedia.html) which was collected by Stanford SNAP. The analysis for this data set uses network visualization and multiple community detection methods to distinguish the patterns and thematic groupings apparent in the articles. The data from the wikispeedia more specifically used was the paths and graphs data. With the specific files used in analysis being the articles, categories, and links. With that data, the community detection methods used were the Louvain and Infomap. The goal of this analysis is to understand what article topics are most prevelant.

## Setup & Data Usage

```{r setup, message = FALSE, warning = FALSE}
library(igraph)
library(dplyr)
library(readr)
library(tidygraph)
library(ggraph)
library(utils)

# --- Read & Prepare the data ---

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

# --- Graph creation ---

g <- graph_from_data_frame(links, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
```

**Vertices**: Wikipedia articles  
**Edges**: Hyperlinks between articles  
**Collected by**: Stanford SNAP Group  
**Purpose**: User navigation in a Wikipedia-based game  
**Access**: [https://snap.stanford.edu/data/wikispeedia.html](https://snap.stanford.edu/data/wikispeedia.html)

## Network Visualizations

```{r louvain, message = FALSE, warning = FALSE}
set.seed(1234)

subgraph <- induced_subgraph(g, vids = names(sort(degree(g), decreasing = TRUE))[1:200])
communities_louvain <- cluster_louvain(subgraph)
V(subgraph)$community <- membership(communities_louvain)

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

ggraph(g_tbl, layout = "lgl") +
  geom_edge_link(alpha = 0.05) +
  geom_node_point(aes(color = factor(community_label)), size = 3) +
  scale_color_brewer(palette = "Set3") +
  theme_void() +
  labs(title = "Louvain Community Detection (Top 200 Nodes)",
       subtitle = "Communities colored by dominant article category",
       color = "Category")
```

```{r infomap, message = FALSE, warning = FALSE}
set.seed(1234)

subgraph <- induced_subgraph(g, vids = names(sort(degree(g), decreasing = TRUE))[1:200])
communities_infomap <- cluster_infomap(subgraph)
V(subgraph)$community <- membership(communities_infomap)

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

ggraph(g_tbl, layout = "lgl") +
  geom_edge_link(alpha = 0.05) +
  geom_node_point(aes(color = factor(community_label)), size = 3) +
  scale_color_brewer(palette = "Set2") +
  theme_void() +
  labs(title = "Infomap Community Detection (Top 200 Nodes)",
       subtitle = "Communities colored by dominant article category",
       color = "Category")

```

## Analysis of Network Visualizations

The project looks at network visualization strategies to understand Wikipedia's hyperlink network. Through the use of two community detection algorithms, the Louvain and Infomap looked at the top 200 connected articles in the Wikispeedia data set. With these algorithms, it helped identify clusters of the tightly connected nodes to see topical groupings. 

### Louvain Method

The Louvain algorithm helps visualize community structures in the network.  In the plot, one of the largest communities corresponded to articles about Countries, while other communities centered around Geography, Religion, and General History. These clusters likely reflect user navigation patterns across thematically grouped articles. The use of the lgl layout made the structure of these dense interconnections more visually interpretable.

### Infomap Method

The Infomap algorithm groups articles based on how information flows through the network, similar to how a reader might navigate from one article to another. When applied to the top 200 most connected articles, it detected only two large communities. Suggesting that many of the articles are part of a tightly connected core, with fewer distinct topic clusters compared to the Louvain method. Infomap may be better at capturing larger structural divisions, but it can miss smaller topic groupings when the number of nodes is limited. Using more nodes or different settings could reveal finer details.

## Conclusion

This project demonstrates that Wikipedia's article hyperlink network naturally forms community clusters around related topics. Even with a limited sample of the top 200 most connected nodes, both Louvain and Infomap identified meaningful groupings aligned with article categories, such as Countries, Geography, and Religion. This reflects users' tendency to navigate semantically related articles, validating the use of network structure as a proxy for topic similarity.

However, this analysis has some limitations:

- Only a small subset (top 200 nodes) was analyzed to maintain visual clarity and app performance.

- Infomap’s coarse clustering at this scale may under-represent finer substructures.

- Category labels were inferred from external mappings and may contain inconsistencies.

Future work could expand the node subset, explore hierarchical community structures, or incorporate centrality measures (like betweenness or closeness) to identify key bridging articles. Additionally, visualizing user navigation paths (using the paths_finished.tsv dataset) could offer deeper behavioral insights beyond static hyperlink structures.

