# 05_plotting_functions_interactive.R - Interactive Plotting Functions with Plotly
# This file contains interactive plotting functions using Plotly

# Create interactive poverty overlap barplot
create_poverty_overlap_plot_interactive <- function(data) {
  data <- data %>%
    mutate(
      poverty = as.numeric(as.character(poverty)),
      adversity = as.numeric(as.character(adversity))
    )
  
  poverty_only <- sum(data$poverty == 1 & (is.na(data$adversity) | data$adversity == 0), na.rm = TRUE)
  poverty_adversity <- sum(data$poverty == 1 & data$adversity == 1, na.rm = TRUE)
  
  poverty_data <- data.frame(
    category = c("Poverty only", "Poverty + Adversity"),
    count = c(poverty_only, poverty_adversity),
    filter_type = c("poverty_only", "poverty_adversity")
  )
  
  plot_ly(poverty_data, 
          x = ~category, 
          y = ~count,
          type = 'bar',
          marker = list(color = THEME_CONFIG$primary_colors[1:2]),
          text = ~count,
          textposition = 'outside',
          customdata = ~filter_type,
          hovertemplate = paste('<b>%{x}</b><br>',
                                'Count: %{y}<br>',
                                '<extra></extra>'),
          source = "overlap_plot") %>%
    layout(
      title = list(text = "Overlap in the literature", 
                   font = list(size = 18, color = THEME_CONFIG$text_primary)),
      xaxis = list(title = ""),
      yaxis = list(title = "Number of articles"),
      plot_bgcolor = THEME_CONFIG$bg_primary,
      paper_bgcolor = THEME_CONFIG$bg_primary,
      font = list(family = THEME_CONFIG$font_family, color = THEME_CONFIG$text_primary)
    )
}

# Create network diagram (with 'synaptic' filtered out and updated colors)
create_network_diagram <- function(data) {
  library(igraph)
  library(visNetwork)
  
  # Prepare data - exclude 'synaptic' (infant brain connectivity)
  all_vars <- unlist(VARIABLE_CATEGORIES)
  all_vars <- all_vars[all_vars != "synaptic"]  # Remove synaptic
  existing_vars <- intersect(all_vars, names(data))
  
  # Create co-occurrence matrix
  poverty_data <- data %>%
    filter(poverty == 1) %>%
    select(all_of(existing_vars))
  
  # Convert to numeric matrix
  data_matrix <- as.matrix(poverty_data)
  data_matrix[data_matrix != 1] <- 0
  data_matrix <- apply(data_matrix, 2, as.numeric)
  
  # Remove columns with no occurrences
  node_counts <- colSums(data_matrix, na.rm = TRUE)
  active_vars <- names(node_counts)[node_counts > 0]
  
  if (length(active_vars) == 0) {
    return(visNetwork(data.frame(id = 1, label = "No data"), data.frame()))
  }
  
  # Filter matrix to only active variables
  data_matrix <- data_matrix[, active_vars]
  
  # Calculate co-occurrence
  cooc_matrix <- t(data_matrix) %*% data_matrix
  diag(cooc_matrix) <- 0
  
  # Create edge list (threshold = 2)
  threshold <- 2
  edges <- which(cooc_matrix >= threshold, arr.ind = TRUE)
  edges <- edges[edges[,1] < edges[,2], , drop = FALSE]
  
  if(nrow(edges) > 0) {
    edge_weights <- cooc_matrix[edges]
    
    # Scale edge widths
    min_width <- 0.5
    max_width <- 5
    edge_widths <- if (max(edge_weights) > min(edge_weights)) {
      min_width + (edge_weights - min(edge_weights)) * 
        (max_width - min_width) / (max(edge_weights) - min(edge_weights))
    } else {
      rep(2, length(edge_weights))
    }
    
    edge_list <- data.frame(
      from = colnames(cooc_matrix)[edges[,1]],
      to = colnames(cooc_matrix)[edges[,2]],
      value = edge_widths,
      title = paste("Co-occurrences:", edge_weights),
      color = list(color = alpha(THEME_CONFIG$text_secondary, 0.3)),
      stringsAsFactors = FALSE
    )
    
    # Create nodes with updated lighter colors
    node_counts_active <- node_counts[active_vars]
    nodes <- data.frame(
      id = names(node_counts_active),
      label = VARIABLE_LABELS[names(node_counts_active)],
      value = sqrt(node_counts_active) * 10,
      group = case_when(
        names(node_counts_active) %in% VARIABLE_CATEGORIES$external_system ~ "External",
        names(node_counts_active) %in% VARIABLE_CATEGORIES$internal_system ~ "Internal",
        names(node_counts_active) %in% VARIABLE_CATEGORIES$cellular ~ "Cellular"
      ),
      title = paste0(VARIABLE_LABELS[names(node_counts_active)], 
                     "<br>Papers: ", node_counts_active,
                     "<br>Click to filter"),
      font = list(size = 18),
      stringsAsFactors = FALSE
    )
    
    # Create the network - reverted to original working format
    visNetwork(nodes, edge_list) %>%
      visGroups(groupname = "External", color = THEME_CONFIG$primary_colors[1]) %>%
      visGroups(groupname = "Internal", color = THEME_CONFIG$primary_colors[3]) %>%
      visGroups(groupname = "Cellular", color = THEME_CONFIG$primary_colors[5]) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        selectedBy = "group",
        nodesIdSelection = TRUE
      ) %>%
      visEdges(smooth = list(type = "continuous")) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -100,
          centralGravity = 0.005,
          springLength = 200,
          springConstant = 0.05,
          avoidOverlap = 1
        ),
        stabilization = list(
          enabled = TRUE,
          iterations = 2000,
          updateInterval = 100
        )
      ) %>%
      visInteraction(
        hover = TRUE,
        hoverConnectedEdges = TRUE,
        navigationButtons = TRUE,
        zoomView = TRUE,
        dragNodes = TRUE
      ) %>%
      visNodes(
        shape = "dot",
        borderWidth = 2,
        borderWidthSelected = 4,
        font = list(
          size = 18,
          face = "arial", 
          strokeWidth = 5,
          strokeColor = "white",
          background = "rgba(255, 255, 255, 0.9)",
          vadjust = -12
        )
      )
  } else {
    # Return network with no edges
    node_counts_active <- node_counts[active_vars]
    nodes <- data.frame(
      id = names(node_counts_active),
      label = VARIABLE_LABELS[names(node_counts_active)],
      value = sqrt(node_counts_active) * 10,
      group = case_when(
        names(node_counts_active) %in% VARIABLE_CATEGORIES$external_system ~ "External",
        names(node_counts_active) %in% VARIABLE_CATEGORIES$internal_system ~ "Internal",
        names(node_counts_active) %in% VARIABLE_CATEGORIES$cellular ~ "Cellular"
      ),
      title = paste0(VARIABLE_LABELS[names(node_counts_active)], 
                     "<br>Papers: ", node_counts_active),
      font = list(size = 18),
      stringsAsFactors = FALSE
    )
    
    visNetwork(nodes, data.frame()) %>%
      visGroups(groupname = "External", color = THEME_CONFIG$primary_colors[1]) %>%
      visGroups(groupname = "Internal", color = THEME_CONFIG$primary_colors[3]) %>%
      visGroups(groupname = "Cellular", color = THEME_CONFIG$primary_colors[5]) %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -100,
          avoidOverlap = 1
        )
      ) %>%
      visNodes(
        shape = "dot",
        borderWidth = 2,
        font = list(
          size = 18,
          face = "arial", 
          strokeWidth = 5,
          strokeColor = "white",
          background = "rgba(255, 255, 255, 0.9)",
          vadjust = -12
        )
      )
  }
}