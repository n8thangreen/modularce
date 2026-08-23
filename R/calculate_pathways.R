
# ai generated
calculate_pathways <- function(model) {
  tree_data <- model$data
  
  treatments <- unique(tree_data$treatment)
  
  all_path_results <- list()
  
  for (trt in treatments) {
    # Filter tree for the current treatment
    current_tree <- tree_data %>% filter(treatment == trt)
    
    # Initialize paths with root nodes
    paths <- list(
      list(
        current_node = "root",
        path_nodes = "root",
        path_prob = 1, # Probability of starting at root is 1
        path_cost = 0,
        path_eff = 0
      )
    )
    
    completed_paths <- list()
    
    # Pre-process transitions by 'from' node to avoid dplyr filtering in the loop
    transitions_by_from <- split(current_tree, current_tree$from)

    # Iteratively expand paths until no more transitions are possible
    while (length(paths) > 0) {
      current_path <- paths[[1]]
      paths <- paths[-1] # Remove the current path from the list to process
      
      from_node <- current_path$current_node
      
      # Find all transitions originating from the current node using pre-processed list
      transitions <- transitions_by_from[[from_node]]
      
      if (is.null(transitions) || nrow(transitions) == 0) {
        # No more transitions from this node, so this path is complete
        completed_paths <- append(completed_paths, list(current_path))
      } else {
        # Expand current path with each possible transition
        for (i in 1:nrow(transitions)) {
          transition <- transitions[i, ]
          new_path <- list(
            current_node = transition$to,
            path_nodes = c(current_path$path_nodes, transition$to),
            path_prob = current_path$path_prob * transition$prob,
            path_cost = current_path$path_cost + transition$cost,
            path_eff = current_path$path_eff + transition$eff
          )
          paths <- append(paths, list(new_path))
        }
      }
    }
    
    # Convert completed paths to a tibble for the current treatment
    path_df <- completed_paths %>%
      purrr::map_df(~tibble(
        treatment = trt,
        path_nodes = paste(.x$path_nodes, collapse = " -> "),
        terminal_node = last(.x$path_nodes),
        terminal_prob = .x$path_prob,
        path_cost = .x$path_cost,
        path_eff = .x$path_eff
      ))
    
    all_path_results[[trt]] <- path_df
  }
  
  return(bind_rows(all_path_results))
}
