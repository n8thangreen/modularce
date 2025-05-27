# functions

##############
# models

# define S3 class
# constructors

#' @export
DecisionTree <- function(data, ...) {
  UseMethod("DecisionTree")
}

#' @export
#' @param N Total sample size
DecisionTree.default <- function(data, N = NA, ...) {
  call_obj <- match.call()
  structure(list(data = data,
                 N = N),
            call = call_obj,
            class = c("DecisionTree", "Model"))
}

#' @export
MarkovModel <- function(model, ...) {
  UseMethod("MarkovModel")
}

#' @export
MarkovModel.default <- function(model = NA,
                                init_probs = NA,
                                trans_matrix = NA,
                                cost_matrix = NA,
                                q_matrix = NA,
                                mapping = NA,
                                N = NA,
                                n_cycles = 10, ...) {
  call_obj <- match.call()
  structure(list(init_probs   = init_probs,
                 trans_matrix = trans_matrix,
                 cost_matrix  = cost_matrix,
                 q_matrix     = q_matrix,
                 mapping      = mapping,
                 N            = N,
                 n_cycles     = n_cycles),
            call = call_obj,
            class = c("MarkovModel", "Model"))
}


###############
# constructors

# Combine all models in to a single list
#' @export
CombinedModel <- function(...) {
  models <- list(...)
  
  if (length(models) < 2) {
    stop("At least two models must be provided.")
  }
  
  if (!all(sapply(models, function(m) inherits(m, "Model")))) {
    stop("All arguments must be of class 'Model'")
  }
  
  structure(models, class = "CombinedModel")
}

# provide additional components needed for each model
#' @export
update_model <- function(model, result) {
  UseMethod("update_model")
}

# Markov model
#' @export
update_model.MarkovModel <- function(model, result) {
  model$init_probs <- map_decision_to_markov(result, model$mapping)
  model
}

# Decision tree
#' @export
update_model.DecisionTree <- function(model, result) {
  model$data <- map_markov_to_decision(model, result)
  model
}

#' @export
update_model.default <- function(model, result) {
  warning("No update method defined for this model type. Returning model unmodified.")
  model
}

###########
# mappings

# how does the output of one model plug into the input of the next model? 

# from decision tree to Markov model
#' @return a list of vectors of probabilities by treatment
#' @export
map_decision_to_markov <- function(decision_result, mapping) {
  probs <- decision_result$terminal_prob
  lapply(probs, \(x) tapply(x, mapping, sum))
}

# from Markov model to decision tree
#' @export
map_markov_to_decision <- function(dt_model, markov_result) {
  mm_probs <- markov_results$terminal_probs
  
  ##TODO: ensure correct order
  dt_model$data |> 
    mutate(to = ifelse(from = "root",
                       yes = mm_probs,
                       no = prob))
}

##########
# runners

# loop through each sequential submodel
#' @export
run_model.CombinedModel <- function(model_chain) {
  result <- list()
  
  for (i in seq_along(model_chain)) {
    current_model <- model_chain[[i]]
    
    if (i > 1) {
      # Update model via S3 dispatch
      current_model <- update_model(current_model, result[[i - 1]])
    }
    
    result[[i]] <- run_model(current_model)
  }
  
  structure(result,
            class = c("output", class(model_chain)))
}

# run a submodel
#' @export
run_model <- function(model, ...) {
  UseMethod("run_model")
}

# ai generated
calculate_pathways <- function(tree_data) {
  
  # Get unique treatments
  treatments <- unique(tree_data$treatment)
  
  all_path_results <- list()
  
  for (trt in treatments) {
    # Filter tree for the current treatment
    current_tree <- tree_data %>% filter(treatment == trt)
    
    # Initialize paths with root nodes
    # Each path will be a list containing:
    # - current_node: the last node reached
    # - path_nodes: vector of nodes in the path
    # - path_prob: cumulative probability along the path
    # - path_cost: cumulative cost along the path
    # - path_eff: cumulative effect along the path
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
    
    # Iteratively expand paths until no more transitions are possible
    while (length(paths) > 0) {
      current_path <- paths[[1]]
      paths <- paths[-1] # Remove the current path from the list to process
      
      from_node <- current_path$current_node
      
      # Find all transitions originating from the current node
      transitions <- current_tree %>% filter(from == from_node)
      
      if (nrow(transitions) == 0) {
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
        path_prob = .x$path_prob,
        path_cost = .x$path_cost,
        path_eff = .x$path_eff
      ))
    
    all_path_results[[trt]] <- path_df
  }
  
  return(bind_rows(all_path_results))
}


#' @import dplyr
#' @export
run_model.DecisionTree <- function(model) {
  
  path_results <- calculate_pathways(model)
  
  res <- 
    path_results %>%
    group_by(treatment) %>%
    summarise(
      expected_cost_overall = sum(path_prob * path_cost),
      expected_eff_overall = sum(path_prob * path_eff)
    )
  
  structure(
    res,
    class = c("output", class(model)))
}

#' @export
run_model.MarkovModel <- function(model) {
  
  init_probs <- model$init_probs
  
  res <- data.frame(
    decision = c("Treatment A", "Treatment B"),
    expected_cost = c(100, 100),
    expected_eff = c(1,1))
  
  if (!is.na(model$N)) {
    res$expected_cost <- res$expected_cost * model$N
    res$expected_eff <- res$expected_eff * model$N
  }
  
  structure(res,
            class = c("output", class(model)))
}

# # wrapper for adopting function
# run_model.MarkovModel <- function(model) {
#   
#   out <- markov_model(start_pop = model$init_probs,
#                       p_matrix = model$trans_matrix,
#                       state_c_matrix = model$cost_matrix,
#                       state_q_matrix = model$q_matrix,
#                       n_cycles = 1,
#                       init_age = 55)
# 
#   # transform
#   res$decision <- names(out$total_costs)
#   res$terminal_prob <- out$pop
#   res$expected_cost <- out$total_costs
#   res$expected_eff <- out$total_QALYS
#   
#   structure(res,
#             class = c("output", class(model)))
# }

# take the output of a model run and
# return cost effectiveness values
#' @export
analysis <- function(results, ...) {
  c(get_costs(results),
    get_effects(results))
}

##########
# helpers

#' @export
get_costs <- function(results, ...) {
  UseMethod("get_costs")
}

#' @export
get_costs.default <- function(results) {
  stop("No method for this model")
}

#' @export
get_costs.DecisionTree <- function(results) {
  results$expected_cost
}

#' @export
get_costs.MarkovModel <- function(results) {
  results$expected_cost
}

#' @export
get_costs.CombinedModel <- function(results) {
  
  total_cost <- 0
  for (i in seq_along(results)) {
    total_cost <- total_cost + get_costs(results[[i]])
  }
  total_cost
}

#' @export
get_effects <- function(results, ...) {
  UseMethod("get_effects")
}

#' @export
get_effects.default <- function(results) {
  stop("No method for this model")
}

#' @export
get_effects.DecisionTree <- function(results) {
  results$expected_eff
}

#' @export
get_effects.MarkovModel <- function(results) {
  results$expected_eff
}

#' @export
get_effects.CombinedModel <- function(results) {
  
  total_eff <- 0
  for (i in seq_along(results)) {
    total_eff <- total_eff + get_effects(results[[i]])
  }
  total_eff
}
