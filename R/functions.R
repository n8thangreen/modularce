# main modelling functions

##############
# models

# define S3 class
# constructors

#' Create a DecisionTree model
#' @param data Dataframe containing decision tree structure
#' @param ... Additional arguments
#' @export
DecisionTree <- function(data, ...) {
  UseMethod("DecisionTree")
}

#' Default DecisionTree constructor
#' @param data Dataframe containing decision tree structure
#' @param N Total sample size
#' @param ... Additional arguments
#' @export
DecisionTree.default <- function(data, N = NA, ...) {
  call_obj <- match.call()
  structure(list(data = data,
                 N = N),
            call = call_obj,
            class = c("DecisionTree", "Model"))
}

#' Create a MarkovModel model
#' @param model Optional existing model
#' @param ... Additional arguments
#' @export
MarkovModel <- function(model, ...) {
  UseMethod("MarkovModel")
}

#' Default MarkovModel constructor
#' @param model Optional existing model
#' @param init_probs Initial state probability array
#' @param trans_matrix Transition probability array
#' @param cost_matrix Cost array
#' @param q_matrix Quality of life (QALY) matrix
#' @param mapping Mapping vector from decision tree nodes to Markov states
#' @param N Total sample size
#' @param n_cycles Number of Markov cycles
#' @param ... Additional arguments
#' @export
MarkovModel.default <- function(model = NA,
                                init_probs = NA,
                                trans_matrix = NA,
                                cost_matrix = NA,
                                q_matrix = NA,
                                mapping = NA,
                                N = NA,
                                n_cycles = 2, ...) {
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

#' Combine all submodels in to a single list
#' @param ... Submodels to combine
#' @export
CombinedModel <- function(...) {
  models <- list(...)
  
  if (length(models) < 2) {
    stop("At least two models must be provided.")
  }
  
  if (!all(sapply(models, function(m) inherits(m, c("CombinedModel", "Model"))))) {
    stop("All arguments must be of class 'Model'")
  }
  
  structure(models, class = "CombinedModel")
}

#' Provide additional components needed for each model
#' @param model Model to update
#' @param result Output from previous model
#' @export
update_model <- function(model, result) {
  UseMethod("update_model")
}

#' Markov model update method
#' @param model MarkovModel object
#' @param result Output object from previous model
#' @export
update_model.MarkovModel <- function(model, result) {
  if (!is.null(result$path_results) && !is.null(model$mapping)) {
    model$init_probs <- map_decision_to_markov(result, model$mapping)
  }
  model
}

#' Decision tree update method
#' @param model DecisionTree object
#' @param result Output object from previous model
#' @export
update_model.DecisionTree <- function(model, result) {
  if (!is.null(result$terminal)) {
    model$data <- map_markov_to_decision(model, result)
  }
  model
}

#' Default update method
#' @param model Model object
#' @param result Result object
#' @export
update_model.default <- function(model, result) {
  warning("No update method defined for this model type. Returning model unmodified.")
  model
}

###########
# mappings

# how does the output of one model plug into the input of the next model? 

#' Map Decision Tree Terminal Node Probabilities to Markov Model Initial States
#' 
#' Aggregates the terminal node probabilities from a Decision Tree output according
#' to a mapping vector, and formats them into a 3D array `[1, states, treatments]`
#' suitable as initial state probabilities (`init_probs`) for a Markov model.
#' 
#' @param decision_result Output object from `run_model(decision_tree)` containing `$path_results`
#' @param mapping Named vector mapping Decision Tree `terminal_node` names to Markov `state` names
#' @return 3D array of dimensions `c(1, n_states, n_treatments)` with initial state probabilities
#' @export
map_decision_to_markov <- function(decision_result, mapping) {
  # 1. Identify all unique Markov target states and treatments
  states <- if (is.factor(mapping)) levels(mapping) else unique(unname(mapping))
  treatments <- unique(decision_result$path_results$treatment)
  
  # 2. Match each decision tree terminal node to its mapped Markov state
  pathways <- decision_result$path_results
  pathways$state <- mapping[pathways$terminal_node]
  
  # 3. Sum probabilities for each (treatment, state) combination
  state_probabilities <- pathways |>
    dplyr::group_by(treatment, state) |>
    dplyr::summarise(prob = sum(terminal_prob), .groups = "drop")
  
  # 4. Construct the 3D array [1, n_states, n_treatments]
  init_array <- array(
    data = 0,
    dim = c(1, length(states), length(treatments)),
    dimnames = list(NULL, state = states, treatment = treatments)
  )
  
  # 5. Populate array cells by treatment and state name
  for (trt in treatments) {
    trt_data <- state_probabilities[state_probabilities$treatment == trt, ]
    valid_states <- intersect(trt_data$state, states)
    if (length(valid_states) > 0) {
      match_rows <- match(valid_states, trt_data$state)
      init_array[1, valid_states, trt] <- trt_data$prob[match_rows]
    }
  }
  
  init_array
}

#' Map Markov Model Ending States to Decision Tree Branch Probabilities
#' 
#' Updates the transition probabilities (`prob`) in a Decision Tree model data table
#' using the final state occupancy probabilities from a Markov model output.
#' 
#' @param dt_model DecisionTree model object containing `$data`
#' @param markov_result Output object from `run_model(markov_model)` containing `$terminal`
#' @return Updated decision tree data frame with adjusted branch probabilities
#' @export
map_markov_to_decision <- function(dt_model, markov_result) {
  # 1. Extract Markov end-cycle state occupancy probabilities and clarify column names
  markov_terminal <- markov_result$terminal |>
    dplyr::rename(markov_prob = probs)
  
  # 2. Join Markov probabilities onto matching decision tree branches ('to' node == Markov 'state')
  updated_data <- dt_model$data |>
    dplyr::left_join(
      markov_terminal,
      by = c("to" = "state", "treatment")
    ) |>
    dplyr::mutate(
      # If a matching Markov end-state probability exists, use it; otherwise keep original prob
      prob = dplyr::coalesce(markov_prob, prob)
    ) |>
    dplyr::select(-markov_prob)
  
  updated_data
}

##########
# runners

#' Loop through each sequential submodel
#'
#' @param model CombinedModel object (list of submodels)
#' @param ... Additional arguments
#' @export
run_model.CombinedModel <- function(model, ...) {
  
  model_chain <- model
  # unnest list of combined models
  # Continue unlisting as long as there are non-Model list elements
  # only works for all same nesting levels
  not_model_list <- function(x) {
    any(sapply(x, function(y) is.list(y) && !inherits(y, "Model")))
  }
  
  while (not_model_list(model_chain)) {
    model_chain <- unlist(model_chain, recursive = FALSE)
  }
  
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

#' Run a submodel
#' @param model Model object
#' @param ... Additional arguments
#' @export
run_model <- function(model, ...) {
  UseMethod("run_model")
}


#' Run a DecisionTree model
#' @param model DecisionTree model
#' @param ... Additional arguments
#' @import dplyr
#' @export
run_model.DecisionTree <- function(model, ...) {
  
  path_results <- calculate_pathways(model)
  
  res <- 
    path_results %>%
    group_by(treatment) %>%
    summarise(
      expected_cost = sum(terminal_prob * path_cost),
      expected_eff = sum(terminal_prob * path_eff)
    )
  
  structure(c(
    res,
    path_results = list(path_results)),
    class = c("output", class(model)))
}


#' Model analysis
#' 
#' Take the output of a model run and
#' return cost effectiveness values
#' 
#' @param results Output of a model run
#' @param ... Additional arguments
#' @export
analysis <- function(results, ...) {
  c(get_costs(results),
    get_effects(results))
}



