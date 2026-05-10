# main modelling functions

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
#' @export
update_model <- function(model, result) {
  UseMethod("update_model")
}

#' Markov model
#' @export
update_model.MarkovModel <- function(model, result) {
  model$init_probs <- map_decision_to_markov(result, model$mapping)
  model
}

#' Decision tree
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

#' States from decision tree to Markov model
#' @return dataframe of probabilities by treatment
#' @export
map_decision_to_markov <- function(decision_result, mapping) {
  df <- 
    decision_result$path_results |>
    mutate(state = mapping[terminal_node]) |> 
    group_by(treatment, state) |> 
    summarise(p = sum(terminal_prob))
  
  # rearrange to 3D format
  wide_df <- 
    tidyr::pivot_wider(
      df, names_from = state,
      values_from = p) |> 
    ungroup() 
  
  res <- 
    wide_df |> 
    select(-treatment) |> 
    split(wide_df$treatment) |> 
    simplify2array()
}

#' States from Markov model to decision tree
#' @export
map_markov_to_decision <- function(dt_model, markov_result) {

  left_join(dt_model$data, markov_result$terminal,
            by = c("to" = "state", "treatment")) |> 
    mutate(prob = coalesce(probs, prob)) |> 
    select(-probs)
}

##########
# runners

#' Loop through each sequential submodel
#'
#' @param model_chain list (possibly nested) of submodels
#' @export
run_model.CombinedModel <- function(model_chain) {
  
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
#' @export
run_model <- function(model, ...) {
  UseMethod("run_model")
}


#' @import dplyr
#' @export
run_model.DecisionTree <- function(model) {
  
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
#' @export
analysis <- function(results, ...) {
  c(get_costs(results),
    get_effects(results))
}


