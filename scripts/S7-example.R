# Load the S7 package
library(S7)

#================================================================
# Class Definitions
#================================================================

# Define the base 'Model' class
Model <- new_class("Model")

# Define the 'DecisionTree' class
DecisionTree <- new_class(
  "DecisionTree",
  parent = Model,
  properties = list(
    data = class_data.frame,
    N = class_numeric
  ),
  constructor = function(data, N = NA_real_) {
    new_object(
      S7_object(),
      data = data,
      N = N
    )
  }
)

# Define the 'MarkovModel' class
MarkovModel <- new_class(
  "MarkovModel",
  parent = Model,
  properties = list(
    init_probs = class_list,
    trans_matrix = class_numeric,
    cost_matrix = class_numeric,
    q_matrix = class_numeric,
    mapping = class_numeric,
    N = class_numeric,
    n_cycles = class_integer
  ),
  constructor = function(init_probs = list(),
                         trans_matrix = array(),
                         cost_matrix = array(),
                         q_matrix = array(),
                         mapping = numeric(),
                         N = NA_real_,
                         n_cycles = 10L) {
    new_object(
      S7_object(),
      init_probs = init_probs,
      trans_matrix = trans_matrix,
      cost_matrix = cost_matrix,
      q_matrix = q_matrix,
      mapping = mapping,
      N = N,
      n_cycles = n_cycles
    )
  }
)

# Define the 'CombinedModel' class
CombinedModel <- new_class(
  "CombinedModel",
  parent = Model,
  properties = list(
    models = class_list
  ),
  constructor = function(...) {
    models <- list(...)
    if (length(models) < 2) {
      stop("At least two models must be provided.")
    }
    if (!all(sapply(models, function(m) inherits(m, "S7_object")))) {
      stop("All arguments must be S7 objects.")
    }
    new_object(S7_object(), models = models)
  }
)

#================================================================
# Method Definitions
#================================================================

# Define generics
run_model <- new_generic("run_model", "model")
update_model <- new_generic("update_model", "model")

get_costs <- new_generic("get_costs", "results")
get_effects <- new_generic("get_effects", "results")

# Methods for DecisionTree
method(run_model, DecisionTree) <- function(model) {
  res <-
    model@data |>
    dplyr::group_by(decision) |>
    dplyr::summarise(
      expected_cost = sum(probability * cost),
      expected_eff = sum(probability * effectiveness)
    )

  terminal_prob <- split(x = model@data$probability,
                         f = model@data$decision)

  if (!is.na(model@N)) {
    res$expected_cost <- res$expected_cost * model@N
    res$expected_eff <- res$expected_eff * model@N
  }

  # Return a list that can be used by other methods
  list(results = res, terminal_prob = terminal_prob)
}

method(update_model, DecisionTree) <- function(model, result) {
    # This is based on the map_markov_to_decision function
    # from your R/functions.R file
    model@data <- tibble::tibble(
        decision = c("Treatment A", "Treatment B"),
        outcome = c("Success", "Failure"),
        probability = c(0.6, 0.4),
        cost = c(900, 2100),
        effectiveness = c(0.88, 0.45)
    )
    model
}


method(get_costs, DecisionTree) <- function(results) {
  results$expected_cost
}

method(get_effects, DecisionTree) <- function(results) {
  results$expected_eff
}

# Methods for MarkovModel
method(run_model, MarkovModel) <- function(model) {
  res <- data.frame(
    decision = c("Treatment A", "Treatment B"),
    expected_cost = c(100, 100),
    expected_eff = c(1, 1)
  )

  if (!is.na(model@N)) {
    res$expected_cost <- res$expected_cost * model@N
    res$expected_eff <- res$expected_eff * model@N
  }
  # Return a simple data frame for the Markov model output
  res
}

method(update_model, MarkovModel) <- function(model, result) {
  # This is based on the map_decision_to_markov function
  # from your R/functions.R file
  probs <- result$terminal_prob
  model@init_probs <- lapply(probs, \(x) tapply(x, model@mapping, sum))
  model
}

method(get_costs, MarkovModel) <- function(results) {
  results$expected_cost
}

method(get_effects, MarkovModel) <- function(results) {
  results$expected_eff
}

# Methods for CombinedModel
method(run_model, CombinedModel) <- function(model) {
  result <- list()
  models <- model@models

  for (i in seq_along(models)) {
    current_model <- models[[i]]

    if (i > 1) {
      current_model <- update_model(current_model, result[[i - 1]])
    }
    result[[i]] <- run_model(current_model)
  }
  result
}

method(get_costs, CombinedModel) <- function(results) {
    total_cost <- 0
    for (i in seq_along(results)) {
        # Assuming the first element of each result is the data frame
        total_cost <- total_cost + get_costs(results[[i]][[1]])
    }
    total_cost
}

method(get_effects, CombinedModel) <- function(results) {
    total_eff <- 0
    for (i in seq_along(results)) {
        # Assuming the first element of each result is the data frame
        total_eff <- total_eff + get_effects(results[[i]][[1]])
    }
    total_eff
}

# Load libraries
library(tibble)
library(dplyr)
library(S7)

#================================================================
# Example Data
#================================================================

decision_tree_data <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 2),
  outcome = c("Success", "Failure", "Success", "Failure"),
  probability = c(0.7, 0.3, 0.6, 0.4),
  cost = c(1000, 2000, 800, 2500),
  effectiveness = c(0.9, 0.5, 0.85, 0.4)
)

# Define dummy Markov model parameters
trans_prob_mat <- array(
  c(0.9, 0.1, 0.2, 0.8, 0.9, 0.1, 0.2, 0.8),
  dim = c(2, 2, 2),
  dimnames = list(NULL, NULL, c("A", "B"))
)

cost_mat <- array(
  c(1000, 2000),
  dim = c(1, 2, 2),
  dimnames = list(NULL, NULL, c("A", "B"))
)

q_mat <- array(
  c(1, 0),
  dim = c(1, 2, 2),
  dimnames = list(NULL, NULL, c("A", "B"))
)

# Mapping from decision tree terminal nodes to Markov states
mapping <- c(1, 2)

#================================================================
# Build and Run Models
#================================================================

# Build individual models
dt <- DecisionTree(data = decision_tree_data)
mm <- MarkovModel(
  trans_matrix = trans_prob_mat,
  cost_matrix = cost_mat,
  q_matrix = q_mat,
  mapping = mapping
)

# Run individual models
run_model(dt)
run_model(mm)

# Combine models and run the chain
full_model <- CombinedModel(dt, mm)
final_result <- run_model(full_model)

# Print the final result
print(final_result)
             
