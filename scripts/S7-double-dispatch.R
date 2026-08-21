# Load necessary libraries
library(S7)
library(tibble)
library(dplyr)

#================================================================
# 1. Class Definitions
#================================================================

# Define a base 'Model' class and a base 'ModelOutput' class
Model <- new_class("Model")
ModelOutput <- new_class("ModelOutput")

# Define the 'DecisionTree' class and its specific output class
DecisionTree <- new_class(
  "DecisionTree",
  parent = Model,
  properties = list(data = class_data.frame)
)
DecisionTreeOutput <- new_class("DecisionTreeOutput", parent = ModelOutput, properties = list(
  results = class_data.frame,
  terminal_prob = class_list
))

# Define the 'MarkovModel' class and its specific output class
MarkovModel <- new_class(
  "MarkovModel",
  parent = Model,
  properties = list(
    init_probs = class_list,
    trans_matrix = class_numeric,
    cost_matrix = class_numeric,
    q_matrix = class_numeric,
    mapping = class_numeric
  )
)
MarkovModelOutput <- new_class("MarkovModelOutput", parent = ModelOutput, properties = list(results = class_data.frame))

# Define the 'CombinedModel' to hold the sequence
CombinedModel <- new_class(
  "CombinedModel",
  parent = Model,
  properties = list(models = class_list),
  constructor = function(...) {
    models <- list(...)
    if (length(models) < 2) {
      stop("At least two models must be provided.")
    }
    new_object(S7_object(), models = models)
  }
)

#================================================================
# 2. Generic Function Definitions
#================================================================

# Generic for running any model
run_model <- new_generic("run_model", "model")

# Generic for updating a model based on a previous result (the double dispatch)
# Dispatches on both the target model and the previous model's output
update_model <- new_generic("update_model", c("target", "previous_output"))

#================================================================
# 3. Method Implementations
#================================================================

# --- Runner Methods ---

# Run a DecisionTree model
method(run_model, DecisionTree) <- function(model) {
  res <-
    model@data |>
    group_by(decision) |>
    summarise(
      expected_cost = sum(probability * cost),
      expected_effectiveness = sum(probability * effectiveness)
    )
  
  # Create a named list of terminal probabilities for each decision
  terminal_prob <- split(x = model@data$probability, f = model@data$decision)

  # Return a typed output object for dispatch
  DecisionTreeOutput(results = res, terminal_prob = terminal_prob)
}

# Run a MarkovModel
method(run_model, MarkovModel) <- function(model) {
  # This is a dummy simulation for demonstration purposes
  res <- data.frame(
    decision = c("Treatment A", "Treatment B"),
    expected_cost = c(100, 100),
    expected_effectiveness = c(1, 1)
  )
  # Return a typed output object
  MarkovModelOutput(results = res)
}

# Run a chain of models
method(run_model, CombinedModel) <- function(model) {
  results <- list()
  model_chain <- model@models

  for (i in seq_along(model_chain)) {
    current_model <- model_chain[[i]]

    if (i > 1) {
      # Use the double-dispatch 'update_model' generic
      current_model <- update_model(current_model, results[[i - 1]])
    }
    results[[i]] <- run_model(current_model)
  }
  results
}

# --- Double Dispatch Update Methods ---

# Default method: if no specific update rule is found, return the model unchanged
method(update_model, list(Model, ModelOutput)) <- function(target, previous_output) {
  warning("No specific update method found. Returning model unchanged.")
  target
}

# Specific method: How to update a MarkovModel from a DecisionTree's output
method(update_model, list(MarkovModel, DecisionTreeOutput)) <- function(target, previous_output) {
  message("Dispatching: update_model(MarkovModel, DecisionTreeOutput)")
  probs <- previous_output@terminal_prob
  # This mapping logic comes from your 'map_decision_to_markov' function
  target@init_probs <- lapply(probs, \(x) tapply(x, target@mapping, sum))
  target
}

# Specific method: How to update a DecisionTree from a MarkovModel's output
method(update_model, list(DecisionTree, MarkovModelOutput)) <- function(target, previous_output) {
  message("Dispatching: update_model(DecisionTree, MarkovModelOutput)")
  # This mapping logic comes from your 'map_markov_to_decision' function
  target@data <- tibble(
    decision = c("Treatment A", "Treatment B"),
    outcome = c("Success", "Failure"),
    probability = c(0.6, 0.4), # Dummy probabilities
    cost = c(900, 2100),       # Dummy costs
    effectiveness = c(0.88, 0.45) # Dummy effectiveness
  )
  target
}

#================================================================
# 4. Example Usage
#================================================================

# Define the initial decision tree data
decision_tree_data <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 2),
  outcome = c("Success", "Failure", "Success", "Failure"),
  probability = c(0.7, 0.3, 0.6, 0.4),
  cost = c(1000, 2000, 800, 2500),
  effectiveness = c(0.9, 0.5, 0.85, 0.4)
)

# Create the initial DecisionTree model
dt1 <- DecisionTree(data = decision_tree_data)

# Create a MarkovModel, leaving init_probs empty as it will be updated
mm <- MarkovModel(
  mapping = c(1, 2), # Mapping from DT nodes to MM states
  trans_matrix = array(c(0.9, 0.1, 0.2, 0.8, 0.9, 0.1, 0.2, 0.8), dim = c(2, 2, 2)),
  cost_matrix = array(c(1000, 2000), dim = c(1, 2, 2)),
  q_matrix = array(c(1, 0), dim = c(1, 2, 2))
)

# Create a second DecisionTree model that will be updated by the Markov model
dt2 <- DecisionTree(data = decision_tree_data)

# Chain the models together: DT -> MM -> DT
full_model <- CombinedModel(dt1, mm, dt2)

# Run the combined model
final_results <- run_model(full_model)

# Inspect the final results
print(final_results)

