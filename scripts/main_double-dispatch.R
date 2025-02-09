# main script

# chatgpt ideas for double dispatch
# hacky

library(tibble)
library(dplyr)

###############################
# 1. Define the DecisionTree and MarkovModel classes
###############################

# DecisionTree constructors and methods
DecisionTree <- function(data, ...) {
  UseMethod("DecisionTree")
}

DecisionTree.default <- function(data, ...) {
  call_obj <- match.call()
  structure(list(data = data),
            call = call_obj,
            class = c("DecisionTree", "Model"))
}

# MarkovModel constructors and methods
MarkovModel <- function(model, ...) {
  UseMethod("MarkovModel")
}

MarkovModel.default <- function(model = NA,
                                init_probs = NA,
                                trans_matrix = NA,
                                cost_matrix = NA,
                                q_matrix = NA,
                                n_cycles = 10, ...) {
  call_obj <- match.call()
  structure(list(init_probs   = init_probs,
                 trans_matrix = trans_matrix,
                 cost_matrix  = cost_matrix,
                 q_matrix     = q_matrix,
                 n_cycles     = n_cycles),
            call = call_obj,
            class = c("MarkovModel", "Model"))
}

###############################
# 2. Define the CombinedModel and infix operator for chaining
###############################

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

# Infix operator for chaining (optional)
`%->%` <- function(mod1, mod2) {
  UseMethod("%->%")
}
`%->%.default` <- function(mod1, mod2) {
  if (!inherits(mod2, "Model")) {
    stop("All arguments must be of class 'Model'")
  } else {
    stop("No method for this model")
  }
}
`%->%.Model` <- function(mod1, mod2) {
  CombinedModel(mod1, mod2)
}

###############################
# 3. Define the runner functions for each model
###############################

run_model <- function(model, ...) {
  UseMethod("run_model")
}

# Run a DecisionTree model
run_model.DecisionTree <- function(model) {
  res <- 
    model$data %>%
    group_by(decision) %>%
    summarise(
      expected_cost = sum(probability * cost),
      expected_effectiveness = sum(probability * effectiveness)
    )
  
  # Tag the output with a specific class so update_model() can inspect it
  structure(res,
            class = c("DecisionTreeOutput", "output", class(model)))
}

# Run a MarkovModel model
run_model.MarkovModel <- function(model) {
  # Use model$init_probs (this is a dummy simulation for demonstration)
  init_probs <- model$init_probs
  
  res <- data.frame(
    decision = c("Treatment A", "Treatment B"),
    expected_cost = c(100, 100),
    expected_effectiveness = c(1, 1)
  )
  
  # Tag with a specific class so update_model() can inspect the result
  structure(res,
            class = c("MarkovModelOutput", "output", class(model)))
}

# Run a CombinedModel: update each model in the chain (using update_model())
# and store each model's output in a list.
run_model.CombinedModel <- function(model_chain) {
  results <- list()
  
  for (i in seq_along(model_chain)) {
    current_model <- model_chain[[i]]
    
    if (i > 1) {
      # Update current_model based on the output of the previous model
      # The update_model() S3 function inspects the previous result’s class
      current_model <- update_model(current_model, results[[i - 1]])
    }
    
    results[[i]] <- run_model(current_model)
  }
  
  structure(results, class = c("CombinedModelOutput", "output"))
}

###############################
# 4. Define the analysis and helper functions
###############################

analysis <- function(results, ...) {
  c(get_costs(results),
    get_effects(results))
}

get_costs <- function(results, ...) {
  UseMethod("get_costs")
}
get_costs.default <- function(results) {
  stop("No method for this model")
}
get_costs.DecisionTree <- function(results) {
  results$expected_cost
}
get_costs.MarkovModel <- function(results) {
  results$expected_cost
}
get_costs.CombinedModel <- function(results) {
  total_cost <- 0
  for (i in seq_along(results)) {
    total_cost <- total_cost + get_costs(results[[i]])
  }
  total_cost
}

get_effects <- function(results, ...) {
  UseMethod("get_effects")
}
get_effects.default <- function(results) {
  stop("No method for this model")
}
get_effects.DecisionTree <- function(results) {
  results$expected_effectiveness
}
get_effects.MarkovModel <- function(results) {
  results$expected_effectiveness
}
get_effects.CombinedModel <- function(results) {
  total_effect <- 0
  for (i in seq_along(results)) {
    total_effect <- total_effect + get_effects(results[[i]])
  }
  total_effect
}

# Group terminal node probabilities to Markov model starting states
map_terminal_to_markov <- function(probs, mapping) {
  indices <- sort(unique(mapping))
  sapply(indices, function(x) sum(probs[x]))
}

# Helper mapping: from DecisionTree output to MarkovModel initial probabilities.
map_decision_to_markov <- function(decision_result, mapping) {
  # Here we assume decision_result has a column "expected_cost" (used as a placeholder).
  probs <- decision_result$expected_cost
  indices <- sort(unique(mapping))
  sapply(indices, function(x) sum(probs[which(mapping == x)]))
}

# Helper mapping: from MarkovModel output to new DecisionTree input.
map_markov_to_decision <- function(markov_result) {
  tibble(
    decision = c("Treatment A", "Treatment B"),
    outcome = c("Success", "Failure"),
    probability = c(0.6, 0.4),  # dummy probabilities
    cost = c(900, 2100),         # dummy costs
    effectiveness = c(0.88, 0.45)  # dummy effectiveness
  )
}

#######################################
# define the update_model S3 functions
# a kind of double dispatch approach

update_model <- function(target, previous) {
  UseMethod("update_model")
}

update_model.DecisionTree <- function(target, previous) {
  UseMethod("update_model.DecisionTree", previous)
}

update_model.MarkovModel <- function(target, previous) {
  UseMethod("update_model.MarkovModel", previous)
}
         
update_model.DecisionTree.MarkovModel <- function(target, previous) {
  target$data <- map_decision_to_markov(target, target$mapping)
  target
}

update_model.MarkovModel.DecisionTree <- function(target, previous) {
  target$data <- map_decision_to_markov(target, target$mapping)
  target
}

update_model.default <- function(target, previous) {
  warning("No update method for this model type; returning model unchanged.")
  target
}

###############################
# Example 
###############################

# Create a decision tree tibble (this is the same as your original input)
# (You may change these values as needed.)
decision_tree <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 2),
  outcome = c("Success", "Failure", "Success", "Failure"),
  probability = c(0.7, 0.2, 0.6, 0.3),
  cost = c(1000, 2000, 800, 2500),
  effectiveness = c(0.9, 0.5, 0.85, 0.4)
)

# Create an initial DecisionTree model
dt <- DecisionTree(decision_tree)

# Define dummy Markov model parameters (using a 3D array for transition probabilities)
trans_prob_mat <- array(c(0.9, 0.1, 0.2, 0.8,
                          0.9, 0.1, 0.2, 0.8),
                        dim = c(2, 2, 2),
                        dimnames = list(NULL, NULL, c("A", "B")))

cost_mat <- array(c(1000, 2000),
                  dim = c(1, 2, 2),
                  dimnames = list(NULL, NULL, c("A", "B")))

q_mat <- array(c(1, 0),
               dim = c(1, 2, 2),
               dimnames = list(NULL, NULL, c("A", "B")))

# Mapping from decision tree terminal nodes to Markov states.
mapping <- c(1, 2, 2, 1)

# Create a MarkovModel from the DecisionTree.
# This uses the decorator MarkovModel.DecisionTree.
mm <- MarkovModel(dt,
                  mapping = mapping,
                  trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat,
                  q_matrix = q_mat)

# Optionally, create a second DecisionTree model that will be updated based on the MarkovModel's output.
dt2 <- DecisionTree(decision_tree)

# Chain the models together into a CombinedModel.
# In this example, we chain: DecisionTree -> MarkovModel -> DecisionTree.
full_model <- CombinedModel(dt, mm, dt2)

# Run the combined model. The output is a list containing the result for each model.
final_results <- run_model(full_model)

# Inspect the results from each model in the chain.
print(final_results)
