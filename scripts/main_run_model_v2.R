# script
# update models inside of run_model()
#

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


update_model <- function(model, result) {
  UseMethod("update_model")
}

update_model.MarkovModel <- function(model, result) {
  model$init_probs <- map_decision_to_markov(result, model$mapping)
  model
}

update_model.DecisionTree <- function(model, result) {
  model$data <- map_markov_to_decision(result)
  model
}

update_model.default <- function(model, result) {
  warning("No update method defined for this model type. Returning model unmodified.")
  model
}

## mappings

map_decision_to_markov <- function(decision_result, mapping) {
  probs <- decision_result$expected_cost
  indices <- sort(unique(mapping))
  sapply(indices, function(x) sum(probs[which(mapping == x)]))
}

map_markov_to_decision <- function(markov_result) {
  tibble(
    decision = c("Treatment A", "Treatment B"),
    outcome = c("Success", "Failure"),
    probability = c(0.6, 0.4),
    cost = c(900, 2100),      
    effectiveness = c(0.88, 0.45)
  )
}

##########
# runners

run_model.CombinedModel <- function(model_chain) {
  result <- NULL
  
  for (i in seq_along(model_chain)) {
    current_model <- model_chain[[i]]
    
    if (i > 1) {
      # Update model via S3 dispatch
      current_model <- update_model(current_model, result)
    }
    
    result <- run_model(current_model)
  }
  
  result
}

run_model <- function(model, ...) {
  UseMethod("run_model")
}

run_model.DecisionTree <- function(model) {
  res <- 
    model$data %>%
    group_by(decision) %>%
    summarise(
      expected_cost = sum(probability * cost),
      expected_effectiveness = sum(probability * effectiveness)
    )
  
  structure(res,
            class = c("output", class(model)))
}

run_model.MarkovModel <- function(model) {
  
  init_probs <- model$init_probs
  
  res <- data.frame(
    decision = c("Treatment A", "Treatment B"),
    expected_cost = c(100, 100),
    expected_effectiveness = c(1,1))
  
  structure(res,
            class = c("output", class(model)))
}

analysis <- function(results, ...) {
  c(get_costs(results),
    get_effects(results))
}

##########
# helpers

get_costs <- function(results, ...) {
  UseMethod("get_costs")
}

#
get_costs.default <- function(results) {
  stop("No method for this model")
}

#
get_costs.DecisionTree <- function(results) {
  results$expected_cost
}

#
get_costs.MarkovModel <- function(results) {
  results$expected_cost
}

#
get_costs.CombinedModel <- function(results) {
  
  total_cost <- 0
  for (i in seq_along(results)) {
    total_cost <- total_cost + get_costs(results[[i]])
  }
  total_cost
}

###############
# EXAMPLE USAGE
###############

dt <- DecisionTree(decision_tree)

mm <- MarkovModel(trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat, q_matrix = q_mat,
                  mapping = mapping)

dt2 <- DecisionTree(decision_tree)

full_model <- CombinedModel(dt, mm, dt2)

final_result <- run_model(full_model)

