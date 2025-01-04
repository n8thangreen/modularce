
# define S3 class
# constructors

DecisionTree <- function(data) {
  structure(list(data = data), class = c("DecisionTree", "Model"))
}

MarkovModel <- function(model, ...) {
  UseMethod("MarkovModel")
}

# first argument?
MarkovModel.default <- function(model = NA, init_probs, trans_matrix = NA, n_cycles = 10) {
  structure(list(init_probs = init_probs,
                 trans_matrix = trans_matrix,
                 n_cycles = n_cycles),
            class = c("MarkovModel", "Model"))
}

# decorator on constructor
MarkovModel.DecisionTree <- function(model, ...) {
  term_probs <- model$term_probs
  init_probs <- map_terminal_to_markov(term_probs, mapping)
  
  nextMethod(generic = MarkovModel, object = model, init_probs, ...)
}

CombinedModel <- function(...) {
  args <- list(...)
  
  if (any(sapply(args, inherits) != "Model")) {
    stop("All arguments must be of class 'Model'")
  }
  
  structure(args, class = "CombinedModel")
}

# infix version
`$->$` <- function(mod1, mod2) {
  
  if (!inherits(mod1, "Model") || !inherits(mod2, "Model")) {
    stop("All arguments must be of class 'Model'")
  }
  
  structure(list(mod1, mod2), class = "CombinedModel")
}

###

run_model <- function(model, ...) {
  UseMethod("run_model")
}

run_model.DecisionTree <- function(model) {
  model$data %>%
    group_by(decision) %>%
    summarise(
      expected_cost = sum(probability * cost),
      expected_effectiveness = sum(probability * effectiveness)
    )
}

#
run_to_markov.DecisionTree <- function(model) {
  function(model, mapping) {
    res <- run_model(model)
    term_probs <- model$term_probs
    res$init_probs <- map_terminal_to_markov(term_probs, mapping)
    res
  }
}

run_model.MarkovModel <- function(model) {
  
}

run_model.CombinedModel <- function(model) {
  
  for (i in 1:length(model)) {
    model_results[[i]] <- run_model(model[[i]])
  }
  
  model_results
}

# helpers

#
get_costs.DecisionTree <- function(model) {
  results$expected_cost
}

#
get_costs.MarkovModel <- function(model) {
  results$cumulative_cost
}

#
get_costs.CombinedModel <- function(model) {
  total_cost <- 0
  for (i in 1:length(model)) {
    total_cost <- total_cost + get_costs(model)
  }
  total_cost
}

# Group terminal node probabilities
# to Markov model starting states
#
map_terminal_to_markov <- function(probs, mapping) {
  
}

