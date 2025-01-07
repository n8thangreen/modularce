
library(tibble)
library(dplyr)

decision_tree <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 2),
  outcome = c("Success", "Failure",
              "Success", "Failure"),
  probability = c(0.7, 0.2, 0.6, 0.3),
  cost = c(1000, 2000, 800, 2500),
  effectiveness = c(0.9, 0.5, 0.85, 0.4)
)

# define S3 class
# constructors

DecisionTree <- function(data) {
  call_obj <- match.call()
  structure(list(data = data),
            call = call_obj,
            class = c("DecisionTree", "Model"))
}

MarkovModel <- function(model, ...) {
  UseMethod("MarkovModel")
}

# first argument?
MarkovModel.default <- function(model = NA,
                                init_probs = NA,
                                trans_matrix = NA,
                                cost_matrix = NA,
                                q_matrix = NA,
                                n_cycles = 10, ...) {
  call_obj <- match.call()
  extra_args <- list(...)
  
  structure(list(init_probs = init_probs,
                 trans_matrix = trans_matrix,
                 cost_matrix = cost_matrix,
                 q_matrix = q_matrix,
                 n_cycles = n_cycles),
            call = call_obj,
            class = c("MarkovModel", "Model"))
}

# decorator on constructor
MarkovModel.DecisionTree <- function(model, mapping, ...) {
  
  init_probs <- map_terminal_to_markov(model$data$probability, mapping)
  
  NextMethod(generic = MarkovModel, object = model, init_probs = init_probs, ...)
}

# only for two models at the moment
#
CombinedModel <- function(mod1, mod2, ...) {
  
  if (!inherits(mod1, "Model") || !inherits(mod2, "Model")) {
    stop("All arguments must be of class 'Model'")
  }
  
  # modify the call object to dispatch on mod1
  mod2_call <- attr(mod2, "call")
  
  # replace with the generic name
  class_names <- as.character(mod2_call[[1]]) |> strsplit("\\.") |> unlist()

  mod21 <- do.call(what = class_names[1],
                   args = c(model = list(mod1), mod2, list(...)))
  
  structure(list(mod1, mod21), class = "CombinedModel")
}

# infix version
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
  CombineModel(mod1, mod2)
}

##########
# runners

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

run_model.CombinedModel <- function(model) {
  
  res <- list()
  
  for (i in seq_along(model)) {
    res[[i]] <- run_model(model[[i]])
  }
  
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

# Group terminal node probabilities
# to Markov model starting states
#
map_terminal_to_markov <- function(probs, mapping) {

  indices <- sort(unique(mapping))
  
  # sum probabilities by group
  sapply(indices, \(x) sum(probs[x]))
}

##############################
# example

dt <- DecisionTree(decision_tree)

trans_prob_mat <- 
  array(c(0.9, 0.1, 0.2, 0.8,
          0.9, 0.1, 0.2, 0.8),
        dim = c(2,2,2),
        dimnames = list(NULL, NULL, c("A", "B")))

cost_mat <- 
  array(c(1000, 2000, 800, 2500,
          1000, 2000, 800, 2500),
        dim = c(2,2,2),
        dimnames = list(NULL, NULL, c("A", "B")))

q_mat <- 
  array(c(1, 0, 1, 0,
          1, 0, 1, 0),
        dim = c(2,2,2),
        dimnames = list(NULL, NULL, c("A", "B")))

# mapping from decision tree terminal nodes to Markov states
mapping <- c(1, 2, 2, 1)

# can either chain the models so include dt as first argument
mm0 <- dt |>
       MarkovModel(trans_matrix = trans_prob_mat,
                   cost_matrix = cost_mat, q_matrix = q_mat,
                   mapping = mapping)

# or create independently and link within CombinedModel()
mm <- MarkovModel(trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat, q_matrix = q_mat)

full_model <- CombinedModel(dt, mm, mapping = mapping)

sim_res <- run_model(full_model)

# could use BCEA package for this
# cea_res <- analysis(sim_res)

## or link after creating the models using infix

##TODO: need additional mapping argument
# full_model <- dt %->% mm
