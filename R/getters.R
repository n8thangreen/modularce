# getters

default_method_error <- function() {
  stop("No method for this model")
}

#' Extract costs from model results
#' @param results Model output results
#' @param ... Additional arguments passed to methods
#' @export
get_costs <- function(results, ...) {
  UseMethod("get_costs")
}

#' Default get_costs method
#' @param results Model output results
#' @param ... Additional arguments
#' @export
get_costs.default <- function(results, ...) {
  default_method_error()
}

#' Extract costs from DecisionTree results
#' @param results DecisionTree model output
#' @param ... Additional arguments
#' @export
get_costs.DecisionTree <- function(results, ...) {
  results$expected_cost
}

#' Extract costs from MarkovModel results
#' @param results MarkovModel output
#' @param ... Additional arguments
#' @export
get_costs.MarkovModel <- function(results, ...) {
  results$expected_cost
}

#' Extract costs from CombinedModel results
#' @param results CombinedModel output
#' @param ... Additional arguments
#' @export
get_costs.CombinedModel <- function(results, ...) {
  total_cost <- 0
  for (i in seq_along(results)) {
    total_cost <- total_cost + get_costs(results[[i]])
  }
  total_cost
}

#' Extract health effects (QALYs) from model results
#' @param results Model output results
#' @param ... Additional arguments passed to methods
#' @export
get_effects <- function(results, ...) {
  UseMethod("get_effects")
}

#' Default get_effects method
#' @param results Model output results
#' @param ... Additional arguments
#' @export
get_effects.default <- function(results, ...) {
  default_method_error()
}

#' Extract health effects from DecisionTree results
#' @param results DecisionTree model output
#' @param ... Additional arguments
#' @export
get_effects.DecisionTree <- function(results, ...) {
  results$expected_eff
}

#' Extract health effects from MarkovModel results
#' @param results MarkovModel output
#' @param ... Additional arguments
#' @export
get_effects.MarkovModel <- function(results, ...) {
  results$expected_eff
}

#' Extract health effects from CombinedModel results
#' @param results CombinedModel output
#' @param ... Additional arguments
#' @export
get_effects.CombinedModel <- function(results, ...) {
  total_eff <- 0
  for (i in seq_along(results)) {
    total_eff <- total_eff + get_effects(results[[i]])
  }
  total_eff
}
