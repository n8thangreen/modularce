# getters

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
