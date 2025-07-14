# S3 class constructor + print/summary methods

#' @export
MarkovModel <- function(strategy_name, params, strategy) {
  structure(
    list(
      name = strategy_name,
      params = params,
      strategy = strategy,
      trace = NULL,
      qalys = NA,
      costs = NA
    ),
    class = "MarkovModel"
  )
}
