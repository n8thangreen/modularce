#' Native S3 method to run MarkovModel
#' @param model MarkovModel object
#' @param ... Additional arguments
#' @importFrom reshape2 melt
#' @export
run_model.MarkovModel <- function(model, ...) {
  out <- markov_model(start_pop = model$init_probs,
                      p_matrix = model$trans_matrix,
                      state_c_matrix = model$cost_matrix,
                      state_q_matrix = model$q_matrix,
                      n_cycles = model$n_cycles)
  
  node_names <- dimnames(model$trans_matrix)[[1]]
  if (is.null(node_names) && !is.null(model$init_probs)) {
    node_names <- colnames(model$init_probs[, , 1, drop = FALSE])
  }
  tx_names <- dimnames(model$trans_matrix)[[3]]
  
  res <- list()
  res$terminal <- out$final_state
  res$expected_cost <- out$total_costs
  res$expected_eff <- out$total_QALYs
  res$pop <- out$pop
  
  if (!is.null(res$terminal) && is.matrix(res$terminal)) {
    dimnames(res$terminal) <- list(node_names, tx_names)
    res$terminal <- reshape2::melt(res$terminal)
    colnames(res$terminal) <- c("state", "treatment", "probs")
  }
  
  structure(res, class = c("output", class(model)))
}
