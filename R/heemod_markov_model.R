#' Wrapper adaptor for heemod package (optional)
#' 
#' @param model A MarkovModel object
#' @export
run_model_heemod <- function(model) {
  if (!requireNamespace("heemod", quietly = TRUE)) {
    stop("Package 'heemod' is required for this adaptor. Please install it with install.packages('heemod').")
  }

  tx_names <- dimnames(model$trans_matrix)[[3]]
  state_names <- dimnames(model$trans_matrix)[[1]]

  all_results <- lapply(tx_names, function(tx) {
    p_matrix     <- model$trans_matrix[, , tx]
    cost_vector  <- model$cost_matrix[, , tx]
    qaly_vector  <- model$q_matrix[, , tx]
    init_vector  <- model$init_probs[, , tx]

    list_of_states <- setNames(
      lapply(seq_along(state_names), function(i) {
        heemod::define_state(
          cost = cost_vector[i],
          utility = qaly_vector[i]
        )
      }),
      state_names
    )
    
    p_list <- as.list(t(p_matrix))
    p_list$state_names <- state_names
    trans_mat <- do.call(heemod::define_transition, p_list)

    strategy <- do.call(heemod::define_strategy,
                        c(transition = list(trans_mat),
                          list_of_states))

    heemod_run <- heemod::run_model(
      strategy,
      cycles = model$n_cycles,
      init = init_vector,
      method = "life-table")

    return(heemod_run)
  })

  names(all_results) <- tx_names

  terminal_probs_list <- lapply(all_results, function(res) {
    final_counts <- heemod::get_counts(res)[res$.cycles + 1, ]
    final_probs <- final_counts / sum(final_counts)
    return(as.data.frame(t(final_probs[state_names])))
  })

  terminal_df <-
    dplyr::bind_rows(terminal_probs_list, .id = "treatment") %>%
    tidyr::pivot_longer(
      cols = -treatment,
      names_to = "state",
      values_to = "probs"
    )

  summary_list <- lapply(all_results, summary)

  combined_summary <- dplyr::bind_rows(lapply(summary_list, function(s) {
    cost <- s$cost * ifelse(is.na(model$N), 1, model$N)
    eff <- s$qaly * ifelse(is.na(model$N), 1, model$N)
    data.frame(cost = cost, eff = eff)
  }), .id = "treatment")

  population_trace <- dplyr::bind_rows(lapply(all_results, heemod::get_counts), .id = "treatment")

  res <- list(
    terminal = terminal_df,
    expected_cost = combined_summary$cost,
    expected_eff = combined_summary$eff,
    pop = population_trace
  )

  structure(res, class = c("output", "heemod_output", class(model)))
}