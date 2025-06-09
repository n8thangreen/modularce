
#' Wrapper for heemod package
#' @import heemod
#' @import dplyr
#' @import tidyr
#' @export
#' 
run_model.MarkovModel <- function(model) {

  # Get names from the dimensions of the input arrays
  tx_names <- dimnames(model$trans_matrix)[[3]]
  state_names <- dimnames(model$trans_matrix)[[1]]

  # Use cost and utility names that heemod understands by default
  # You can customize these, but 'cost' and 'qaly' are standard
  cost_names <- "cost"
  util_names <- "qaly"

  # 1. Loop through each treatment to define and run a heemod strategy
  all_results <- lapply(tx_names, function(tx) {

    # Slice the arrays to get matrices for the current treatment
    p_matrix     <- model$trans_matrix[, , tx]
    cost_vector  <- model$cost_matrix[, , tx]
    qaly_vector  <- model$q_matrix[, , tx]
    init_vector  <- model$init_probs[, , tx]

    # 2. Define the states for heemod programmatically
    list_of_states <- setNames(
      lapply(seq_along(state_names), function(i) {
        define_state(
          cost = cost_vector[i],
          utility = qaly_vector[i]
        )
      }),
      state_names
    )
    
    # 3. Define the transition matrix for heemod
    p_list <- as.list(t(p_matrix))
    p_list$state_names <- state_names
    trans_mat <- do.call(heemod::define_transition, p_list)

    # 4. Combine states and transitions into a single strategy object
    strategy <- do.call(define_strategy,
                        c(transition = list(trans_mat),
                          list_of_states))

    # 5. Run the heemod model for this strategy
    heemod_run <- heemod::run_model(
      strategy,
      cycles = model$n_cycles,
      init = init_vector,
      method = "life-table")

    return(heemod_run)
  })

  # Name the list of results by treatment for clarity
  names(all_results) <- tx_names

  # 6. Harmonize the output to match your framework's expectations

  # Get final state probabilities for the 'terminal' output
  terminal_probs_list <- lapply(all_results, function(res) {
    # Get the state populations at the final cycle
    final_counts <- get_counts(res)[res$.cycles + 1, ]
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

  # Get summarized cost/effectiveness results
  summary_list <- lapply(all_results, summary)

  combined_summary <- dplyr::bind_rows(lapply(summary_list, function(s) {
    # heemod summary is per-person, so multiply by N if available
    cost <- s$cost * ifelse(is.na(model$N), 1, model$N)
    eff <- s$qaly * ifelse(is.na(model$N), 1, model$N)
    data.frame(cost = cost, eff = eff)
  }), .id = "treatment")

  # Get the full population trace (optional, but good to have)
  population_trace <- dplyr::bind_rows(lapply(all_results, get_counts), .id = "treatment")

  # Structure the final result list
  res <- list(
    terminal = terminal_df,
    expected_cost = combined_summary$cost,
    expected_eff = combined_summary$eff,
    pop = population_trace
  )

  structure(res,
            class = c("output", "heemod_output", class(model)))
}