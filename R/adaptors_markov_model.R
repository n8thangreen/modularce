
#' Adaptor wrapper for heemod package
#' 
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
    
    # 1. Slice the arrays for the current treatment
    p_matrix    <- model$trans_matrix[, , tx]
    
    cost_vector <- model$cost_matrix[1, , tx] 
    qaly_vector <- model$q_matrix[1, , tx]    
    init_vector <- model$init_probs[1, , tx]  
    
    # 2. Build a complete parameter set for THIS iteration
    # This is the most robust way to work with heemod programmatically
    params_list <- list()
    for (i in seq_along(state_names)) {
      params_list[[paste0("cost_", state_names[i])]] <- cost_vector[i]
      params_list[[paste0("qaly_", state_names[i])]] <- qaly_vector[i]
    }
    for (r in seq_along(state_names)) {
      for (c in seq_along(state_names)) {
        params_list[[paste0("p_", r, "_", c)]] <- p_matrix[r, c]
      }
    }
    current_params <- do.call(heemod::define_parameters, params_list)
    
    # 3. Define states using formulas that refer to the parameters
    # The formula `~cost_Healthy` tells heemod to look for `cost_Healthy` in the parameters object
    state_objects <- lapply(state_names, function(sn) {
      heemod::define_state(
        cost = as.formula(paste0("~cost_", sn)),
        utility = as.formula(paste0("~qaly_", sn))
      )
    })
    names(state_objects) <- state_names
    
    # 4. Define the transition matrix using formulas referring to the parameters
    p_formula_names <- paste0("~p_", as.vector(t(outer(1:6, 1:6, paste, sep = "_"))))
    trans_mat <- heemod::define_transition(
      state_names = state_names,
      !!!lapply(p_formula_names, as.formula)
    )
    
    # 5. Define the strategy by explicitly naming the state objects
    # We construct the call and evaluate it to avoid any ambiguity
    strategy_call_args <- c(list(transition = trans_mat), state_objects)
    current_strategy <- do.call(heemod::define_strategy, strategy_call_args)
    
    # 6. Run the model
    heemod_run <- heemod::run_model(
      strategies = setNames(list(current_strategy), tx),
      parameters = current_params,
      cycles = model$n_cycles,
      init = unname(init_vector),
      method = "life-table"
    )
    
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

#' #' dummy function
#' #' @export
#' run_model.MarkovModel <- function(model) {
#'   
#'   init_probs <- model$init_probs
#'   
#'   res <- data.frame(
#'     decision = c("Treatment A", "Treatment B"),
#'     expected_cost = c(100, 100),
#'     expected_eff = c(1,1))
#'   
#'   if (!is.na(model$N)) {
#'     res$expected_cost <- res$expected_cost * model$N
#'     res$expected_eff <- res$expected_eff * model$N
#'   }
#'   
#'   structure(res,
#'             class = c("output", class(model)))
#' }

#' #' Wrapper for adapter function
#' #' @export
#' run_model.MarkovModel <- function(model) {
#'   
#'   out <- markov_model(start_pop = model$init_probs,
#'                       p_matrix = model$trans_matrix,
#'                       state_c_matrix = model$cost_matrix,
#'                       state_q_matrix = model$q_matrix,
#'                       n_cycles = model$n_cycles)
#'   
#'   node_names <- names(model$init_probs[, , 1])
#'   tx_names <- dimnames(model$trans_matrix)[[3]]
#'   
#'   # harmonise 
#'   res <- list()
#'   res$terminal <- out$final_state
#'   res$expected_cost <- out$total_costs
#'   res$expected_eff <- out$total_QALYs
#'   res$pop <- out$pop
#'   
#'   ##TODO: hacky
#'   # convert to long dataframe
#'   dimnames(res$terminal) <- list(node_names, tx_names)
#'   res$terminal <- reshape2::melt(res$terminal)
#'   colnames(res$terminal) <- c("state", "treatment", "probs")
#'   
#'   structure(res,
#'             class = c("output", class(model)))
#' }
