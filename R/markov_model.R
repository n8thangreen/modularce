# based on PharmacoEconomics (2022) paper
# https://github.com/Excel-R-tutorials/Markov-model-introduction/blob/main/Markov_model_realworld.R

#
markov_model <- function(start_pop,
                         p_matrix,
                         state_c_matrix,
                         state_q_matrix,
                         n_cycles = 2,
                         s_names = NULL,
                         t_names = NULL) {
  
  n_states <- ncol(p_matrix)
  n_treat <- dim(p_matrix)[3]
  
  pop <- array(data = NA,
               dim = c(n_states, n_cycles, n_treat),
               dimnames = list(state = s_names,
                               cycle = NULL,
                               treatment = t_names))
  
  for (i in 1:n_treat) {
    pop[, cycle = 1, i] <- unlist(start_pop[1, , i])
  }
  
  cycle_empty_array <-
    array(NA,
          dim = c(n_treat, n_cycles),
          dimnames = list(treatment = t_names,
                          cycle = NULL))
  
  cycle_state_costs <- cycle_empty_array
  cycle_costs <- cycle_QALYs <- cycle_empty_array
  cycle_QALE <- cycle_empty_array   # quality-adjusted life expectancy
  
  total_costs <- setNames(rep(NA, n_treat), t_names)
  total_QALYs <- setNames(rep(NA, n_treat), t_names)
  
  for (i in 1:n_treat) {
    for (j in 2:n_cycles) {
      
      # Matrix multiplication
      pop[, cycle = j, treatment = i] <-
        pop[, cycle = j - 1, treatment = i] %*% p_matrix[, , treatment = i]
    }
    
    cycle_costs[i, ] <-
      (state_c_matrix[, , treatment = i] %*% pop[, , treatment = i]) * 1/(1 + 0.035)^(1:n_cycles - 1)
    
    cycle_QALE[i, ] <-
      state_q_matrix[, , treatment = i] %*%  pop[, , treatment = i]
    
    cycle_QALYs[i, ] <- cycle_QALE[i, ] * 1/(1 + 0.035)^(1:n_cycles - 1)
    
    total_costs[i] <- sum(cycle_costs[treatment = i, -1])
    total_QALYs[i] <- sum(cycle_QALYs[treatment = i, -1])
  }
  
  final_pop <- pop[, n_cycles, , drop = FALSE]
  
  list_of_slices <- lapply(1:dim(final_pop)[3], function(i) final_pop[, , i])
  final_pop_mat <- do.call(cbind, list_of_slices)
  
  list(final_state = final_pop_mat,
       pop = pop[, , , drop = FALSE],
       total_costs = total_costs,
       total_QALYs = total_QALYs)
}
