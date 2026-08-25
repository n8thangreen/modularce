# ==============================================================================
# Modular Cost-Effectiveness Analysis in R using S3 Classes
#
# Tutorial Example based on:
# Lin, Y. S., O’Mahony, J. F., & van Rosmalen, J. (2023)
# A Simple Cost-Effectiveness Model of Screening: An Open-Source Teaching 
# and Research Tool Coded in R. PharmacoEconomics - Open, 7(4), 507–523.
# https://doi.org/10.1007/s41669-023-00414-1
# ==============================================================================

library(tibble)
library(dplyr)

# ==============================================================================
# 1. Define Model Data & Parameters
# ==============================================================================

# Decision Tree: Screening event (diagnostic testing & workup)
decision_tree <- tribble(
  ~from,                  ~to,                    ~prob, ~cost, ~eff,
  # Initial cohort split across underlying health states prior to screening
  "root",                 "Healthy",              0.80,      0,    0,
  "root",                 "Pre_clinical_Disease", 0.15,      0,    0,
  "root",                 "Disease",              0.05,      0,    0,
  "root",                 "Treated",              0.00,      0,    0,
  "root",                 "Death_from_Disease",   0.00,      0,    0,
  "root",                 "Other_Cause_Death",    0.00,      0,    0,
  
  # Diagnostic testing outcomes starting from "Healthy"
  "Healthy",              "FP",                   0.10,    200,    0,  # False Positive (screening + biopsy)
  "Healthy",              "TN",                   0.90,     50,    0,  # True Negative (screening only)
  
  # Diagnostic testing outcomes starting from "Pre-clinical Disease"
  "Pre_clinical_Disease", "TP_Preclinical",       0.70,    200,    0,  # True Positive
  "Pre_clinical_Disease", "FN_Preclinical",       0.30,     50,    0,  # False Negative
  
  # Diagnostic testing outcomes starting from "Clinical Disease"
  "Disease",              "TP_Disease",           1.00,    200,    0,  # True Positive
  "Disease",              "FN_Disease",           0.00,     50,    0   # False Negative
)

decision_tree

# Markov model state names
states <- c("Healthy", "Pre_clinical_Disease", "Disease", 
            "Treated", "Death_from_Disease", "Other_Cause_Death")

# Time-homogeneous annual transition probability matrix (6 x 6)
trans_mat <- matrix(
  c(
    # Healthy (H)
    0.90, 0.05, 0.00, 0.00, 0.00, 0.05,
    # Pre-clinical Disease (P)
    0.00, 0.65, 0.20, 0.00, 0.10, 0.05,
    # Clinical Disease (D)
    0.00, 0.00, 0.60, 0.20, 0.20, 0.00,
    # Treated (T)
    0.00, 0.00, 0.00, 0.95, 0.00, 0.05,
    # Death from Disease (DD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    # Other-Cause Death (OCD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
  ),
  nrow = 6, byrow = TRUE,
  dimnames = list(from = states, to = states)
)

trans_mat

# Annual state costs and health utilities (QALY weights)
cost_vec <- c(
  Healthy              = 0,
  Pre_clinical_Disease = 0,
  Disease              = 0,
  Treated              = 100,
  Death_from_Disease   = 0,
  Other_Cause_Death    = 0
)

util_vec <- c(
  Healthy              = 1.0,
  Pre_clinical_Disease = 0.8,
  Disease              = 0.5,
  Treated              = 0.9,
  Death_from_Disease   = 0.0,
  Other_Cause_Death    = 0.0
)

# Baseline cohort distribution for natural history (no screening)
init_pop <- c(
  Healthy              = 1.0,
  Pre_clinical_Disease = 0.0,
  Disease              = 0.0,
  Treated              = 0.0,
  Death_from_Disease   = 0.0,
  Other_Cause_Death    = 0.0
)

cost_vec
util_vec
init_pop

# Mapping from Decision Tree terminal nodes to Markov states
mapping <- c(
  FP                 = "Treated",
  TN                 = "Healthy",
  TP_Preclinical     = "Treated",
  FN_Preclinical     = "Pre_clinical_Disease",
  TP_Disease         = "Treated",
  FN_Disease         = "Disease",
  Death_from_Disease = "Death_from_Disease",
  Other_Cause_Death  = "Other_Cause_Death",
  Treated            = "Treated"
)

mapping

# ==============================================================================
# 2. S3 Framework for Modular Cost-Effectiveness Modeling
# ==============================================================================

# --- A. S3 Constructors -------------------------------------------------------

#' DecisionTree Model Constructor
DecisionTree <- function(data) {
  structure(list(data = data), 
            class = c("DecisionTree", "Model"))
}

#' MarkovModel Constructor
MarkovModel <- function(trans_matrix, cost_vector, util_vector, 
                        init_probs = NULL, mapping = NULL, n_cycles = 2, 
                        discount_rate = 0.035) {
  structure(
    list(
      trans_matrix  = trans_matrix,
      cost_vector   = cost_vector,
      util_vector   = util_vector,
      init_probs    = init_probs,
      mapping       = mapping,
      n_cycles      = n_cycles,
      discount_rate = discount_rate
    ),
    class = c("MarkovModel", "Model")
  )
}

#' CombinedModel Constructor (Sequences multiple submodels)
CombinedModel <- function(...) {
  structure(list(...), 
            class = c("CombinedModel", "Model"))
}


# --- B. S3 Generics -----------------------------------------------------------

#' Generic to execute a model
run_model <- function(model, ...) {
  UseMethod("run_model")
}

#' Generic to update a downstream model using previous model outputs
update_model <- function(model, result) {
  UseMethod("update_model")
}

#' Generics to extract cost-effectiveness endpoints
get_costs <- function(x) {
  UseMethod("get_costs")
}

get_effects <- function(x) {
  UseMethod("get_effects")
}


# --- C. Decision Tree Engine & S3 Methods --------------------------------------

#' Calculate probabilities, costs, and effects for all root-to-terminal pathways
calculate_pathways <- function(tree_data) {
  paths <- list(list(node = "root", 
                     prob = 1, 
                     cost = 0, 
                     eff = 0))
  completed <- list()
  
  while (length(paths) > 0) {
    curr <- paths[[1]]
    paths <- paths[-1]
    
    transitions <- tree_data[tree_data$from == curr$node, ]
    
    if (nrow(transitions) == 0) {
      completed[[length(completed) + 1]] <- curr
    } else {
      for (i in seq_len(nrow(transitions))) {
        tr <- transitions[i, ]
        
        paths[[length(paths) + 1]] <- list(
          node = tr$to,
          prob = curr$prob * tr$prob,
          cost = curr$cost + tr$cost,
          eff  = curr$eff + tr$eff
        )
      }
    }
  }
  
  tibble(
    terminal_node = sapply(completed, `[[`, "node"),
    prob          = sapply(completed, `[[`, "prob"),
    cost          = sapply(completed, `[[`, "cost"),
    eff           = sapply(completed, `[[`, "eff")
  )
}

#' Execute DecisionTree model
run_model.DecisionTree <- function(model, ...) {
  pathways <- calculate_pathways(model$data)
  
  structure(
    list(
      expected_cost = sum(pathways$prob * pathways$cost),
      expected_eff  = sum(pathways$prob * pathways$eff),
      path_results  = pathways
    ),
    class = c("output_DecisionTree", "output")
  )
}

#' Update DecisionTree root branch probabilities from previous Markov end states
update_model.DecisionTree <- function(model, prev_result) {
  if (!is.null(prev_result$terminal)) {
    end_states <- prev_result$terminal
    tree <- model$data
    root_idx <- which(tree$from == "root" & tree$to %in% names(end_states))
    tree$prob[root_idx] <- end_states[tree$to[root_idx]]
    model$data <- tree
  }
  model
}


# --- D. Markov Model Engine & S3 Methods ---------------------------------------

#' Time-homogeneous Markov simulation engine
markov_model <- function(start_pop, trans_mat, cost_vec, util_vec, n_cycles, 
                         discount_rate = 0.035) {
  n_states <- length(start_pop)
  state_names <- names(start_pop)
  if (is.null(state_names)) state_names <- colnames(trans_mat)
  
  trace <- matrix(NA, nrow = n_cycles, ncol = n_states, 
                  dimnames = list(paste0("cycle_", 1:n_cycles), state_names))
  trace[1, ] <- start_pop
  
  for (t in 2:n_cycles) {
    trace[t, ] <- trace[t - 1, ] %*% trans_mat
  }
  
  # Annual discount factor
  disc_vec <- 1 / (1 + discount_rate)^(0:(n_cycles - 1))
  
  cycle_costs <- (trace %*% cost_vec)[, 1] * disc_vec
  cycle_qalys <- (trace %*% util_vec)[, 1] * disc_vec
  
  # Total discounted costs and QALYs accrued during the interval (cycles 2:n_cycles)
  total_cost <- sum(cycle_costs[-1])
  total_qaly <- sum(cycle_qalys[-1])
  
  list(
    trace         = trace,
    final_state   = trace[n_cycles, ],
    expected_cost = total_cost,
    expected_eff  = total_qaly
  )
}

#' Execute MarkovModel
run_model.MarkovModel <- function(model, ...) {
  out <- markov_model(
    start_pop     = model$init_probs,
    trans_mat     = model$trans_matrix,
    cost_vec      = model$cost_vector,
    util_vec      = model$util_vector,
    n_cycles      = model$n_cycles,
    discount_rate = model$discount_rate
  )
  
  structure(
    list(
      expected_cost = out$expected_cost,
      expected_eff  = out$expected_eff,
      terminal      = out$final_state,
      trace         = out$trace
    ),
    class = c("output_MarkovModel", "output")
  )
}

#' Update MarkovModel initial state probabilities from DecisionTree terminal nodes
update_model.MarkovModel <- function(model, prev_result) {
  if (!is.null(prev_result$path_results) && !is.null(model$mapping)) {
    all_states <- colnames(model$trans_matrix)
    new_init   <- setNames(rep(0, length(all_states)), all_states)
    
    paths       <- prev_result$path_results
    paths$state <- model$mapping[paths$terminal_node]
    
    # Sum the probabilities for each state
    for (s in unique(paths$state)) {
      new_init[s] <- sum(paths$prob[paths$state == s])
    }
    
    model$init_probs <- new_init
  }
  model
}


# --- E. CombinedModel Orchestrator & Getters -----------------------------------

#' Execute a sequential chain of submodels
run_model.CombinedModel <- function(model, ...) {
  results <- list()
  for (i in seq_along(model)) {
    current_submodel <- model[[i]]
    if (i > 1) {
      current_submodel <- update_model(current_submodel, results[[i - 1]])
    }
    results[[i]] <- run_model(current_submodel)
  }
  structure(results, class = c("output_CombinedModel", "output"))
}

# Cost getters
get_costs.output_DecisionTree  <- function(x) x$expected_cost
get_costs.output_MarkovModel   <- function(x) x$expected_cost
get_costs.output_CombinedModel <- function(x) sum(sapply(x, get_costs))

# Health effect (QALY) getters
get_effects.output_DecisionTree  <- function(x) x$expected_eff
get_effects.output_MarkovModel   <- function(x) x$expected_eff
get_effects.output_CombinedModel <- function(x) sum(sapply(x, get_effects))


# ==============================================================================
# 3. Model Execution & Cost-Effectiveness Analysis
# ==============================================================================

# Build base model components
dt <- DecisionTree(decision_tree)

mm2 <- MarkovModel(
  trans_matrix = trans_mat,
  cost_vector  = cost_vec,
  util_vector  = util_vec,
  mapping      = mapping,
  n_cycles     = 2
)

mm5 <- MarkovModel(
  trans_matrix = trans_mat,
  cost_vector  = cost_vec,
  util_vector  = util_vec,
  mapping      = mapping,
  n_cycles     = 5
)

mm10 <- MarkovModel(
  trans_matrix = trans_mat,
  cost_vector  = cost_vec,
  util_vector  = util_vec,
  mapping      = mapping,
  n_cycles     = 10
)

cat("====================================================\n")
cat("1. Standalone Submodel Execution\n")
cat("====================================================\n")

# Run standalone Decision Tree
dt_res <- run_model(dt)
cat("Decision Tree Cost: $", round(get_costs(dt_res), 2), 
    " | QALYs: ", round(get_effects(dt_res), 4), "\n", sep = "")
print(dt_res$path_results)

cat("\n====================================================\n")
cat("2. Single Screening Round (DT + 2-Year Markov Interval)\n")
cat("====================================================\n")

pair2 <- CombinedModel(dt, mm2)
pair2_res <- run_model(pair2)
cat("Combined (DT + MM2) Total Cost: $", round(get_costs(pair2_res), 2), 
    " | Total QALYs: ", round(get_effects(pair2_res), 4), "\n\n", sep = "")

cat("====================================================\n")
cat("3. Policy Scenario Comparison (20-Year Time Horizon)\n")
cat("====================================================\n")

# Define screening strategies over a 20-year time horizon
strategies <- list(
  "No Screening (Markov 20y)"      = MarkovModel(trans_mat, cost_vec, util_vec, 
                                                 init_probs = init_pop, n_cycles = 20),
  "10-Yearly Screening (2 rounds)" = do.call(CombinedModel, rep(list(dt, mm10), 2)),
  "5-Yearly Screening (4 rounds)"  = do.call(CombinedModel, rep(list(dt, mm5), 4)),
  "2-Yearly Screening (10 rounds)" = do.call(CombinedModel, rep(list(dt, mm2), 10))
)

# Run models and extract cost-effectiveness metrics
results_table <- tibble(
  Strategy      = names(strategies),
  Cost          = sapply(strategies, function(s) get_costs(run_model(s))),
  QALYs         = sapply(strategies, function(s) get_effects(run_model(s)))
) |> 
  mutate(
    Inc_Cost = Cost - Cost[1],
    Inc_QALY = QALYs - QALYs[1],
    ICER     = Inc_Cost / Inc_QALY
  )

print(results_table)
cat("\nAnalysis complete!\n")
