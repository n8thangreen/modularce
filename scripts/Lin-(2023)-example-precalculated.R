# ==============================================================================
# Alternative Lin (2023) Screening Model Example with Precalculated Scenario Inputs
# 
# Reference:
# Lin, Y. S., O’Mahony, J. F., & van Rosmalen, J. (2023)
# A Simple Cost-Effectiveness Model of Screening: An Open-Source Teaching and Research Tool Coded in R.
# PharmacoEconomics - Open, 7(4), 507–523. https://doi.org/10.1007/s41669-023-00414-1
# ==============================================================================

library(tibble)
library(dplyr)
library(modularce)

# ------------------------------------------------------------------------------
# 1. Define Model Data (Decision Tree & Markov Parameters)
# ------------------------------------------------------------------------------

decision_tree <- tribble(
  ~treatment, ~from,                ~to,              ~prob, ~cost,  ~eff,
  # Initial population split (true underlying state for Scenario A)
  "A",        "root",               "Healthy",              0.80,    0,      0,
  "A",        "root",               "Pre_clinical_Disease", 0.15,    0,      0,
  "A",        "root",               "Disease",              0.05,    0,      0,
  # Direct transitions to death states
  "A",        "root",               "Death_from_Disease",   0.00,    0,      0,
  "A",        "root",               "Other_Cause_Death",    0.00,    0,      0,
  "A",        "root",               "Treated",              0.00,    0,      0,
  
  # Starting "Healthy"
  "A",        "Healthy",            "FP",                   0.10,  200,      0,
  "A",        "Healthy",            "TN",                   0.90,   50,      0,
  
  # Starting "Pre-clinical Disease"
  "A",        "Pre_clinical_Disease", "TP_Preclinical",     0.70,  200,      0,
  "A",        "Pre_clinical_Disease", "FN_Preclinical",     0.30,   50,      0,
  
  # Starting "Disease"
  "A",        "Disease",            "TP_Disease",           1.00,  200,      0,
  "A",        "Disease",            "FN_Disease",           0.00,   50,      0
)

states <- c("Healthy", "Pre_clinical_Disease", "Disease", "Treated", "Death_from_Disease", "Other_Cause_Death")

mapping <- c(
  Treated            = "Treated",
  FP                 = "Treated",
  TN                 = "Healthy",
  TP_Preclinical     = "Treated",
  FN_Preclinical     = "Pre_clinical_Disease",
  TP_Disease         = "Treated",
  FN_Disease         = "Disease",
  Death_from_Disease = "Death_from_Disease",
  Other_Cause_Death  = "Other_Cause_Death"
)
mapping <- factor(mapping, levels = states)

trans_prob_mat <- array(
  c(
    # FROM Healthy
    0.90, 0.05, 0.00, 0.00, 0.00, 0.05,
    # FROM Pre-clinical Disease
    0.00, 0.65, 0.20, 0.00, 0.10, 0.05,
    # FROM Disease
    0.00, 0.00, 0.60, 0.20, 0.20, 0.00,
    # FROM Treated
    0.00, 0.00, 0.00, 0.95, 0.00, 0.05,
    # FROM Death from Disease
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    # FROM Other-Cause Death
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
  ),
  dim = c(6, 6, 1),
  dimnames = list(to = states, from = states, scenario = "A")
) |> aperm(c(2, 1, 3))

cost_mat <- array(
  c(0, 0, 0, 100, 0, 0),
  dim = c(1, 6, 1),
  dimnames = list(NULL, state = states, scenario = "A")
)

q_mat <- array(
  c(1, 0.8, 0.5, 0.9, 0, 0),
  dim = c(1, 6, 1),
  dimnames = list(NULL, state = states, scenario = "A")
)

p_init <- array(
  c(1, 0, 0, 0, 0, 0),
  dim = c(1, 6, 1),
  dimnames = list(NULL, states, "A")
)

# ------------------------------------------------------------------------------
# 2. Precalculate Base Model Objects & Scenario Grid
# ------------------------------------------------------------------------------

# Base Decision Tree
dt <- DecisionTree(decision_tree)

# Precalculate decision tree pathway tree structure upfront
dt_pathways <- modularce:::calculate_pathways(dt)
cat("Precalculated decision tree pathways:\n")
print(dt_pathways)

# Define scenario parameter combinations upfront in a grid
scenario_grid <- expand.grid(
  t_between_screens = c(2, 5, 10, 15, 20),
  num_screens       = c(1, 2, 3, 4, 5, 10),
  stringsAsFactors  = FALSE
)

# Precalculate distinct MarkovModel objects for each cycle duration
t_values <- unique(scenario_grid$t_between_screens)
mm_precalculated <- setNames(
  lapply(t_values, function(t_val) {
    MarkovModel(
      trans_matrix = trans_prob_mat,
      cost_matrix  = cost_mat,
      q_matrix     = q_mat,
      init_probs   = p_init,
      mapping      = mapping,
      n_cycles     = t_val
    )
  }),
  as.character(t_values)
)

# Precalculate all pair models (DT + MM)
pair_models_precalculated <- setNames(
  lapply(t_values, function(t_val) {
    CombinedModel(dt, mm_precalculated[[as.character(t_val)]])
  }),
  as.character(t_values)
)

# Precalculate the complete list of CombinedModel scenario inputs before running execution
scenario_inputs <- lapply(seq_len(nrow(scenario_grid)), function(i) {
  t_val <- scenario_grid$t_between_screens[i]
  n_scr <- scenario_grid$num_screens[i]
  pair_mod <- pair_models_precalculated[[as.character(t_val)]]
  do.call(CombinedModel, args = rep(pair_mod, n_scr))
})

# Name the precalculated scenario inputs
scenario_labels <- sprintf("t_between_%d_screens_%d", 
                           scenario_grid$t_between_screens, 
                           scenario_grid$num_screens)
names(scenario_inputs) <- scenario_labels

cat(sprintf("\nPrecalculated %d scenario model input chains.\n", length(scenario_inputs)))

# ------------------------------------------------------------------------------
# 3. Batch Run Precalculated Scenario Inputs
# ------------------------------------------------------------------------------

# Execute all precalculated scenario models in a clean batch call
scenario_results <- lapply(scenario_inputs, run_model)

# ------------------------------------------------------------------------------
# 4. Summarize Precalculated Scenario Results
# ------------------------------------------------------------------------------

scenario_summary <- scenario_grid %>%
  mutate(
    scenario_id   = scenario_labels,
    expected_cost = sapply(scenario_results, get_costs),
    expected_eff  = sapply(scenario_results, get_effects)
  )

cat("\nSummary of Precalculated Scenario Results (First 10 scenarios):\n")
print(head(scenario_summary, 10))

cat("\nAll 30 scenario inputs successfully precalculated and executed!\n")
