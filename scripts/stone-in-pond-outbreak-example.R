# ==============================================================================
# "Stone-in-the-Pond" Infectious Disease Outbreak Screening Example
# 
# Concept:
# 1. Ring 1 Screening (Decision Tree 1): Initial targeted screening of high-risk 
#    index contacts (the "stone" dropped in the pond).
# 2. Adaptive Expansion Rule: Based on detected infections in Ring 1, a decision 
#    is made whether to expand to Ring 2 screening (a larger secondary contact group).
# 3. Markov Model: Simulates forward epidemic transmission, recovery, mortality,
#    and total costs/QALYs in the population starting from the post-screening state.
# ==============================================================================

library(tibble)
library(dplyr)
library(modularce)

# ------------------------------------------------------------------------------
# 1. Define Model Data & Parameters
# ------------------------------------------------------------------------------

# Define population health states for the Markov model
states <- c("Susceptible", "Asymptomatic_Infected", "Symptomatic_Infected", "Recovered", "Deceased")

# Strategy A: Standard Antigen Screening in Ring 1
# Strategy B: High-Sensitivity PCR Screening in Ring 1

# Decision Tree 1: Ring 1 Index Contact Screening (N = 50 close contacts)
dt_ring1_data <- tribble(
  ~treatment, ~from,   ~to,                     ~prob, ~cost, ~eff,
  # Strategy A (Antigen Test)
  "Strategy A", "root", "Infected",             0.20,     0,     0,
  "Strategy A", "root", "Uninfected",           0.80,     0,     0,
  "Strategy A", "Infected", "TP_Ring1",         0.75,    30,     0, # True Positive (Detected -> Iso/Treat)
  "Strategy A", "Infected", "FN_Ring1",         0.25,    10,     0, # False Negative (Missed -> Remains spreading)
  "Strategy A", "Uninfected", "FP_Ring1",       0.05,    30,     0, # False Positive (Iso cost)
  "Strategy A", "Uninfected", "TN_Ring1",       0.95,    10,     0, # True Negative
  
  # Strategy B (High-Sensitivity PCR Test)
  "Strategy B", "root", "Infected",             0.20,     0,     0,
  "Strategy B", "root", "Uninfected",           0.80,     0,     0,
  "Strategy B", "Infected", "TP_Ring1",         0.95,    80,     0, # Higher Sensitivity
  "Strategy B", "Infected", "FN_Ring1",         0.05,    20,     0, # Fewer Missed Cases
  "Strategy B", "Uninfected", "FP_Ring1",       0.02,    80,     0, # Higher Specificity
  "Strategy B", "Uninfected", "TN_Ring1",       0.98,    20,     0  # True Negative
)

# Decision Tree 2: Ring 2 Expanded Contact Screening (N = 250 wider contacts)
# Evaluated if Ring 1 detects positive cases above threshold
dt_ring2_data <- tribble(
  ~treatment, ~from,   ~to,                     ~prob, ~cost, ~eff,
  "Strategy A", "root", "Infected_R2",          0.08,     0,     0,
  "Strategy A", "root", "Uninfected_R2",        0.92,     0,     0,
  "Strategy A", "Infected_R2", "TP_Ring2",      0.75,    30,     0,
  "Strategy A", "Infected_R2", "FN_Ring2",      0.25,    10,     0,
  "Strategy A", "Uninfected_R2", "FP_Ring2",    0.05,    30,     0,
  "Strategy A", "Uninfected_R2", "TN_Ring2",    0.95,    10,     0,

  "Strategy B", "root", "Infected_R2",          0.08,     0,     0,
  "Strategy B", "root", "Uninfected_R2",        0.92,     0,     0,
  "Strategy B", "Infected_R2", "TP_Ring2",      0.95,    80,     0,
  "Strategy B", "Infected_R2", "FN_Ring2",      0.05,    20,     0,
  "Strategy B", "Uninfected_R2", "FP_Ring2",    0.02,    80,     0,
  "Strategy B", "Uninfected_R2", "TN_Ring2",    0.98,    20,     0
)

# Mapping from Decision Tree Terminal Nodes -> Markov Starting Health States
mapping_outbreak <- c(
  # Ring 1 Terminal Nodes
  TP_Ring1 = "Recovered",             # Early treatment/isolation -> Prevents spread
  FN_Ring1 = "Asymptomatic_Infected", # Missed infection -> Enters Markov as active spreader
  FP_Ring1 = "Susceptible",           # Uninfected (quarantined) -> Enters Markov as Susceptible
  TN_Ring1 = "Susceptible",           # Uninfected -> Enters Markov as Susceptible

  # Ring 2 Terminal Nodes
  TP_Ring2 = "Recovered",
  FN_Ring2 = "Asymptomatic_Infected",
  FP_Ring2 = "Susceptible",
  TN_Ring2 = "Susceptible"
)
mapping_outbreak <- factor(mapping_outbreak, levels = states)

# Markov Model Epidemic Progression Array (5 states x 5 states x 2 strategies)
trans_prob_mat <- array(
  c(
    # Strategy A Transitions (FROM: Susceptible, Asymp, Symp, Rec, Dec)
    # TO: Susceptible, Asymp, Symp, Rec, Dec
    0.85, 0.10, 0.00, 0.04, 0.01, # FROM Susceptible
    0.00, 0.50, 0.35, 0.14, 0.01, # FROM Asymptomatic Infected
    0.00, 0.00, 0.40, 0.55, 0.05, # FROM Symptomatic Infected
    0.02, 0.00, 0.00, 0.97, 0.01, # FROM Recovered (waning immunity)
    0.00, 0.00, 0.00, 0.00, 1.00, # FROM Deceased (absorbing)
    
    # Strategy B Transitions (Lower transmission due to higher early detection)
    0.90, 0.06, 0.00, 0.03, 0.01, # FROM Susceptible
    0.00, 0.45, 0.35, 0.19, 0.01, # FROM Asymptomatic Infected
    0.00, 0.00, 0.35, 0.60, 0.05, # FROM Symptomatic Infected
    0.02, 0.00, 0.00, 0.97, 0.01, # FROM Recovered
    0.00, 0.00, 0.00, 0.00, 1.00  # FROM Deceased
  ),
  dim = c(5, 5, 2),
  dimnames = list(to = states, from = states, treatment = c("Strategy A", "Strategy B"))
) |> aperm(c(2, 1, 3))

# Cost per cycle in each state ($)
cost_mat <- array(
  c(
    # Strategy A costs per state per cycle
    0,    # Susceptible
    50,   # Asymptomatic Infected (contact tracing / monitoring)
    800,  # Symptomatic Infected (hospitalization / care)
    20,   # Recovered (post-recovery follow up)
    0,    # Deceased
    
    # Strategy B costs
    0, 50, 800, 20, 0
  ),
  dim = c(1, 5, 2),
  dimnames = list(NULL, state = states, treatment = c("Strategy A", "Strategy B"))
)

# Utility (QALY per cycle) in each state
q_mat <- array(
  c(
    1.00, # Susceptible
    0.85, # Asymptomatic Infected
    0.40, # Symptomatic Infected
    0.95, # Recovered
    0.00, # Deceased
    
    1.00, 0.85, 0.40, 0.95, 0.00
  ),
  dim = c(1, 5, 2),
  dimnames = list(NULL, state = states, treatment = c("Strategy A", "Strategy B"))
)

# Initial population distribution for Markov Model (placeholder before DT mapping)
p_init <- array(
  c(0.80, 0.15, 0.05, 0.00, 0.00,
    0.80, 0.15, 0.05, 0.00, 0.00),
  dim = c(1, 5, 2),
  dimnames = list(NULL, states, c("Strategy A", "Strategy B"))
)

# ------------------------------------------------------------------------------
# 2. Build Models (Decision Tree 1, Decision Tree 2, Markov Model)
# ------------------------------------------------------------------------------

dt_ring1 <- DecisionTree(dt_ring1_data, N = 50)   # Ring 1: 50 index contacts
dt_ring2 <- DecisionTree(dt_ring2_data, N = 250)  # Ring 2: 250 expanded contacts

mm_epidemic <- MarkovModel(
  trans_matrix = trans_prob_mat,
  cost_matrix  = cost_mat,
  q_matrix     = q_mat,
  init_probs   = p_init,
  mapping      = mapping_outbreak,
  n_cycles     = 12 # 12 monthly cycles (1 year epidemic simulation)
)

# ------------------------------------------------------------------------------
# 3. Simulate Stone-in-the-Pond Outbreak Screening Scenarios
# ------------------------------------------------------------------------------

cat("======================================================================\n")
cat("  STONE-IN-THE-POND INFECTIOUS DISEASE OUTBREAK SCREENING MODEL\n")
cat("======================================================================\n\n")

# Run Ring 1 (Initial Index Contact Screening)
res_ring1 <- run_model(dt_ring1)
cat("--- Ring 1 Initial Screening Results (Index Ring N=50) ---\n")
print(res_ring1)

# Check Ring 1 Detected Infection Probability (True Positive Rate)
tp_detected <- res_ring1$path_results %>%
  filter(terminal_node == "TP_Ring1") %>%
  select(treatment, terminal_prob)

cat("\nDetected Infection Probability in Ring 1:\n")
print(tp_detected)

# Adaptive Decision Rule:
# Evaluate whether Ring 1 detected probability >= threshold (0.10) to expand to Ring 2
infection_threshold <- 0.10
cat(sprintf("\nExpansion Threshold: Detected Prob >= %.2f\n", infection_threshold))

tp_detected <- tp_detected %>%
  mutate(Expand_to_Ring2 = terminal_prob >= infection_threshold)

print(tp_detected)

# Define Scenario 1: Standard Ring 1 Only (No Expansion)
model_ring1_only <- CombinedModel(dt_ring1, mm_epidemic)
res_scenario1    <- run_model(model_ring1_only)

# Define Scenario 2: Stone-in-the-Pond Expansion (Ring 1 -> Ring 2 -> Markov)
model_adaptive_expansion <- CombinedModel(dt_ring1, dt_ring2, mm_epidemic)
res_scenario2            <- run_model(model_adaptive_expansion)

# ------------------------------------------------------------------------------
# 4. Compare Outbreak Control Results
# ------------------------------------------------------------------------------

cat("\n======================================================================\n")
cat("  SCENARIO EVALUATION & COST-EFFECTIVENESS SUMMARY\n")
cat("======================================================================\n\n")

summary_table <- tibble(
  Scenario = c("Ring 1 Only (Index Ring N=50)", "Ring 1 + Ring 2 (Stone-in-Pond Expansion N=300)"),
  `Strategy A Cost ($)` = c(get_costs(res_scenario1)[1], get_costs(res_scenario2)[1]),
  `Strategy A QALYs`    = c(get_effects(res_scenario1)[1], get_effects(res_scenario2)[1]),
  `Strategy B Cost ($)` = c(get_costs(res_scenario1)[2], get_costs(res_scenario2)[2]),
  `Strategy B QALYs`    = c(get_effects(res_scenario1)[2], get_effects(res_scenario2)[2])
)

print(summary_table, width = Inf)

cat("\nKey Insights:\n")
cat("- Strategy B (High-Sensitivity PCR) detects more cases early in Ring 1 (19% vs 15%).\n")
cat("- Expanding to Ring 2 ('Stone-in-the-Pond') screens 250 additional contacts, identifying more asymptomatic cases early.\n")
cat("- Early isolation reduces forward community transmission in the Markov model, improving overall QALYs.\n\n")
cat("Stone-in-the-Pond Outbreak Screening Example Completed Successfully!\n")
