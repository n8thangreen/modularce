# example script from:
# Lin, Y. S., O’Mahony, J. F., & van Rosmalen, J. (2023)
# A Simple Cost-Effectiveness Model of Screening: An Open-Source Teaching and Research Tool Coded in R. PharmacoEconomics - Open, 7(4), 507–523
# https://doi.org/10.1007/s41669-023-00414-1


library(tibble)
library(dplyr)

## define model data

decision_tree <- tribble(
  ~treatment, ~from,                ~to,              ~prob, ~cost,  ~eff,
  # Initial population split (true underlying state for Model A)
  "A",        "root",               "Healthy",        0.65,   0,      1.0, # Higher prob of starting healthy
  "A",        "root",               "Pre_clinical_Disease", 0.15,   0,      0.9,
  "A",        "root",               "Disease",        0.05,   0,      0.5,
  # Direct transitions to death states
  "A",        "root",               "Death_from_Disease", 0.05,   0,  0,
  "A",        "root",               "Other_Cause_Death",  0.1,    0,  0,
  
  # If starting "Healthy" (true health status)
  "A",        "Healthy",            "FP",             0.1,   50,     0.95, # False Positive
  "A",        "Healthy",            "TN",             0.9,   200,    0.6,  # True Negative
  
  # If starting "Pre-clinical Disease" (true health status)
  # Assuming diagnostic test operates differently or has different outcomes
  "A",        "Pre-clinical Disease", "TP_Preclinical", 0.7,   75,     0.9,  # True Positive for pre-clinical
  "A",        "Pre-clinical Disease", "FN_Preclinical", 0.3,   120,    0.7,  # False Negative for pre-clinical
  
  # If starting "Disease" (true health status)
  # Assuming diagnostic test operates differently or has different outcomes
  "A",        "Disease",            "TP_Disease",     1,   80,     0.92, # True Positive for active disease
  "A",        "Disease",            "FN_Disease",     0,   130,    0.68, # False Negative for active disease
  
  # --- Repeat for Treatment B ---
  "B",        "root",               "Healthy",        0.55,   0,      1.0,
  "B",        "root",               "Pre_clinical Disease", 0.2,   0,      0.92,
  "B",        "root",               "Disease",        0.05,   0,      0.55,
  # Direct transitions to death states
  "B",        "root",               "Death_from_Disease", 0.05,   9000,   0,
  "B",        "root",               "Other_Cause_Death",  0.15,    4000,   0,
  
  # If starting "Healthy" (true health status)
  "B",        "Healthy",            "FP",             0.08,  40,     0.98,
  "B",        "Healthy",            "TN",             0.92,  180,    0.65,
  
  # If starting "Pre-clinical Disease" (true health status)
  "B",        "Pre-clinical Disease", "TP_Preclinical", 0.75,  65,     0.92,
  "B",        "Pre-clinical Disease", "FN_Preclinical", 0.25,  110,    0.72,
  
  # If starting "Disease" (true health status)
  "B",        "Disease",            "TP_Disease",     0.85,  70,     0.94,
  "B",        "Disease",            "FN_Disease",     0.15,  120,    0.70,
)

# Mapping from decision tree terminal nodes to Markov states
# same for all treatments
mapping <- c(FP = "treated", TN = "healthy",
             TP_Preclinical = "treated", FN_Preclinical = "Pre_clinical_Disease",
             TP_Disease = "treated", FN_Disease = "Disease",
             Death_from_Disease = "Death_from_Disease", Other_Cause_Death = "Other_Cause_Death")

# Define state names for clarity
states <- c("Healthy", "Pre_clinical_Disease", "Disease", "Treated", "Death_from_Disease", "Other_Cause_Death")

# Example probabilities (REPLACE WITH YOUR OWN DATA!)
# Each row must sum to 1.
trans_prob_mat <- array(
  c(
    # FROM Healthy (H)
    0.80, # To Healthy
    0.15, # To Pre-clinical Disease
    0.00, # To Disease (assuming progression via pre-clinical)
    0.00, # To Treated (assuming treatment only after diagnosis)
    0.00, # To Death from Disease (assuming no direct death from healthy)
    0.05, # To Other-Cause Death
    
    # FROM Pre-clinical Disease (P)
    0.00, # To Healthy (assuming no spontaneous recovery to healthy from pre-clinical)
    0.60, # To Pre-clinical Disease (staying untreated)
    0.20, # To Disease (progressing to active disease)
    0.10, # To Treated (initiating treatment at pre-clinical stage, if applicable)
    0.05, # To Death from Disease (due to progression during pre-clinical stage)
    0.05, # To Other-Cause Death
    
    # FROM Disease (D) - New state
    0.00, # To Healthy (unlikely without treatment)
    0.00, # To Pre-clinical Disease (unlikely to regress to pre-clinical)
    0.60, # To Disease (remaining in active disease, untreated)
    0.30, # To Treated (initiating treatment for active disease)
    0.05, # To Death from Disease (progression of active disease)
    0.05, # To Other-Cause Death
    
    # FROM Treated (T) - Adjusted for new "Disease" state
    0.05, # To Healthy (recovery due to treatment? - adjust if no recovery)
    0.00, # To Pre-clinical Disease (unlikely from treated state)
    0.05, # To Disease (treatment failure/relapse to active disease)
    0.80, # To Treated (remaining treated and stable)
    0.05, # To Death from Disease (despite treatment)
    0.05, # To Other-Cause Death
    
    # FROM Death from Disease (DD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00, # All to Death from Disease
    
    # FROM Other-Cause Death (OCD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00  # All to Other-Cause Death
  ),
  dim = c(6, 6, 1), # Now a 6x6 matrix
  dimnames = list(from = states,
                  to = states,
                  scenario = "Default")
)

cost_mat <- array(
  c(
    50,    # Cost in Healthy
    500,   # Cost in Pre-clinical Disease
    1000,  # Cost in Disease
    1500,  # Cost in Treated
    0,     # Cost in Death from Disease
    0      # Cost in Other-Cause Death
  ),
  dim = c(1, 6, 1), # 1 row, 6 states, 1 scenario
  dimnames = list(NULL,
                  state = states,
                  scenario = "Default")
)

q_mat <- array(
  c(
    1,    # Healthy
    0,    # Pre-clinical Disease
    0,    # Disease
    0,    # Treated
    0,    # Death from Disease
    0     # Other-Cause Death
  ),
  dim = c(1, 6, 1), # 1 row, 6 states, 1 scenario
  dimnames = list(NULL,
                  state = states,
                  scenario = "Default")
)

## build models

dt <- DecisionTree(decision_tree)
dt_N <- DecisionTree(decision_tree, N = 100)

mm0 <- MarkovModel(trans_matrix = trans_prob_mat,
                   cost_matrix = cost_mat,
                   q_matrix = q_mat,
                   init_probs = data.frame(
                     state = c("Healthy", "Pre_clinical_Disease", "Disease", "Treated", "Death_from_Disease", "Other_Cause_Death"),
                     p = c(1, 0, 0, 0, 0, 0)))

mm <- MarkovModel(trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat,
                  q_matrix = q_mat,
                  mapping = mapping)

dt2 <- DecisionTree(decision_tree)


## run models

# single
run_model(dt)
run_model(mm0)

full_model <- CombinedModel(dt, mm)
final_result <- run_model(full_model)

full_model2 <- CombinedModel(mm0, dt)
final_result2 <- run_model(full_model2)




# could use BCEA package for this
# cea_res <- analysis(final_result)
