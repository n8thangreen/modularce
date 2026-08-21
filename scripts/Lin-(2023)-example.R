# ==========================================================
# example script from:
# Lin, Y. S., O’Mahony, J. F., & van Rosmalen, J. (2023)
# A Simple Cost-Effectiveness Model of Screening: An Open-Source Teaching and Research Tool Coded in R. PharmacoEconomics - Open, 7(4), 507–523
# https://doi.org/10.1007/s41669-023-00414-1
# ==========================================================


library(tibble)
library(dplyr)


## define model data

decision_tree <- tribble(
  ~treatment, ~from,                ~to,              ~prob, ~cost,  ~eff,
  # Initial population split (true underlying state for Model A)
  "A",        "root",               "Healthy",        0.8,    0,      0, # Higher prob of starting healthy
  "A",        "root",               "Pre_clinical_Disease", 0.15,  0, 0,
  "A",        "root",               "Disease",        0.05,   0,      0,
  # Direct transitions to death states
  "A",        "root",               "Death_from_Disease", 0.0,    0,  0,
  "A",        "root",               "Other_Cause_Death",  0.0,    0,  0,
  "A",        "root",               "Treated",        0.0,    0,  0,
  
  # starting "Healthy" (true health status)
  "A",        "Healthy",            "FP",             0.1,   200,     0,    # False Positive
  "A",        "Healthy",            "TN",             0.9,   50,      0,    # True Negative
  
  # starting "Pre-clinical Disease" (true health status)
  # Assuming diagnostic test operates differently or has different outcomes
  "A",        "Pre_clinical_Disease", "TP_Preclinical", 0.7,   200,     0,  # True Positive for pre-clinical
  "A",        "Pre_clinical_Disease", "FN_Preclinical", 0.3,   50,      0,  # False Negative for pre-clinical
  
  # starting "Disease" (true health status)
  # Assuming diagnostic test operates differently or has different outcomes
  "A",        "Disease",            "TP_Disease",     1,   200,     0,     # True Positive for active disease
  "A",        "Disease",            "FN_Disease",     0,   50,      0,     # False Negative for active disease
)

# markov model
states <- c("Healthy", "Pre_clinical_Disease", "Disease", "Treated", "Death_from_Disease", "Other_Cause_Death")

# Mapping from decision tree terminal nodes to Markov states
# same for all treatments
#          # dt        mm        
mapping <- c(Treated = "Treated",
             FP      = "Treated",
             TN      = "Healthy",
             TP_Preclinical = "Treated",
             FN_Preclinical = "Pre_clinical_Disease",
             TP_Disease     = "Treated",
             FN_Disease     = "Disease",
             Death_from_Disease = "Death_from_Disease",
             Other_Cause_Death  = "Other_Cause_Death")

# need to order states
mapping <- factor(mapping, levels = states)

# transition probabilities in markov model
trans_prob_mat <- array(
  c(
    # FROM Healthy (H)
    0.90,  # To Healthy
    0.05,  # To Pre-clinical Disease
    0.00,  # To Disease (assuming progression via pre-clinical)
    0.00,  # To Treated (assuming treatment only after diagnosis)
    0.00,  # To Death from Disease (assuming no direct death from healthy)
    0.05,  # To Other-Cause Death
    
    # FROM Pre-clinical Disease (P)
    0.00, # To Healthy (assuming no spontaneous recovery to healthy from pre-clinical)
    0.65, # To Pre-clinical Disease (staying untreated)
    0.20, # To Disease (progressing to active disease)
    0.00, # To Treated (initiating treatment at pre-clinical stage, if applicable)
    0.10, # To Death from Disease (due to progression during pre-clinical stage)
    0.05, # To Other-Cause Death
    
    # FROM Disease (D)
    0.00, # To Healthy
    0.00, # To Pre-clinical Disease (unlikely to regress to pre-clinical)
    0.60, # To Disease (remaining in active disease, untreated)
    0.20, # To Treated (initiating treatment for active disease)
    0.20, # To Death from Disease (progression of active disease)
    0.00, # To Other-Cause Death
    
    # FROM Treated (T) - Adjusted for new "Disease" state
    0.00, # To Healthy
    0.00, # To Pre-clinical Disease (unlikely from treated state)
    0.00, # To Disease (treatment failure/relapse to active disease)
    0.95, # To Treated (remaining treated and stable)
    0.00, # To Death from Disease (despite treatment)
    0.05, # To Other-Cause Death
    
    # FROM Death from Disease (DD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00, # All to Death from Disease
    
    # FROM Other-Cause Death (OCD) - Absorbing state
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00  # All to Other-Cause Death
  ),
  dim = c(6, 6, 1),
  dimnames = list(to = states,
                  from = states,
                  scenario = "A")
) |> 
  aperm(c(2, 1, 3))  # rearrange dimensions

cost_mat <- array(
  c(
    0,     # Healthy
    0,     # Pre-clinical Disease
    0,     # Disease
    100,   # Treated
    0,     # Death from Disease
    0      # Other-Cause Death
  ),
  dim = c(1, 6, 1), # 1 row, 6 states, 1 scenario
  dimnames = list(NULL,
                  state = states,
                  scenario = "A")
)

q_mat <- array(
  c(
    1,      # Healthy
    0.8,    # Pre-clinical Disease
    0.5,    # Disease
    0.9,    # Treated
    0,      # Death from Disease
    0       # Other-Cause Death
  ),
  dim = c(1, 6, 1), # 1 row, 6 states, 1 scenario
  dimnames = list(NULL,
                  state = states,
                  scenario = "A")
)

p_init <- array(c(1, 0, 0, 0, 0, 0),
                dim = c(1, 6, 1),
                dimnames = list(
                  NULL,
                  c("Healthy", "Pre_clinical_Disease", "Disease",
                    "Treated", "Death_from_Disease", "Other_Cause_Death"),
                  "A"))

## build models

dt <- DecisionTree(decision_tree)

# every 2 years
mm2 <- MarkovModel(trans_matrix = trans_prob_mat,
                   cost_matrix = cost_mat,
                   q_matrix = q_mat,
                   init_probs = p_init,
                   mapping = mapping,
                   n_cycles = 2)

# every 5 years
mm5 <- MarkovModel(trans_matrix = trans_prob_mat,
                   cost_matrix = cost_mat,
                   q_matrix = q_mat,
                   init_probs = p_init,
                   mapping = mapping,
                   n_cycles = 5)

# every 10 years
mm10 <- MarkovModel(trans_matrix = trans_prob_mat,
                    cost_matrix = cost_mat,
                    q_matrix = q_mat,
                    init_probs = p_init,
                    mapping = mapping,
                    n_cycles = 10)
#############
# run models

# single
run_model(dt)

res5 <- run_model(mm5)
res10 <- run_model(mm10)

pair_model2 <- CombinedModel(dt, mm2)
pair_result2 <- run_model(pair_model2)

pair_model5 <- CombinedModel(dt, mm5)
pair_result5 <- run_model(pair_model5)

pair_model10 <- CombinedModel(dt, mm10)
pair_model10 <- CombinedModel(dt, mm10)

results5 <-
  CombinedModel(pair_model5, pair_model5) |> 
  run_model()

# time horizon 20 years
screening_submodels2 <- rep(pair_model2, 10)
screening_model2 <- do.call(CombinedModel, args = screening_submodels2)
screening_results2 <- run_model(screening_model2)

screening_submodels5 <- rep(pair_model5, 4)
screening_model5 <- do.call(CombinedModel, args = screening_submodels5)
screening_results5 <- run_model(screening_model5)

screening_submodels10 <- rep(pair_model10, 2)
screening_model10 <- do.call(CombinedModel, args = screening_submodels10)
screening_results10 <- run_model(screening_model10)


#########################
# loop through scenarios

##TODO: should have the final markov model to death
##      e.g. for 100 years or until prob death = 1

t_between_screens <- c(2, 5, 10, 15, 20)
num_screens <- c(1, 2, 3, 4, 5, 10)  # -> time horizon

all_results <- list()

dt <- DecisionTree(decision_tree)

scenario_counter <- 1

for (t_val in t_between_screens) {
  
  mm_scenario <- MarkovModel(trans_matrix = trans_prob_mat,
                             cost_matrix = cost_mat,
                             q_matrix = q_mat,
                             init_probs = p_init,
                             mapping = mapping,
                             n_cycles = t_val)
  
  pair_model_scenario <- CombinedModel(dt, mm_scenario)
  
  for (n_scr in num_screens) {
    
    screening_submodels <- rep(pair_model_scenario, n_scr)
    
    screening_model <- do.call(CombinedModel, args = screening_submodels)
    
    results_scenario <- run_model(screening_model)
    
    result_label <- paste0("t_between_", t_val, "_screens_", n_scr)
    
    all_results[[result_label]] <- list(
      t_between_screens = t_val,
      num_screens = n_scr,
      results = results_scenario)
    
    scenario_counter <- scenario_counter + 1
  }
}


