# basic example script
#
# two state Markov model
# imperfect test

library(tibble)
library(dplyr)

##########
# EXAMPLE 
##########

## define model data

decision_tree <- tribble(
  ~treatment,    ~from,   ~to,       ~prob, ~cost, ~eff,
  "A",           "root",  "healthy", 0.7,   0,     0,
  "A",           "root",  "sick",    0.3,   0,     0,
  "A",           "healthy", "FP",    0.9,   50,    0.95,
  "A",           "healthy", "TN",    0.1,   200,   0.6,
  "A",           "sick",  "TP",      0.8,   75,    0.9,
  "A",           "sick",  "FN",      0.2,   120,   0.7,
            
  "B",           "root",  "healthy", 0.6,   0,     0,
  "B",           "root",  "sick",    0.4,   0,     0,
  "B",           "healthy", "FP",   0.92,  40,    0.98,
  "B",           "healthy", "TN",   0.08,  180,   0.65,
  "B",           "sick",  "TP",     0.85,  65,    0.92,
  "B",           "sick",  "FN",     0.15,  110,   0.72,
)

# Define dummy Markov model
# parameters (using a 3D array for transition probabilities)
trans_prob_mat <- array(c(0.9, 0.2, 0.1, 0.8,
                          0.9, 0.2, 0.1, 0.8),
                        dim = c(2, 2, 2),
                        dimnames = list(NULL, NULL, c("A", "B")))

cost_mat <- array(c(1000, 2000),
                  dim = c(1, 2, 2),
                  dimnames = list(NULL, NULL, c("A", "B")))

q_mat <- array(c(1, 0),
               dim = c(1, 2, 2),
               dimnames = list(NULL, NULL, c("A", "B")))

p_init <- array(c(1, 0),
                dim = c(1, 2, 2),
                dimnames = list(NULL,
                                c("healthy", "sick"),
                                c("A", "B")))

# Mapping from decision tree terminal nodes to Markov states
# same for all treatments
mapping <- c(FP = "healthy", TP = "healthy", TN = "healthy", FN = "sick")

## build models

dt <- DecisionTree(decision_tree)
dt_N <- DecisionTree(decision_tree, N = 100)

mm0 <- MarkovModel(trans_matrix = trans_prob_mat,
                   cost_matrix = cost_mat,
                   q_matrix = q_mat,
                   init_probs = p_init)

mm <- MarkovModel(trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat,
                  q_matrix = q_mat,
                  mapping = mapping)

#############
# run models

# single
run_model(dt)
run_model(mm0)

full_model <- CombinedModel(dt, mm)
final_result <- run_model(full_model)

full_model2 <- CombinedModel(mm0, dt)
final_result2 <- run_model(full_model2)

# 3 steps
full_model3 <- CombinedModel(dt, mm, dt)
final_result3 <- run_model(full_model3)



# could use BCEA package for this
# cea_res <- analysis(final_result)
