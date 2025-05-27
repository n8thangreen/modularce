# script
# update models inside of run_model()
#

library(tibble)
library(dplyr)


##########
# EXAMPLE 
##########

## define model data

decision_tree <- tribble(
  ~treatment,    ~from,   ~to,     ~prob, ~cost, ~eff,
  "Treatment A", "root",  "healthy", 0.7,   100,   0.8,
  "Treatment A", "root",  "sick",    0.3,   150,   0.75,
  "Treatment A", "healthy", "pos",   0.9,   50,    0.95,
  "Treatment A", "healthy", "neg",   0.1,   200,   0.6,
  "Treatment A", "sick",  "pos",     0.8,   75,    0.9,
  "Treatment A", "sick",  "neg",     0.2,   120,   0.7,
  
  "Treatment B", "root",  "healthy", 0.6,   80,    0.85,
  "Treatment B", "root",  "sick",    0.4,   130,   0.7,
  "Treatment B", "healthy", "pos",   0.92,  40,    0.98,
  "Treatment B", "healthy", "neg",   0.08,  180,   0.65,
  "Treatment B", "sick",  "pos",     0.85,  65,    0.92,
  "Treatment B", "sick",  "neg",     0.15,  110,   0.72
)

# Define dummy Markov model parameters (using a 3D array for transition probabilities)
trans_prob_mat <- array(c(0.9, 0.1, 0.2, 0.8,
                          0.9, 0.1, 0.2, 0.8),
                        dim = c(2, 2, 2),
                        dimnames = list(NULL, NULL, c("A", "B")))

cost_mat <- array(c(1000, 2000),
                  dim = c(1, 2, 2),
                  dimnames = list(NULL, NULL, c("A", "B")))

q_mat <- array(c(1, 0),
               dim = c(1, 2, 2),
               dimnames = list(NULL, NULL, c("A", "B")))

# Mapping from decision tree terminal nodes to Markov states
# same for all treatments
mapping <- c(1, 2)


## build models

dt <- DecisionTree(decision_tree)
dt_N <- DecisionTree(decision_tree, N = 100)

mm <- MarkovModel(trans_matrix = trans_prob_mat,
                  cost_matrix = cost_mat,
                  q_matrix = q_mat,
                  mapping = mapping)

dt2 <- DecisionTree(decision_tree)


## run models

run_model(dt)
run_model(mm)

full_model <- CombinedModel(dt, mm)

final_result <- run_model(full_model)

# could use BCEA package for this
# cea_res <- analysis(final_result)
