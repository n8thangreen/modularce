# script
# update models inside of run_model()
#

library(tibble)
library(dplyr)


##########
# EXAMPLE 
##########

## define model data

decision_tree <- tibble(
  decision = rep(c("Treatment A", "Treatment B"), each = 2),
  outcome = c("Success", "Failure",
              "Success", "Failure"),
  probability = c(0.7, 0.3, 0.6, 0.4),
  cost = c(1000, 2000, 800, 2500),
  effectiveness = c(0.9, 0.5, 0.85, 0.4))

decision_tree

# # A tibble: 4 × 5
#   decision    outcome probability  cost effectiveness
#   <chr>       <chr>         <dbl> <dbl>         <dbl>
# 1 Treatment A Success         0.7  1000          0.9 
# 2 Treatment A Failure         0.2  2000          0.5 
# 3 Treatment B Success         0.6   800          0.85
# 4 Treatment B Failure         0.3  2500          0.4 

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
