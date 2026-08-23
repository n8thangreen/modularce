test_that("CombinedModel constructor validates input objects", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "Healthy",  1.0,   0,     0
  )
  dt <- DecisionTree(dt_data)

  expect_error(CombinedModel(dt), "At least two models must be provided")
  expect_error(CombinedModel(dt, "not_a_model"), "All arguments must be of class 'Model'")
})

test_that("map_decision_to_markov maps terminal probabilities to initial Markov states", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "Node1",    0.7,   10,    0.5,
    "A",        "root",  "Node2",    0.3,   20,    0.3
  )
  dt <- DecisionTree(dt_data)
  dt_res <- run_model(dt)

  mapping <- c(Node1 = "State1", Node2 = "State2")
  mapping <- factor(mapping, levels = c("State1", "State2"))

  init_arr <- map_decision_to_markov(dt_res, mapping)

  expect_equal(dim(init_arr), c(1, 2, 1)) # 1 row, 2 states, 1 treatment
  expect_equal(colnames(init_arr[, , 1, drop = FALSE]), c("State1", "State2"))
  expect_equal(as.vector(unlist(init_arr)), c(0.7, 0.3))
})

test_that("CombinedModel executes model chain and updates intermediate inputs", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "H",        0.8,   50,    0.9,
    "A",        "root",  "S",        0.2,   200,   0.4
  )
  dt <- DecisionTree(dt_data)

  states <- c("H", "S")
  mapping <- c(H = "H", S = "S")
  mapping <- factor(mapping, levels = states)

  trans_prob <- array(c(0.9, 0.1, 0.0, 1.0), dim = c(2, 2, 1), dimnames = list(states, states, "A"))
  cost_mat <- array(c(10, 0), dim = c(1, 2, 1), dimnames = list(NULL, states, "A"))
  q_mat <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, states, "A"))

  mm <- MarkovModel(
    trans_matrix = trans_prob,
    cost_matrix = cost_mat,
    q_matrix = q_mat,
    mapping = mapping,
    n_cycles = 2
  )

  combined <- CombinedModel(dt, mm)
  results <- run_model(combined)

  expect_s3_class(results, "CombinedModel")
  expect_length(results, 2)
  expect_s3_class(results[[1]], "DecisionTree")
  expect_s3_class(results[[2]], "MarkovModel")
  expect_true(get_costs(results) > 0)
  expect_true(get_effects(results) > 0)
})

test_that("map_markov_to_decision joins Markov terminal probabilities to Decision Tree data", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "State1",    0.5,   10,    0.5,
    "A",        "root",  "State2",    0.5,   20,    0.3,
    "B",        "root",  "State1",    0.2,   10,    0.5,
    "B",        "root",  "State2",    0.8,   20,    0.3,
    "A",        "State1","Terminal1", 1.0,   0,     0
  )
  dt <- DecisionTree(dt_data)

  markov_result <- list(terminal = tibble::tribble(
    ~state,   ~treatment, ~probs,
    "State1", "A",        0.9,
    "State2", "A",        0.1,
    "State1", "B",        0.7,
    "State2", "B",        0.3
  ))

  res <- map_markov_to_decision(dt, markov_result)

  # Check probabilities for treatment A
  prob_A_State1 <- res$prob[res$treatment == "A" & res$to == "State1"]
  expect_equal(prob_A_State1, 0.9)

  prob_A_State2 <- res$prob[res$treatment == "A" & res$to == "State2"]
  expect_equal(prob_A_State2, 0.1)

  # Check probabilities for treatment B
  prob_B_State1 <- res$prob[res$treatment == "B" & res$to == "State1"]
  expect_equal(prob_B_State1, 0.7)

  prob_B_State2 <- res$prob[res$treatment == "B" & res$to == "State2"]
  expect_equal(prob_B_State2, 0.3)

  # Check probability that shouldn't change
  prob_A_Terminal1 <- res$prob[res$treatment == "A" & res$to == "Terminal1"]
  expect_equal(prob_A_Terminal1, 1.0)
})
