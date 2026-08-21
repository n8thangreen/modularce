test_that("MarkovModel constructor creates valid S3 object", {
  trans_prob_mat <- array(c(0.9, 0.1, 0.2, 0.8), dim = c(2, 2, 1), dimnames = list(NULL, NULL, "A"))
  cost_mat <- array(c(100, 200), dim = c(1, 2, 1), dimnames = list(NULL, NULL, "A"))
  q_mat <- array(c(1, 0.5), dim = c(1, 2, 1), dimnames = list(NULL, NULL, "A"))
  p_init <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "S"), "A"))

  mm <- MarkovModel(
    trans_matrix = trans_prob_mat,
    cost_matrix = cost_mat,
    q_matrix = q_mat,
    init_probs = p_init,
    n_cycles = 5
  )

  expect_s3_class(mm, "MarkovModel")
  expect_s3_class(mm, "Model")
  expect_equal(mm$n_cycles, 5)
})

test_that("markov_model calculates cycle transitions accurately", {
  # 2 states (Healthy, Dead), 1 treatment
  # Healthy -> Healthy (0.9), Healthy -> Dead (0.1)
  # Dead -> Dead (1.0)
  trans_prob <- array(c(0.9, 0.1, 0.0, 1.0), dim = c(2, 2, 1), dimnames = list(to = c("H", "D"), from = c("H", "D"), "A"))
  trans_prob <- aperm(trans_prob, c(2, 1, 3)) # rearrange (from, to, treatment)

  cost_mat <- array(c(10, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))
  q_mat <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))
  p_init <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))

  out <- markov_model(
    start_pop = p_init,
    p_matrix = trans_prob,
    state_c_matrix = cost_mat,
    state_q_matrix = q_mat,
    n_cycles = 3,
    s_names = c("H", "D"),
    t_names = "A"
  )

  expect_equal(dim(out$pop), c(2, 3, 1))
  # Cycle 1: [1, 0]
  expect_equal(as.vector(out$pop[, 1, 1]), c(1, 0))
  # Cycle 2: [0.9, 0.1]
  expect_equal(as.vector(out$pop[, 2, 1]), c(0.9, 0.1))
  # Cycle 3: [0.81, 0.19]
  expect_equal(as.vector(out$pop[, 3, 1]), c(0.81, 0.19))
})

test_that("run_model.MarkovModel produces structured output", {
  trans_prob <- array(c(0.9, 0.1, 0.0, 1.0), dim = c(2, 2, 1), dimnames = list(c("H", "D"), c("H", "D"), "A"))
  cost_mat <- array(c(10, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))
  q_mat <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))
  p_init <- array(c(1, 0), dim = c(1, 2, 1), dimnames = list(NULL, c("H", "D"), "A"))

  mm <- MarkovModel(
    trans_matrix = trans_prob,
    cost_matrix = cost_mat,
    q_matrix = q_mat,
    init_probs = p_init,
    n_cycles = 3
  )

  res <- run_model(mm)

  expect_s3_class(res, "output")
  expect_s3_class(res, "MarkovModel")
  expect_named(res, c("terminal", "expected_cost", "expected_eff", "pop"))
  expect_true(is.numeric(get_costs(res)))
  expect_true(is.numeric(get_effects(res)))
})
