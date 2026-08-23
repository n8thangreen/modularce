test_that("run_model_heemod stops when heemod is not installed", {
  library(mockery)

  stub(run_model_heemod, "requireNamespace", FALSE)

  expect_error(
    run_model_heemod(list()),
    "Package 'heemod' is required for this adaptor. Please install it with install.packages\\('heemod'\\)."
  )
})

test_that("run_model_heemod handles valid input and uses mockery to avoid heemod", {
  library(mockery)

  stub(run_model_heemod, "requireNamespace", TRUE)

  mock_define_state <- mock(1, 2)
  stub(run_model_heemod, "heemod::define_state", mock_define_state)

  mock_define_transition <- mock(3)
  stub(run_model_heemod, "heemod::define_transition", mock_define_transition)

  mock_define_strategy <- mock(4)
  stub(run_model_heemod, "heemod::define_strategy", mock_define_strategy)

  # return a fake heemod_run object
  fake_res <- structure(list(.cycles = 1), class = "fake_heemod")
  mock_run_model <- mock(fake_res)
  stub(run_model_heemod, "heemod::run_model", mock_run_model)

  # get_counts is called twice per treatment: once for terminal probs, once for population_trace
  # so mock should have 2 values (or cycle = TRUE, but setting 2 values is safer)
  mock_get_counts <- mock(
    matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE),
    matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
  )
  stub(run_model_heemod, "heemod::get_counts", mock_get_counts)

  mock_summary <- function(res) {
    list(cost = 100, qaly = 5)
  }
  stub(run_model_heemod, "summary", mock_summary)

  model <- list(
    trans_matrix = array(c(0.9, 0.1, 0.2, 0.8), dim = c(2, 2, 1), dimnames = list(c("A", "B"), c("A", "B"), "tx1")),
    cost_matrix = array(c(10, 20), dim = c(2, 1, 1), dimnames = list(c("A", "B"), NULL, "tx1")),
    q_matrix = array(c(0.8, 0.4), dim = c(2, 1, 1), dimnames = list(c("A", "B"), NULL, "tx1")),
    init_probs = array(c(1, 0), dim = c(2, 1, 1), dimnames = list(c("A", "B"), NULL, "tx1")),
    n_cycles = 1,
    N = NA
  )

  class(model) <- "MarkovModel"

  res <- run_model_heemod(model)

  expect_s3_class(res, "output")
  expect_s3_class(res, "heemod_output")
  expect_equal(res$expected_cost, 100)
  expect_equal(res$expected_eff, 5)

  expect_called(mock_define_state, 2)
  expect_called(mock_define_transition, 1)
  expect_called(mock_define_strategy, 1)
  expect_called(mock_run_model, 1)
  expect_called(mock_get_counts, 2)
})
