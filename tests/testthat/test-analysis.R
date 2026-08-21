test_that("analysis returns combined cost and effectiveness values", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "Outcome1", 0.6,   100,   0.8,
    "A",        "root",  "Outcome2", 0.4,   200,   0.5
  )
  dt <- DecisionTree(dt_data)
  res <- run_model(dt)

  summary_vals <- analysis(res)

  expect_true(is.numeric(summary_vals))
  expect_equal(unname(summary_vals), c(140, 0.68))
})
