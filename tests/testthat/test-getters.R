test_that("get_costs.default and get_effects.default work", {
  # We should rely on standard testthat package loading
  # A simple list that does not have a class, so it uses the default methods
  results <- list(expected_cost = 100, expected_eff = 5)

  # Verify that default getters extract the correct values
  expect_equal(get_costs(results), 100)
  expect_equal(get_effects(results), 5)

  # Direct calls to the default methods as fallback
  expect_equal(get_costs.default(results), 100)
  expect_equal(get_effects.default(results), 5)

  # When expected_cost/eff are missing, it should return NULL
  results_empty <- list()
  expect_null(get_costs(results_empty))
  expect_null(get_effects(results_empty))
})
