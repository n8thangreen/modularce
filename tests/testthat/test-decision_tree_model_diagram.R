test_that("plot_decision_tree_diagram prevents path traversal", {
  expect_error(
    plot_decision_tree_diagram(file = "../invalid_path.png"),
    "Invalid file path: (directory does not exist or path is invalid.|path must be within the safe_dir.)"
  )

  expect_error(
    plot_decision_tree_diagram(file = "/etc/passwd"),
    "Invalid file path: (directory does not exist or path is invalid.|path must be within the safe_dir.)"
  )
})
