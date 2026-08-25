test_that("plot_decision_tree_diagram returns a valid DiagrammeR object", {
  # Call with default parameters
  p <- plot_decision_tree_diagram()

  # Check it returns an htmlwidget and grViz object
  expect_s3_class(p, "grViz")
  expect_s3_class(p, "htmlwidget")

  # Check that the diagram contains default values
  expect_true(grepl("0.80", p$x$diagram) || grepl("0.8", p$x$diagram))
  expect_true(grepl("0.15", p$x$diagram))
  expect_true(grepl("0.05", p$x$diagram))
})

test_that("plot_decision_tree_diagram handles custom probabilities", {
  # Call with custom probabilities
  p <- plot_decision_tree_diagram(p_healthy = 0.5, p_preclin = 0.3, p_disease = 0.2)

  expect_s3_class(p, "grViz")

  # Check that the diagram contains the updated values
  expect_true(grepl("0.5", p$x$diagram))
  expect_true(grepl("0.3", p$x$diagram))
  expect_true(grepl("0.2", p$x$diagram))
})
