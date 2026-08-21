test_that("DecisionTree constructor creates valid S3 object", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "Healthy",  0.7,   0,     0,
    "A",        "root",  "Sick",     0.3,   0,     0,
    "A",        "Healthy", "TN",     0.9,   50,    0.9,
    "A",        "Healthy", "FP",     0.1,   200,   0.6,
    "A",        "Sick",    "TP",     0.8,   75,    0.85,
    "A",        "Sick",    "FN",     0.2,   120,   0.5
  )

  dt <- DecisionTree(dt_data)

  expect_s3_class(dt, "DecisionTree")
  expect_s3_class(dt, "Model")
  expect_equal(dt$data, dt_data)
  expect_true(is.na(dt$N))
})

test_that("calculate_pathways correctly evaluates tree probabilities and costs", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,   ~to,        ~prob, ~cost, ~eff,
    "A",        "root",  "Healthy",  0.7,   0,     0,
    "A",        "root",  "Sick",     0.3,   0,     0,
    "A",        "Healthy", "TN",     0.9,   50,    0.9,
    "A",        "Healthy", "FP",     0.1,   200,   0.6
  )

  dt <- DecisionTree(dt_data)
  paths <- modularce:::calculate_pathways(dt)

  expect_s3_class(paths, "data.frame")
  expect_equal(nrow(paths), 3) # Sick (terminal node), TN, FP
  
  tn_row <- paths[paths$terminal_node == "TN", ]
  expect_equal(tn_row$terminal_prob, 0.7 * 0.9)
  expect_equal(tn_row$path_cost, 50)
  expect_equal(tn_row$path_eff, 0.9)
})

test_that("run_model.DecisionTree calculates expected costs and effects", {
  dt_data <- tibble::tribble(
    ~treatment, ~from,     ~to,       ~prob, ~cost, ~eff,
    "A",        "root",    "Node1",   0.6,   100,   0.8,
    "A",        "root",    "Node2",   0.4,   200,   0.5
  )

  dt <- DecisionTree(dt_data)
  res <- run_model(dt)

  expect_s3_class(res, "output")
  expect_s3_class(res, "DecisionTree")
  
  expected_c <- 0.6 * 100 + 0.4 * 200 # 60 + 80 = 140
  expected_e <- 0.6 * 0.8 + 0.4 * 0.5 # 0.48 + 0.20 = 0.68

  expect_equal(get_costs(res), expected_c)
  expect_equal(get_effects(res), expected_e)
})
