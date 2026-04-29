test_that("Test 1: Basic classification works for a test time-series as a matrix", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50)

  classes <- beta_classifier(yt, group, L = 3, yt_new, plot = FALSE)

  expect_type(classes, "integer")
  expect_length(classes, 1)
  expect_true(classes[1] %in% unique(group))
})


test_that("Test 2: Classification works for test time-series as a array", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(300), dim = c(50, 3, 2))

  classes <- beta_classifier(yt, group, L = 3, yt_new, plot = FALSE)

  expect_length(classes, 2)
  expect_true(all(classes %in% group))
})


test_that("Test 3: Function runs with plot = TRUE without errors", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(300), dim = c(50, 3, 2))

  expect_silent(beta_classifier(yt, group, L = 3, yt_new, plot = TRUE))
})


test_that("Test 4: Throws error for invalid dimensions of training data yt", {
  set.seed(12092024)
  yt <- array(rnorm(100), dim = c(50, 3))

  expect_error(beta_classifier(yt, group = c(1, 1, 2), L = 3, yt_new = matrix(rnorm(50), nrow = 50)))
})


test_that("Test 5: Throws error if group length mismatches the number of slices in yt", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group_invalid <- c(1, 2, 3)

  expect_error(beta_classifier(yt, group_invalid, L = 3, yt_new = matrix(rnorm(150), nrow = 50)))
})


test_that("Test 6: Throws error for invalid values of L", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))

  expect_error(beta_classifier(yt, group, L = 1, yt_new = matrix(rnorm(150), nrow = 50)))
  expect_error(beta_classifier(yt, group, L = 30, yt_new = matrix(rnorm(150), nrow = 50)))
  expect_error(beta_classifier(yt, group, L = c(3.5, 4.7), yt_new = matrix(rnorm(150), nrow = 50)))
})


test_that("Test 7: Throws error for invalid dimensions of test data yt_new", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(300), dim = c(50, 4, 2))

  expect_error(beta_classifier(yt, group, L = 3, yt_new))
})


test_that("Test 8: Handles single training time-series correctly", {
  set.seed(12092024)
  yt <- array(rnorm(150), dim = c(50, 3, 1))
  group <- c(1)
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))

  expect_error(beta_classifier(yt, group, L = 3, yt_new, plot = FALSE))
})


test_that("Test 9: Handles single test time-series as a slice correctly", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))

  classes <- beta_classifier(yt, group, L = 3, yt_new, plot = FALSE)

  expect_length(classes, 1)
  expect_true(classes[1] %in% group)
})


test_that("Test 10: Function handles large input data efficiently", {
  set.seed(12092024)
  yt <- array(rnorm(50000), dim = c(50, 10, 100))
  group <- rep(1:5, each = 20)
  yt_new <- array(rnorm(500), dim = c(50, 10, 1))

  expect_silent(classes <- beta_classifier(yt, group, L = 3, yt_new, plot = FALSE))
  expect_length(classes, 1)
  expect_true(classes[1] %in% group)
})


test_that("Test 11: Throws error for non-numeric yt", {
  yt <- array(rep("a", 1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50)

  expect_error(beta_classifier(yt, group, L = 3, yt_new), "All elements of yt must be numeric.")
})


test_that("Test 12: Throws error when yt has too many columns", {
  set.seed(04292026)
  yt <- array(rnorm(2010), dim = c(2, 1001, 1))
  group <- c(1)
  yt_new <- matrix(rnorm(2), nrow = 2)

  expect_error(beta_classifier(yt, group, L = 1, yt_new), 
               "yt is too large. Reduce the number of columns or slices for computational feasibility.")
})


test_that("Test 13: Throws error for non-numeric group", {
  set.seed(04292026)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep("a", 5), rep("b", 5))
  yt_new <- matrix(rnorm(150), nrow = 50)

  expect_error(beta_classifier(yt, group, L = 3, yt_new), "All elements of group should be numeric")
})


test_that("Test 14: Throws error for invalid plot argument", {
  set.seed(04292026)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50)

  expect_error(beta_classifier(yt, group, L = 3, yt_new, plot = "TRUE"),
               "plot should be a single logical value.")
  expect_error(beta_classifier(yt, group, L = 3, yt_new, plot = c(TRUE, FALSE)),
               "plot should contain a single logical value.")
})


test_that("Test 15: Throws error for non-numeric yt_new", {
  set.seed(04292026)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rep("a", 150), nrow = 50)

  expect_error(beta_classifier(yt, group, L = 3, yt_new), "All elements of yt_new must be numeric.")
})


test_that("Test 16: Works with L as a valid integer vector", {
  set.seed(04292026)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50)

  classes <- suppressWarnings(beta_classifier(yt, group, L = c(3, 5), yt_new, plot = FALSE))

  expect_type(classes, "integer")
  expect_length(classes, 1)
  expect_true(classes[1] %in% unique(group))
})


test_that("Test 17: Works correctly with three or more classes", {
  set.seed(04292026)
  yt <- array(rnorm(2250), dim = c(50, 3, 15))
  group <- c(rep(1, 5), rep(2, 5), rep(3, 5))
  yt_new <- array(rnorm(300), dim = c(50, 3, 2))

  classes <- beta_classifier(yt, group, L = 3, yt_new, plot = FALSE)

  expect_length(classes, 2)
  expect_true(all(classes %in% unique(group)))
})

