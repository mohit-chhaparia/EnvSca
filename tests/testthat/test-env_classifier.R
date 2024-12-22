test_that("Test 1: Basic functionality with valid inputs", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  result <- env_classifier(yt, group, L = 3, yt_new, kappa = 0.5)
  expect_type(result, "integer")
  expect_length(result, 1)
})


test_that("Test 2: Validation for yt input", {
  set.seed(12092024)
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  expect_error(env_classifier(NULL, group, L = 3, yt_new, kappa = 0.5), "yt cannot be NULL")
  expect_error(env_classifier(1:10, group, L = 3, yt_new, kappa = 0.5), "yt should be a 3D array")
  expect_error(env_classifier(array(1:5, dim = c(1, 1, 1)), group, L = 3, yt_new, kappa = 0.5), "Minimum dimension of yt")
})


test_that("Test 3: Validation for group input", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  expect_error(env_classifier(yt, NULL, L = 3, yt_new, kappa = 0.5), "group cannot be null")
  expect_error(env_classifier(yt, c(1, 2), L = 3, yt_new, kappa = 0.5), "Number of elements in group")
})


test_that("Test 4: Validation for L input", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  expect_error(env_classifier(yt, group, NULL, yt_new, kappa = 0.5), "L cannot be NULL")
  expect_error(env_classifier(yt, group, 1.5, yt_new, kappa = 0.5), "All elements of L should be integers")
  expect_error(env_classifier(yt, group, 60, yt_new, kappa = 0.5), "All elements of L should be integers between 2 and")
})


test_that("Test 5: Validation for yt_new input", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  expect_error(env_classifier(yt, group, L = 3, NULL, kappa = 0.5), "yt_new cannot be NULL")
  expect_error(env_classifier(yt, group, L = 3, data.frame(1:10), kappa = 0.5), "yt_new should be a")
  expect_error(env_classifier(yt, group, L = 3, array(1:10, dim = c(10, 2, 1)), kappa = 0.5), "Dimension of each slice of yt_new")
})


test_that("Test 6: Validation for kappa input", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  expect_error(env_classifier(yt, group, L = 3, yt_new, NULL), "kappa cannot be NULL")
  expect_error(env_classifier(yt, group, L = 3, yt_new, c(0.5, 0.6)), "kappa should be a single numeric element")
  expect_error(env_classifier(yt, group, L = 3, yt_new, 1.5), "kappa should be a single element between 0 and 1")
})


test_that("Test 7: Plots are generated without errors when plot = TRUE", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))
  expect_silent(env_classifier(yt, group, L = 3, yt_new, kappa = 0.5, plot = TRUE))
})


test_that("Test 8: Handles smallest valid inputs", {
  set.seed(12092024)
  yt <- array(rnorm(72), dim = c(9, 2, 4))
  group <- rep(c(1, 2), 2)
  yt_new <- array(rnorm(18), dim = c(9, 2, 1))
  result <- env_classifier(yt, group, L = 2, yt_new, kappa = 0.5)
  expect_length(result, 1)
})


test_that("Test 9: Handles insufficient class representation", {
  set.seed(12092024)
  yt <- array(rnorm(108), dim = c(9, 2, 6))
  group <- c(1, 2, 1, 1, 1, 1)
  yt_new <- array(rnorm(18), dim = c(9, 2, 1))
  expect_error(env_classifier(yt, group, L = 2, yt_new, kappa = 0.5))
})


test_that("Test 10: Handles largest valid inputs", {
  set.seed(12092024)
  yt <- array(rnorm(10000), dim = c(100, 10, 10))
  group <- rep(1:2, each = 5)
  yt_new <- array(rnorm(1000), dim = c(100, 10, 1))

  warnings <- capture_warnings(env_classifier(yt, group, L = 5, yt_new, kappa = 0.8))

  expect_true(any(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt.", warnings)))
  expect_true(sum(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt_group.", warnings)) == 2)
  expect_true(sum(grepl("It is feasible to have L less than the cube root of the number of rows in yt", warnings)) == 12)

  result <- suppressWarnings(env_classifier(yt, group, L = 5, yt_new, kappa = 0.8))
  expect_length(result, 1)
})


test_that("Test 11: Handles yt_new as matrix", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50)
  result <- env_classifier(yt, group, L = 3, yt_new, kappa = 0.5)
  expect_length(result, 1)
  expect_type(result, "integer")
})


test_that("Test 12: Warning for suboptimal L values", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))

  warnings <- capture_warnings(env_classifier(yt, group, L = 24, yt_new, kappa = 0.5))

  expect_true(any(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt.", warnings)))
  expect_true(sum(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt_group.", warnings)) == 2)
  expect_true(sum(grepl("It is feasible to have L less than the cube root of the number of rows in yt", warnings)) == 12)

  expect_error(env_classifier(yt, group, L = 50, yt_new, kappa = 0.5), "All elements of L should be integers between 2 and half of number of rows in yt.")
})


