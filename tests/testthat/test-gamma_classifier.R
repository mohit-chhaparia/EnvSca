test_that("Test 1: gamma_classifier handles valid inputs and outputs correctly", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(600), dim = c(50, 3, 4))

  classes <- gamma_classifier(yt, group, L = 3, yt_new)

  expect_equal(length(classes), dim(yt_new)[3])
  expect_true(all(classes %in% unique(group)))
})


test_that("Test 2: gamma_classifier handles single test time-series array correctly", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(150), dim = c(50, 3, 1))

  classes <- gamma_classifier(yt, group, L = 3, yt_new)

  expect_equal(length(classes), 1)
  expect_true(classes %in% unique(group))
})


test_that("Test 3: gamma_classifier handles test time-series as a matrix", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- matrix(rnorm(150), nrow = 50, ncol = 3)

  classes <- gamma_classifier(yt, group, L = 3, yt_new)

  expect_equal(length(classes), 1)
  expect_true(classes %in% unique(group))
})


test_that("Test 4: gamma_classifier handles invalid inputs with appropriate error messages", {
  expect_error(gamma_classifier(NULL, c(1, 2), 3, array(1, c(50, 3, 1))), "yt cannot be NULL")
  expect_error(gamma_classifier(matrix(1, 50, 3), c(1, 2), 3, array(1, c(50, 3, 1))), "yt should be a 3D array")

  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))

  expect_error(gamma_classifier(yt, c(1, 2), 3, array(1, c(50, 3, 1))), "Number of elements in group should be equal to the number of slices in yt")
  expect_error(gamma_classifier(yt, c(rep(1, 5), rep(2, 5)), NULL, array(1, c(50, 3, 1))), "L cannot be NULL")
  expect_error(gamma_classifier(yt, c(rep(1, 5), rep(2, 5)), 3, NULL), "yt_new cannot be NULL")
})


test_that("Test 5: gamma_classifier handles edge cases for L parameter", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(600), dim = c(50, 3, 4))

  expect_error(gamma_classifier(yt, group, L = 1, yt_new), "All elements of L should be integers between 2 and half of number of rows in yt")
  expect_error(gamma_classifier(yt, group, L = 30, yt_new), "All elements of L should be integers between 2 and half of number of rows in yt")
  expect_error(gamma_classifier(yt, group, L = 3.5, yt_new), "All elements of L should be integers between 2 and half of number of rows in yt")
})


test_that("Test 6: gamma_classifier issues warnings for suboptimal L values", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(600), dim = c(50, 3, 4))

  warnings <- capture_warnings(gamma_classifier(yt, group, L = 4, yt_new))
  expect_true(any(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt.", warnings)))
  expect_true(sum(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt_group.", warnings)) == 2)
  expect_true(sum(grepl("It is feasible to have L less than the cube root of the number of rows in yt", warnings)) == 14)
})


test_that("Test 7: gamma_classifier handles the plot functionality", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  yt_new <- array(rnorm(600), dim = c(50, 3, 4))

  expect_silent(gamma_classifier(yt, group, L = 3, yt_new, plot = TRUE))
})


test_that("Test 8: gamma_classifier handles large datasets efficiently", {
  set.seed(12092024)
  yt <- array(rnorm(1e5), dim = c(100, 10, 100))
  group <- rep(1:10, each = 10)
  yt_new <- array(rnorm(3e4), dim = c(100, 10, 30))

  warnings <- capture_warnings(gamma_classifier(yt, group, L = 5, yt_new))

  expect_true(any(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt.", warnings)))
  expect_true(sum(grepl("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt_group.", warnings)) == 10)
  expect_true(sum(grepl("It is feasible to have L less than the cube root of the number of rows in yt", warnings)) == 130)

  classes <- suppressWarnings(gamma_classifier(yt, group, L = 5, yt_new))

  expect_equal(length(classes), dim(yt_new)[3])
})

