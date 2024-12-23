test_that("Test 1: Basic Functionality", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  kappa <- seq(0, 1, length.out = 10)
  L <- 3

  result <- env_classifier_crossv(yt, group, L, kappa, plot = FALSE)

  expect_equal(length(result), length(kappa))
  expect_true(all(result >= 0 & result <= dim(yt)[3]))
})


test_that("Test 2: Input Validation: yt", {
  expect_error(env_classifier_crossv(NULL, c(1, 1), 3, c(0.1), FALSE), "yt cannot be NULL")
  expect_error(env_classifier_crossv(matrix(1:10, nrow = 2), c(1, 1), 3, c(0.1), FALSE), "yt should be a 3D array")

  yt <- array(c("a", "b", "c"), dim = c(2, 1, 2))

  expect_error(env_classifier_crossv(yt, c(1, 1), 3, c(0.1), FALSE), "All elements of yt must be numeric")
})


test_that("Test 3: Input Validation: group", {
  set.seed(12092024)
  yt <- array(rnorm(100), dim = c(10, 2, 5))

  expect_error(env_classifier_crossv(yt, NULL, 3, c(0.1), FALSE), "group cannot be null")
  expect_error(env_classifier_crossv(yt, c("a", "b"), 3, c(0.1), FALSE), "All elements of group should be numeric")
  expect_error(env_classifier_crossv(yt, c(1, 1, 1), 3, c(0.1), FALSE), "Number of elements in group should be equal")
})


test_that("Test 4: Input Validation: L", {
  set.seed(12092024)
  yt <- array(rnorm(100), dim = c(10, 2, 5))

  expect_error(env_classifier_crossv(yt, c(1, 2, 1, 2, 1), NULL, c(0.1), FALSE), "L cannot be NULL")
  expect_error(env_classifier_crossv(yt, c(1, 2, 1, 2, 1), "a", c(0.1), FALSE), "All elements of L should be integers between 2 and half of number of rows in yt.")
  expect_error(env_classifier_crossv(yt, c(1, 2, 1, 2, 1), 20, c(0.1), FALSE), "All elements of L should be integers between 2 and half of number of rows in yt.")
})


test_that("Test 5: Input Validation: kappa", {
  set.seed(12092024)
  yt <- array(rnorm(100), dim = c(10, 2, 5))

  expect_error(suppressWarnings(env_classifier_crossv(yt, c(1, 2, 1, 2, 1), 3, NULL, FALSE)), "kappa cannot be NULL")
  expect_error(suppressWarnings(env_classifier_crossv(yt, c(1, 2, 1, 2, 1), 3, c(-0.1), FALSE)), "Elements of kappa should be between 0 and 1")
})


test_that("Test 6: Output Validation", {
  set.seed(12092024)
  yt <- array(rnorm(1500), dim = c(50, 3, 10))
  group <- c(rep(1, 5), rep(2, 5))
  kappa <- seq(0, 1, length.out = 10)
  L <- 3

  result <- env_classifier_crossv(yt, group, L, kappa, plot = FALSE)

  expect_equal(length(result), length(kappa))
  expect_true(is.numeric(result))
})




