test_that("Test 1: env_get works for simple input", {
  yt <- matrix(rnorm(20000), nrow = 500, ncol = 40)
  L <- c(3, 7, 11)

  expect_warning(env_get(yt, L))
  result <- suppressWarnings(env_get(yt, L))

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_type(result$freq, "double")
  expect_type(result$envelope, "double")
  expect_type(result$scale, "double")

  expect_length(result$freq, 250)
  expect_length(result$envelope, 250)
  expect_equal(dim(result$scale), c(250, 40))
})


test_that("Test 2: env_get handles single observation", {
  yt <- matrix(rnorm(4), nrow = 1, ncol = 4)
  L <- 2

  result <- expect_error(env_get(yt, L))

  # freq, envelope, and scale are empty or NULL
  expect_length(result$freq, 0)
  expect_length(result$envelope, 0)
  expect_equal(dim(result$scale), NULL)
})


test_that("Test 3: env_get works with small matrix", {
  yt <- matrix(rnorm(15), nrow = 5, ncol = 3)
  L <- 2

  expect_warning(env_get(yt, L))
  result <- suppressWarnings(env_get(yt, L))

  expect_length(result$freq, 2)
  expect_length(result$envelope, 2)
  expect_equal(dim(result$scale), c(2, 3))
})


test_that("Test 4: env_get handles univariate time series", {
  yt <- matrix(rnorm(500), nrow = 500, ncol = 1)
  L <- 3

  result <- env_get(yt, L)

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_type(result$freq, "double")
  expect_type(result$envelope, "double")
  expect_type(result$scale, "double")

  expect_length(result$freq, 250)
  expect_length(result$envelope, 250)
  expect_equal(dim(result$scale), c(250, 1))
})


test_that("Test 5: env_get handles maximum feasible L", {
  yt <- matrix(rnorm(200), nrow = 50, ncol = 4)
  L <- floor(nrow(yt) / 2) - 1

  expect_warning(env_get(yt, L))
  result <- suppressWarnings(env_get(yt, L))

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_length(result$freq, floor(nrow(yt) / 2))
  expect_length(result$envelope, floor(nrow(yt) / 2))
  expect_equal(dim(result$scale), c(floor(nrow(yt) / 2), ncol(yt)))
})


test_that("Test 6: env_get fails for invalid L inputs", {
  yt <- matrix(rnorm(100), nrow = 25, ncol = 4)

  expect_error(env_get(yt, NULL), "L cannot be NULL.")
  expect_error(env_get(yt, -1), "L should be a integer between 1 and half of the number of rows in yt.")
  expect_error(env_get(yt, nrow(yt)), "L should be a integer between 1 and half of the number of rows in yt.")
  expect_error(env_get(yt, 0), "L should be a integer between 1 and half of the number of rows in yt.")
})


test_that("Test 7: env_get fails for invalid yt inputs", {
  expect_error(env_get(NULL, 3), "yt cannot be NULL")
  expect_error(env_get(data.frame(rnorm(100)), 3), "yt must be a matrix")
  expect_error(env_get(matrix("a", nrow = 10, ncol = 2), 3), "All elements of yt must be numeric.")
  expect_error(env_get(matrix(rnorm(200), nrow = 1), 3), "yt must have atleast 2 rows and 1 column.")
  expect_error(env_get(matrix(rnorm(2e4), nrow = 100, ncol = 1e4), 3), "yt has too many columns. Please reduce the dimensionality for computational feasibility.")
})


test_that("Test 8: env_get works with large matrix inputs", {
  yt <- matrix(rnorm(1e6), nrow = 5000, ncol = 200)
  L <- 5

  result <- env_get(yt, L)

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_length(result$freq, floor(nrow(yt) / 2))
  expect_length(result$envelope, floor(nrow(yt) / 2))
  expect_equal(dim(result$scale), c(floor(nrow(yt) / 2), ncol(yt)))
})


test_that("Test 9: env_get produces consistent results with fixed seed", {
  set.seed(12092024)
  yt <- matrix(rnorm(500), nrow = 50, ncol = 10)
  L <- 3

  result1 <- env_get(yt, L)

  set.seed(12092024)
  result2 <- env_get(yt, L)

  expect_equal(result1, result2)
})

