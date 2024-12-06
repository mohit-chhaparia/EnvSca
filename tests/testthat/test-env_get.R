test_that("Test 1: env_get works for simple input", {

  yt <- matrix(rnorm(20), nrow = 5, ncol = 4)
  L <- 2

  expect_warning(env_get(yt, L))
  result <- suppressWarnings(env_get(yt, L))

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_type(result$freq, "double")
  expect_type(result$envelope, "double")
  expect_type(result$scale, "double")

  expect_length(result$freq, 2)
  expect_length(result$envelope, 2)
  expect_equal(dim(result$scale), c(2, 4))
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
