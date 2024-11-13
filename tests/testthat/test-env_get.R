test_that("Test 1: env_get works for simple input", {

  yt <- matrix(rnorm(20), nrow = 5, ncol = 4)
  L <- 2

  result <- env_get(yt, L)

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale"))

  expect_type(result$freq, "double")
  expect_type(result$envelope, "double")
  expect_type(result$scale, "double")

  expect_length(result$freq, 2)
  expect_length(result$envelope, 2)
  expect_equal(dim(result$scale), c(2, 4))
})
