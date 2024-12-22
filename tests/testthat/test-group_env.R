test_that("Test 1: group_env throws an error for NULL yt_group", {
  expect_error(group_env(NULL, L = 3), "yt_group cannot be NULL.")
})

test_that("Test 2: group_env throws an error for invalid yt_group dimensions", {
  yt_group <- matrix(rnorm(100), nrow = 10, ncol = 10)
  expect_error(group_env(yt_group, L = 3), "yt_group should be a 3D array where each slice represents a time-series.")
})

test_that("Test 3: group_env throws an error for non-numeric yt_group", {
  yt_group <- array(rep("a", 300), dim = c(10, 3, 10))
  expect_error(group_env(yt_group, L = 3), "All elements of yt_group must be numeric.")
})

test_that("Test 4: group_env throws an error for invalid L values", {
  set.seed(12092024)
  yt_group <- array(rnorm(3000), dim = c(100, 3, 10))

  expect_error(group_env(yt_group, L = NULL), "L cannot be NULL.")
  expect_error(group_env(yt_group, L = "3"), "All elements of L should be integers")
  expect_error(group_env(yt_group, L = c(1, 50)), "All elements of L should be integers between 2 and half of number of rows in yt_group.")
})

test_that("Test 5: group_env throws an error for invalid plot argument", {
  set.seed(12092024)
  yt_group <- array(rnorm(3000), dim = c(100, 3, 10))

  expect_error(group_env(yt_group, L = 3, plot = "TRUE"), "plot should be a single logical value.")
  expect_error(group_env(yt_group, L = 3, plot = c(TRUE, FALSE)), "plot should contain a single logical value.")
})

test_that("Test 6: group_env returns expected components", {
  set.seed(12092024)
  yt_group <- array(rnorm(3000), dim = c(100, 3, 10))

  result <- group_env(yt_group, L = 3, plot = FALSE)

  expect_type(result, "list")
  expect_named(result, c("freq", "envelope", "scale", "envelope_ind", "scale_ind"))

  expect_type(result$freq, "double")
  expect_type(result$envelope, "double")
  expect_type(result$scale, "double")
  expect_type(result$envelope_ind, "list")
  expect_type(result$scale_ind, "list")
})

test_that("Test 7: group_env processes large but valid inputs", {
  set.seed(12092024)
  yt_group <- array(rnorm(5000), dim = c(100, 5, 10))

  result <- group_env(yt_group, L = 3, plot = FALSE)

  expect_true(length(result$freq) > 0)
  expect_true(length(result$envelope) > 0)
})

test_that("Test 8: group_env generates warnings for L exceeding cube root of rows", {
  set.seed(12092024)
  yt_group <- array(rnorm(3000), dim = c(100, 3, 10))

  warnings <- capture_warnings(group_env(yt_group, L = 5))
  expect_true(any(grepl("It is feasible if all elements of L are integers", warnings)))
  expect_true(sum(grepl("It is feasible to have L less than the cube root of the number of rows", warnings)) == 10)
})

test_that("Test 9: group_env generates plots when plot = TRUE", {
  set.seed(12092024)
  yt_group <- array(rnorm(3000), dim = c(100, 3, 10))

  expect_silent(group_env(yt_group, L = 3, plot = TRUE))
})
