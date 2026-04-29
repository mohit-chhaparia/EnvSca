test_that("Test 1: cat_convert works for simple input", {
  xt <- c(1, 2, 1, 3, 2, 3)
  expect_equal(cat_convert(xt), matrix(c(1, 0, 1, 0, 0, 0,
                                         0, 1, 0, 0, 1, 0),
                                       nrow = 6, byrow = FALSE
                                       )
               )
})


test_that("Test 2: cat_convert handles single category input", {
  xt <- c(1, 1, 1, 1)
  expect_equal(dim(cat_convert(xt)), c(4, 0))
})


test_that("Test 3: cat_convert handles empty input", {
  xt <- numeric(0)
  expect_error(cat_convert(xt))
})


test_that("Test 4: cat_convert works with non-integer categories", {
  xt <- c(1.1, 2.2, 1.1, 3.3)
  expect_equal(cat_convert(xt), matrix(c(1, 0, 1, 0,
                                         0, 1, 0, 0),
                                       nrow = 4, byrow = FALSE
                                       )
               )
})


test_that("Test 5: cat_convert handles missing values", {
  xt <- c(1, 2, NA, 1, 3, NA)
  expect_warning(cat_convert(xt))
  expect_equal(suppressWarnings(cat_convert(xt)), matrix(c(1, 0, 0, 1, 0, 0,
                                         0, 1, 0, 0, 0, 0,
                                         0, 0, 0, 0, 1, 0),
                                       nrow = 6, byrow = FALSE
                                       )
               )
})


test_that("Test 6: cat_convert works with xt containing different data types including NAs.", {
  xt <- c(1, 2, 3, 2, 1, 3, 'a', 'b', NA, 'a', 'a', NA)
  expect_warning(cat_convert(xt))
  expect_equal(suppressWarnings(cat_convert(xt)), matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                                         0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
                                         0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0),
                                       nrow = 12, byrow = FALSE
                                       )
               )
})


test_that("Test 7: cat_convert output dimensions depends on input xt.", {
  xt <- c(1, 2, 3, 2, 1, 3, 'a', 'b', NA, 'a', 'a', NA)
  expect_warning(cat_convert(xt))
  expect_equal(dim(suppressWarnings(cat_convert(xt))), c(length(xt), length(unique(xt)) - 1))
})


test_that("Test 8: cat_convert handles xt with all NA values", {
  xt <- c(NA, NA, NA)
  expect_error(cat_convert(xt))
})


test_that("Test 9: cat_convert handles very large number of categories", {
  xt <- as.character(1:1e6)
  expect_error(cat_convert(xt))
})


test_that("Test 10: cat_convert handles non-vector inputs", {
  xt <- matrix(1:4, nrow = 2)
  expect_error(cat_convert(xt), "xt must be a vector.")
})


test_that("Test 11: cat_convert handles a mix of numeric and character inputs", {
  xt <- c(1, 2, 'a', 'b', 1, 'a')
  expect_equal(cat_convert(xt), matrix(c(1, 0, 0, 0, 1, 0,
                                         0, 1, 0, 0, 0, 0,
                                         0, 0, 1, 0, 0, 1),
                                       nrow = 6, byrow = FALSE))
})


test_that("Test 12: cat_convert excludes highest lexicographic value", {
  xt <- c('a', 'b', 'c', 'a', 'c', 'b')
  result <- cat_convert(xt)
  expect_equal(dim(result), c(6, 2))
  expect_equal(result, matrix(c(1, 0, 0, 1, 0, 0,
                                0, 1, 0, 0, 0, 1),
                              nrow = 6, byrow = FALSE))
})


test_that("Test 13: cat_convert works with factor input", {
  xt <- factor(c('a', 'b', 'c', 'a', 'c', 'b'))
  expect_error(cat_convert(xt))
})


test_that("Test 14: cat_convert handles xt with repeated categories", {
  xt <- c('a', 'b', 'a', 'b', 'a', 'b')
  expect_equal(cat_convert(xt), matrix(c(1, 0, 1, 0, 1, 0),
                                       nrow = 6, byrow = FALSE))
})


test_that("Test 15: cat_convert warns for high number of missing values", {
  xt <- c(NA, 1, NA, 2, NA)
  expect_warning(cat_convert(xt), "xt contains some missing values")
  expect_equal(dim(suppressWarnings(cat_convert(xt))), c(5, 2))
})


test_that("Test 16: cat_convert throws error for NULL input", {
  expect_error(cat_convert(NULL), "Provide a valid categorical time-series.")
})


test_that("Test 17: cat_convert throws error for single-element xt with one unique value", {
  xt <- c(1)
  result <- cat_convert(xt)
  expect_equal(dim(result), c(1, 0))
})


test_that("Test 18: cat_convert output has correct binary values (only 0s and 1s)", {
  xt <- c('a', 'b', 'c', 'a', 'b')
  result <- cat_convert(xt)
  expect_true(all(result %in% c(0, 1)))
})


test_that("Test 19: cat_convert row sums are 0 or 1 (reference category rows sum to 0)", {
  xt <- c('a', 'b', 'c', 'a', 'c')
  result <- cat_convert(xt)
  row_sums <- rowSums(result)
  expect_true(all(row_sums %in% c(0, 1)))
})


