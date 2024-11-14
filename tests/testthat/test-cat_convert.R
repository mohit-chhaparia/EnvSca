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
  expect_equal(cat_convert(xt), matrix(c(1, 0, 0, 1, 0, 0,
                                         0, 1, 0, 0, 0, 0,
                                         0, 0, 0, 0, 1, 0),
                                       nrow = 6, byrow = FALSE
                                       )
               )
})

test_that("Test 6: cat_convert works with xt containing different data types including NAs.", {
  xt <- c(1, 2, 3, 2, 1, 3, 'a', 'b', NA, 'a', 'a', NA)
  expect_equal(cat_convert(xt), matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
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
  expect_equal(dim(cat_convert(xt)), c(length(xt), length(unique(xt)) - 1))
})

