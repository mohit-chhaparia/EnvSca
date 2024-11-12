test_that("Test 1: cat_convert works for simple input", {
  xt <- c(1, 2, 1, 3, 2, 3)

  expect_equal(cat_convert(xt), matrix(c(1, 0, 1, 0, 0, 0,
                                         0, 1, 0, 0, 1, 0,
                                         0, 0, 0, 1, 0, 1),
                                       nrow = 6, byrow = FALSE
                                       )
               )
})
