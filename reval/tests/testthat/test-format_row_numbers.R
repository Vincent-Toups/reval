library(testthat)

test_that("format_row_numbers works as expected", {
  expect_equal(reval:::format_row_numbers(c(1, 2, 3)), "1, 2, 3")
  expect_equal(reval:::format_row_numbers(integer(0)), NA)
  expect_equal(reval:::format_row_numbers(c(1:15)), "1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ...")
})
