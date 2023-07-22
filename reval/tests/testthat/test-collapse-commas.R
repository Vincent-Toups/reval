library(testthat);
test_that("reval:::collapse_commas correctly collapses character vectors", {
  expect_equal(reval:::collapse_commas(c("apple", "banana", "cherry")), "apple, banana, cherry")
  expect_equal(reval:::collapse_commas(c("1", "2", "3")), "1, 2, 3")
  expect_equal(reval:::collapse_commas(c()), "")
  expect_equal(reval:::collapse_commas(c("single")), "single")
})
