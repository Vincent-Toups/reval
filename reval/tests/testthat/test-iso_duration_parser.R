library(testthat)
library(reval)  

test_that("check_ISO8601_durations_cpp correctly validates durations", {
  expect_equal(check_ISO8601_durations_cpp(c("P1Y", "P0.5Y", "PT36H")), c(TRUE, TRUE, TRUE))
  expect_equal(check_ISO8601_durations_cpp(c("P1.5Y2M", "P1Y0.5M", "P1Y", "PT0.5H")), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("check_ISO8601_durations_cpp handles NA values correctly", {
  expect_equal(check_ISO8601_durations_cpp(c("P1Y", NA, "PT36H")), c(TRUE, NA, TRUE))
})

test_that("check_ISO8601_durations_cpp handles empty strings and invalid values correctly", {
  expect_equal(check_ISO8601_durations_cpp(c("", "INVALID", "P1Y2M")), c(FALSE, FALSE, TRUE))
})
