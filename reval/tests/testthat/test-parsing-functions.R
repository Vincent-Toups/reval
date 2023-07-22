library(testthat);
library(reval);

test_that("reval:::parses_as_int correctly identifies integers", {
  expect_equal(reval:::parses_as_int("123"), TRUE)
  expect_equal(reval:::parses_as_int("-123"), TRUE)
  expect_equal(reval:::parses_as_int("123.45"), FALSE)
  expect_equal(reval:::parses_as_int("abc"), FALSE)
})

test_that("reval:::parses_as_formatted_int correctly identifies formatted integers", {
  expect_equal(reval:::parses_as_formatted_int("123", "3"), TRUE)
  expect_equal(reval:::parses_as_formatted_int("1234", "3"), FALSE)
  expect_equal(reval:::parses_as_formatted_int("12", "3"), TRUE)
  expect_equal(reval:::parses_as_formatted_int("-123", "3"), TRUE)
})

test_that("reval:::parses_as_float correctly identifies floats", {
  expect_equal(reval:::parses_as_float("123.45"), TRUE)
  expect_equal(reval:::parses_as_float("-123.45"), TRUE)
  expect_equal(reval:::parses_as_float("123"), TRUE)
  expect_equal(reval:::parses_as_float("abc"), FALSE)
})

test_that("reval:::parses_as_formatted_float correctly identifies formatted floats", {
  expect_equal(reval:::parses_as_formatted_float("123.45", "3.2"), TRUE)
  expect_equal(reval:::parses_as_formatted_float("-123.45", "3.2"), TRUE)
  expect_equal(reval:::parses_as_formatted_float("123", "3.2"), TRUE)
  expect_equal(reval:::parses_as_formatted_float("123.456", "3.2"), FALSE)
  expect_equal(reval:::parses_as_formatted_float("1234.45", "3.2"), FALSE)
  expect_equal(reval:::parses_as_formatted_float("abc", "3.2"), FALSE)
})
