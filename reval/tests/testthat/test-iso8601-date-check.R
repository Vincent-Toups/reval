library(testthat);

test_that("parses_as_iso8601_date correctly validates ISO 8601 date strings", {
  # Test a valid date
  valid_date <- c("2023-07-03")
  expect_true(reval::parses_as_iso8601_date(valid_date))
  
  # Test multiple valid dates
  valid_dates <- c("2023-07-03", "2020-02-29", "2000-01-01")
  expect_true(all(reval::parses_as_iso8601_date(valid_dates)))
  
  # Test an invalid date (non-leap year February 29)
  invalid_date <- c("2023-02-29")
  expect_false(reval::parses_as_iso8601_date(invalid_date))
  
  # Test multiple invalid dates
  invalid_dates <- c("2023-02-29", "2020-02-30", "2000-13-01")
  expect_false(any(reval::parses_as_iso8601_date(invalid_dates)))
  
  # Test a syntactically incorrect date
  incorrect_syntax_date <- c("20230703")
  expect_false(reval::parses_as_iso8601_date(incorrect_syntax_date))
  
  # Test an empty string
  empty_string <- c("")
  expect_false(reval::parses_as_iso8601_date(empty_string))
})
