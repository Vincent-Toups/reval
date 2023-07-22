library(testthat);

test_that("find_name_pairs identifies correct pairs", {
  # Expected output: pairs of names that differ only by the last character "C" or "N"
  expected_output <- list(c("var1C", "var1N"), c("var2C", "var2N"))
  
  # Case 1: Exactly matching names
  result <- reval:::find_name_pairs(c("var1C", "var1N", "var2C", "var2N", "var3"))
  expect_equal(result, expected_output)
  
  # Case 2: Some names not in the correct format
  result <- reval:::find_name_pairs(c("var1C", "var1N", "var2C", "var2N", "var3", "var4X"))
  expect_equal(result, expected_output)
  
  # Case 3: Some names missing one of the pair
  result <- reval:::find_name_pairs(c("var1C", "var1N", "var2C", "var3N", "var4X"))
  expect_equal(result, list(c("var1C", "var1N")))
  
  # Case 4: No matching names
  result <- reval:::find_name_pairs(c("var1C", "var2N", "var3C", "var4N"))
  expect_equal(result, list())
})
