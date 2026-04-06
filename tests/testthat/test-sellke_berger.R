# Tests for Sellke-Berger Bayes Factor computation

library(testthat)

# Load the function we're testing
source("../../R/sellke_berger.R")

test_that("sellke_berger_bf returns correct values for known p-values", {
  
  # Values verified against Sellke et al. (2001) and Colquhoun (2017)
  expect_equal(sellke_berger_bf(0.05), 2.456023, tolerance = 0.0001)
  expect_equal(sellke_berger_bf(0.01), 7.988401, tolerance = 0.0001)
  expect_equal(sellke_berger_bf(0.001), 53.256, tolerance = 0.001)
  
})

test_that("sellke_berger_bf handles edge cases", {
  
  # At p = 1/e (~0.368), BF should be 1
  expect_equal(sellke_berger_bf(1/exp(1)), 1, tolerance = 0.0001)
  
  # For p > 1/e, BF should be 1
  expect_equal(sellke_berger_bf(0.5), 1)
  
  # p = 1 should return 1
  expect_equal(sellke_berger_bf(1), 1)
  
})

test_that("sellke_berger_bf handles vectors", {
  
  p_vals <- c(0.05, 0.01, 0.001)
  result <- sellke_berger_bf(p_vals)
  
  expect_length(result, 3)
  expect_equal(result[1], 2.456023, tolerance = 0.0001)
  
})

test_that("sellke_berger_bf validates input", {
  
  # Should error on invalid inputs
  expect_error(sellke_berger_bf(-0.1))
  expect_error(sellke_berger_bf(0))
  expect_error(sellke_berger_bf(1.5))
  expect_error(sellke_berger_bf("not a number"))
  
})