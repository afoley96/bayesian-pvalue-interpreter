# Tests for posterior probability computation

library(testthat)

source("../../R/sellke_berger.R")
source("../../R/posterior.R")

test_that("prior_to_posterior returns correct values", {
  
  # With BF = 2.46 (from p = 0.05) and prior = 0.5
  expect_equal(
    prior_to_posterior(0.5, 2.456023),
    0.7106501,
    tolerance = 0.0001
  )
  
  # With prior = 0.1
  expect_equal(
    prior_to_posterior(0.1, 2.456023),
    0.2143871,
    tolerance = 0.0001
  )
  
  # With prior = 0.9
  expect_equal(
    prior_to_posterior(0.9, 2.456023),
    0.9567178,
    tolerance = 0.0001
  )
  
})

test_that("prior_to_posterior handles extreme Bayes factors", {
  
  # Very strong evidence (BF = 100) with prior = 0.5
  expect_equal(
    prior_to_posterior(0.5, 100),
    100 / 101,
    tolerance = 0.0001
  )
  
  # Weak evidence (BF = 1) should not change prior
  expect_equal(
    prior_to_posterior(0.5, 1),
    0.5,
    tolerance = 0.0001
  )
  
})

test_that("interpret_pvalue returns complete results", {
  
  result <- interpret_pvalue(0.05, prior = 0.5)
  
  expect_type(result, "list")
  expect_named(result, c("p_value", "prior", "bayes_factor", "posterior", "fpr"))
  expect_equal(result$p_value, 0.05)
  expect_equal(result$prior, 0.5)
  expect_equal(result$posterior + result$fpr, 1)
  
})

test_that("interpret_pvalue uses default prior of 0.5", {
  
  result <- interpret_pvalue(0.05)
  expect_equal(result$prior, 0.5)
  
})