# Tests for Colquhoun Bayes Factor computation

library(testthat)

source("../../R/posterior.R")
source("../../R/colquhoun.R")

test_that("colquhoun_bf returns expected structure", {
  
  result <- colquhoun_bf(p = 0.05, n = 16, d = 1.0)
  
  expect_type(result, "list")
  expect_named(result, c("p_value", "n_per_group", "effect_size",
                         "t_statistic", "bayes_factor", "power", "method"))
  expect_equal(result$method, "colquhoun")
  
})

test_that("colquhoun_bf computes correct values for known inputs", {
  
  result <- colquhoun_bf(p = 0.05, n = 16, d = 1.0)
  
  # t-statistic for p=0.05, df=30
  expect_equal(result$t_statistic, 2.042272, tolerance = 0.0001)
  
  # Power should be ~78% for this design
  expect_equal(result$power, 0.7813978, tolerance = 0.001)
  
  # BF should be ~5.51
  expect_equal(result$bayes_factor, 5.512922, tolerance = 0.001)
  
})

test_that("colquhoun_bf gives low power for underpowered studies", {
  
  # Small sample, small effect = underpowered
  underpowered <- colquhoun_bf(p = 0.05, n = 10, d = 0.3)
  
  # Should have low power
  expect_lt(underpowered$power, 0.20)
  
  # BF should still be positive
  expect_gt(underpowered$bayes_factor, 0)
  
})

test_that("interpret_pvalue_colquhoun returns complete results", {
  
  result <- interpret_pvalue_colquhoun(p = 0.05, n = 16, d = 1.0, prior = 0.5)
  
  expect_type(result, "list")
  expect_equal(result$prior, 0.5)
  expect_equal(result$posterior + result$fpr, 1, tolerance = 0.0001)
  expect_equal(result$method, "colquhoun")
  
})

test_that("colquhoun_bf validates inputs", {
  
  expect_error(colquhoun_bf(p = -0.1, n = 16, d = 1.0))
  expect_error(colquhoun_bf(p = 0.05, n = 1, d = 1.0))
  expect_error(colquhoun_bf(p = 0.05, n = 16, d = -0.5))
  
})