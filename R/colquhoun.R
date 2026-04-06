#' Compute Bayes Factor using Colquhoun's Method
#'
#' Given an observed p-value, sample size, and effect size, computes
#' the likelihood ratio (Bayes factor) using the ratio of probability
#' densities under H1 vs H0.
#'
#' @param p Observed p-value
#' @param n Sample size per group (assumes two equal groups)
#' @param d Standardized effect size (Cohen's d)
#'
#' @return A list containing:
#'   - bf: Bayes factor (likelihood ratio for H1 vs H0)
#'   - power: Statistical power for this design
#'   - method: "colquhoun"
#'
#' @details
#' This method computes the likelihood ratio as the ratio of probability
#' densities of the t-distribution at the observed t-statistic:
#'
#'   LR = f(t | H1) / f(t | H0)
#'
#' Where H1 is represented by a non-central t-distribution with
#' non-centrality parameter delta = d * sqrt(n/2).
#'
#' Unlike the Sellke-Berger bound, this gives the exact likelihood ratio
#' for the specified alternative hypothesis, but requires knowing the
#' expected effect size.
#'
#' Reference: Colquhoun (2017). The reproducibility of research and the
#' misinterpretation of p-values. Royal Society Open Science.
#'
#' @examples
#' colquhoun_bf(p = 0.05, n = 16, d = 1.0)
#' colquhoun_bf(p = 0.05, n = 50, d = 0.5)
#'
#' @export
colquhoun_bf <- function(p, n, d) {
  
  # Input validation
  if (!is.numeric(p) || !is.numeric(n) || !is.numeric(d)) {
    stop("p, n, and d must be numeric")
  }
  
  if (any(p <= 0 | p >= 1)) {
    stop("p must be between 0 and 1 (exclusive)")
  }
  
  if (any(n < 2)) {
    stop("n must be at least 2")
  }
  
  if (any(d <= 0)) {
    stop("d must be positive")
  }
  
  # Degrees of freedom (two-sample t-test)
  df <- 2 * n - 2
  
  # Convert p-value to t-statistic (two-tailed)
  t_obs <- qt(1 - p / 2, df)
  
  # Non-centrality parameter under H1
  # For two-sample t-test: delta = d * sqrt(n/2)
  ncp <- d * sqrt(n / 2)
  
  # Likelihood ratio = density under H1 / density under H0
  # H0: central t-distribution
  # H1: non-central t-distribution with ncp = delta
  density_h0 <- dt(t_obs, df)
  density_h1 <- dt(t_obs, df, ncp = ncp)
  
  bf <- density_h1 / density_h0
  
  # Also compute power for reference
  t_crit <- qt(1 - 0.05 / 2, df)  # critical value at alpha = 0.05
  power <- 1 - pt(t_crit, df, ncp = ncp) + pt(-t_crit, df, ncp = ncp)
  
  return(list(
    p_value = p,
    n_per_group = n,
    effect_size = d,
    t_statistic = t_obs,
    bayes_factor = bf,
    power = power,
    method = "colquhoun"
  ))
}


#' Full Pipeline: P-value to Posterior using Colquhoun Method
#'
#' @param p Observed p-value
#' @param n Sample size per group
#' @param d Standardized effect size (Cohen's d)
#' @param prior Prior probability that H1 is true (default 0.5)
#'
#' @return A list containing all results
#'
#' @export
interpret_pvalue_colquhoun <- function(p, n, d, prior = 0.5) {
  
  # Get Colquhoun BF
  colq <- colquhoun_bf(p, n, d)
  
  # Load posterior function if needed
  if (!exists("prior_to_posterior")) {
    source("R/posterior.R")
  }
  
  posterior <- prior_to_posterior(prior, colq$bayes_factor)
  fpr <- 1 - posterior
  
  return(list(
    p_value = p,
    n_per_group = n,
    effect_size = d,
    prior = prior,
    t_statistic = colq$t_statistic,
    bayes_factor = colq$bayes_factor,
    power = colq$power,
    posterior = posterior,
    fpr = fpr,
    method = "colquhoun"
  ))
}