#' Convert Prior Probability and Bayes Factor to Posterior Probability
#'
#' Given a prior probability that an effect is real and a Bayes factor,
#' computes the posterior probability using Bayes' theorem.
#'
#' @param prior Prior probability that H1 is true (between 0 and 1)
#' @param bf Bayes factor (evidence for H1 vs H0)
#'
#' @return Posterior probability that H1 is true
#'
#' @details
#' Uses the formula:
#'   posterior = (bf * prior) / (bf * prior + (1 - prior))
#'
#' This is Bayes' theorem applied to hypothesis testing:
#'   P(H1|data) = P(data|H1) * P(H1) / P(data)
#'
#' Where the Bayes factor is P(data|H1) / P(data|H0).
#'
#' @examples
#' prior_to_posterior(0.5, 2.46)   # ~0.71 (p=0.05, prior=50%)
#' prior_to_posterior(0.1, 2.46)   # ~0.21 (p=0.05, prior=10%)
#' prior_to_posterior(0.9, 2.46)   # ~0.96 (p=0.05, prior=90%)
#'
#' @export
prior_to_posterior <- function(prior, bf) {
  
  
  # Input validation
  if (!is.numeric(prior) || !is.numeric(bf)) {
    stop("prior and bf must be numeric")
  }
  
  if (any(prior <= 0 | prior >= 1)) {
    stop("prior must be between 0 and 1 (exclusive)")
  }
  
  if (any(bf <= 0)) {
    stop("bf must be positive")
  }
  
  # Bayes' theorem
  posterior <- (bf * prior) / (bf * prior + (1 - prior))
  
  return(posterior)
}


#' Compute False Positive Risk from Posterior Probability
#'
#' @param posterior Posterior probability that H1 is true
#'
#' @return False positive risk (probability result is a false positive)
#'
#' @export
posterior_to_fpr <- function(posterior) {
  return(1 - posterior)
}


#' Full Pipeline: P-value to Posterior Probability
#'
#' Convenience function that takes a p-value and prior,
#' computes the Sellke-Berger Bayes factor, and returns
#' the posterior probability.
#'
#' @param p Observed p-value
#' @param prior Prior probability that H1 is true (default 0.5)
#'
#' @return A list containing:
#'   - bf: Minimum Bayes factor
#'   - posterior: Posterior probability H1 is true
#'   - fpr: False positive risk
#'
#' @examples
#' interpret_pvalue(0.05)
#' interpret_pvalue(0.05, prior = 0.3)
#' interpret_pvalue(c(0.05, 0.01, 0.001))
#'
#' @export
interpret_pvalue <- function(p, prior = 0.5) {
  
  # Source the sellke_berger function if not already loaded
  # (In a package, this wouldn't be necessary)
  if (!exists("sellke_berger_bf")) {
    source("R/sellke_berger.R")
  }
  
  bf <- sellke_berger_bf(p)
  posterior <- prior_to_posterior(prior, bf)
  fpr <- posterior_to_fpr(posterior)
  
  return(list(
    p_value = p,
    prior = prior,
    bayes_factor = bf,
    posterior = posterior,
    fpr = fpr
  ))
}