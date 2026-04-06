#' Compute Minimum Bayes Factor (Sellke-Berger Bound)
#'
#' Given an observed p-value, computes the minimum Bayes factor
#' (maximum evidence for H1 over H0) using the Sellke-Berger bound.
#'
#' @param p Observed p-value (must be between 0 and 1)
#'
#' @return Minimum Bayes factor (evidence for H1 vs H0)
#'
#' @details
#' For p < 1/e (approximately 0.368), the bound is:
#'   BF_min = 1 / (-e * p * log(p))
#'
#' For p >= 1/e, the bound is 1 (no evidence either way).
#'
#' Reference: Sellke, Bayarri, & Berger (2001). Calibration of p-values
#' for testing precise null hypotheses. The American Statistician, 55(1), 62-71.
#'
#' @examples
#' sellke_berger_bf(0.05)  # approximately 2.46
#' sellke_berger_bf(0.01)  # approximately 7.93
#' sellke_berger_bf(0.001) # approximately 53.15
#'
#' @export
sellke_berger_bf <- function(p) {
  
  
  
  # Input validation
  if (!is.numeric(p)) {
    stop("p must be numeric")
  }
  
  if (any(p <= 0 | p > 1)) {
    stop("p must be between 0 (exclusive) and 1 (inclusive)")
  }
  
  # Sellke-Berger bound
  # For p >= 1/e, the minimum Bayes factor is 1 (no evidence)
  # For p < 1/e, use the formula
  
  e <- exp(1)
  threshold <- 1 / e
  
  # Vectorized computation
  bf <- ifelse(
    p >= threshold,
    1,
    1 / (-e * p * log(p))
  )
  
  return(bf)
}