# Detection framework for regime change in longevity improvement.
#
# The question this project was originally built to answer: given a run of
# disappointing years, how much evidence does it take to reject the
# assumption that the long-run data-generating process (DGP) for annual life
# expectancy improvement is unchanged? This is the "1-in-1000-year storm
# recurring within a lifetime" intuition made quantitative, and it is the
# formal complement to the qualitative "slowing down or returning to normal?"
# question posed by Hiam et al. (2023, British Medical Bulletin).
#
# The null DGP is estimated from a calibration window (annual e0 changes,
# 1991-2010): Normal(mu0, sigma0^2). Three tools:
#   (A) storm_probability() - how surprising a single "bad" year is;
#   (B) years_to_detect()   - how many years of a sustained lower regime it
#       takes to reject "DGP unchanged" at a given level (a power/sample-size
#       calculation, closed form);
#   (C) first_detection_year() - retrospective early warning: walking forward
#       from the change point, the first year at which the accumulated
#       evidence rejects the null (how early the monitor would have fired).
#
# Depends on get_ll() from R/bayes_factors.R and slowdown_index() from
# R/monitoring_index.R.

#' (A) Surprise of a single "bad" year under the calibration DGP.
#'
#' @param mu0,sd0 Mean and SD of annual change in the calibration window.
#' @param threshold Annual change at or below which a year is "bad"
#'   (default 0: zero-or-negative improvement).
#' @return Tibble: p_year (P(change <= threshold | null)), odds_1_in
#'   (reciprocal), and years_iid_to_1_in_1000 (consecutive independent such
#'   years whose joint probability first falls below 1/1000).
storm_probability <- function(mu0, sd0, threshold = 0) {
  p <- stats::pnorm(threshold, mean = mu0, sd = sd0)
  tibble::tibble(
    p_year = p,
    odds_1_in = 1 / p,
    years_iid_to_1_in_1000 = ceiling(log(1 / 1000) / log(p))
  )
}

#' (B) Years of a sustained lower regime needed to detect the change.
#'
#' Under a sustained alternative in which the true mean annual change is
#' `k * mu0` (k = retained fraction of the old rate; k = 0 is a complete
#' stall) with unchanged SD, the mean of n annual changes has expected
#' one-sided z of (k - 1) * mu0 * sqrt(n) / sd0. Solving for rejection at
#' level `alpha` gives the closed form below. Negative year-to-year
#' autocorrelation (rho < 0, as observed for e0 changes) *reduces* the
#' variance of a multi-year mean, so it makes detection faster; the optional
#' `rho` argument applies the AR(1) effective-sample-size correction
#' n_eff = n * (1 - rho) / (1 + rho).
#'
#' @param mu0,sd0 Calibration mean and SD.
#' @param k Retained fraction of the old improvement rate (0 = full stall).
#' @param alpha One-sided significance level.
#' @param rho Optional lag-1 autocorrelation (default 0 = iid).
#' @return Integer number of years (ceiling), NA if k >= 1 (no change to detect).
years_to_detect <- function(mu0, sd0, k, alpha = 0.05, rho = 0) {
  if (k >= 1) return(NA_integer_)
  z <- stats::qnorm(1 - alpha)
  n_iid <- (z * sd0 / ((1 - k) * mu0))^2
  # AR(1) correction: n_eff = n * (1-rho)/(1+rho); invert to get raw years.
  n_years <- if (rho == 0) n_iid else n_iid * (1 + rho) / (1 - rho)
  as.integer(ceiling(n_years))
}

#' (C) Retrospective early warning: first year the monitor fires.
#'
#' Walks forward from `start_year`, and for each end-year computes the
#' likelihood-ratio test of "DGP unchanged" over the accumulated window
#' [start_year, end], returning the first end-year at which p < alpha (and
#' the p-value trajectory). Answers: how early would a monitor committed to
#' this test have flagged the slowdown?
#'
#' @param dta Data with `year` and the outcome column.
#' @param before_period c(first, last) calibration years.
#' @param outcome_var Unquoted annual-change column.
#' @param start_year First post-change year to accumulate from.
#' @param end_year Last available year.
#' @param alpha One-sided level.
#' @return list(first_year = <year or NA>, trajectory = tibble(end, n, p_value)).
first_detection_year <- function(dta, before_period, outcome_var, start_year,
                                 end_year, alpha = 0.05) {
  ends <- start_year:end_year
  traj <- purrr::map_dfr(ends, function(e) {
    idx <- slowdown_index(dta, before_period, c(start_year, e), {{ outcome_var }})
    tibble::tibble(end = e, n = idx$n_after, slowdown_pct = idx$slowdown_pct,
                   p_value = idx$p_value)
  })
  hit <- traj %>% dplyr::filter(p_value < alpha)
  list(
    first_year = if (nrow(hit)) min(hit$end) else NA_integer_,
    trajectory = traj
  )
}
