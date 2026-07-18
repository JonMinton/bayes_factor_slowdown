# Bayes factor machinery for slowdown scenarios.
#
# Corrected and documented replacement for the get_ll / calc_bayes_factors
# pair in scripts/load_packages_and_functions.R. The legacy get_ll contained
# an operator-precedence error - `(1 / 2 * sig_sq)` where the normal
# log-likelihood requires `1 / (2 * sig_sq)` - which attenuated all Bayes
# factors toward 1 by a factor of sigma^4 on the log scale (see
# audit/get_ll_fix_impact.qmd). The legacy version is preserved here, only
# for that audit, as get_ll_legacy.

#' Normal log-likelihood of a sample.
#'
#' @param x Numeric vector of observations.
#' @param mu Mean of the normal model.
#' @param sig_sq Variance of the normal model.
#' @return Total log-likelihood (scalar).
get_ll <- function(x, mu, sig_sq) {
  sum(dnorm(x, mean = mu, sd = sqrt(sig_sq), log = TRUE))
}

#' Legacy (buggy) normal log-likelihood, kept only for the audit report.
#' The final term multiplies by sig_sq/2 instead of dividing by 2*sig_sq.
get_ll_legacy <- function(x, mu, sig_sq) {
  sig <- sqrt(sig_sq)
  n <- length(x)
  -n * log(sig) - (n / 2) * log(2 * pi) - (1 / 2 * sig_sq) * sum((x - mu)^2)
}

#' Bayes factors for proportional-slowdown scenarios.
#'
#' Calibrates a Normal(mu, sigma^2) model of annual e0 change on
#' `before_period`, then for each scenario mean mu * perc (same sigma)
#' computes the Bayes factor (likelihood ratio) against the no-slowdown
#' null (perc = 1) over the observations in `after_period`.
#'
#' @param dta Data frame with a `year` column and the outcome column.
#' @param before_period c(first, last) year of the calibration window.
#' @param after_period Single year (start, open-ended) or c(first, last).
#' @param outcome_var Unquoted outcome column (annual change in e0).
#' @param perc_range Scenario means as proportions of the calibration mean.
#' @param ll_fun Log-likelihood function (get_ll, or get_ll_legacy for the
#'   audit).
#' @return Tibble: perc, mu, ll, log_bf, bayes_factor. log_bf (natural log)
#'   is returned alongside bayes_factor because corrected BFs can overflow
#'   exp() for long observation windows.
calc_bayes_factors <- function(dta, before_period, after_period, outcome_var,
                               perc_range = seq(from = 1, to = 0, by = -0.01),
                               ll_fun = get_ll) {
  before_dta <- dta %>%
    dplyr::filter(dplyr::between(year, before_period[1], before_period[2]))

  if (length(after_period) == 1) {
    after_dta <- dta %>% dplyr::filter(year >= after_period)
  } else {
    after_dta <- dta %>%
      dplyr::filter(dplyr::between(year, after_period[1], after_period[2]))
  }

  x_after <- after_dta %>% dplyr::pull({{ outcome_var }})
  x_before <- before_dta %>% dplyr::pull({{ outcome_var }})
  mu_hat <- mean(x_before)
  sig_sq_hat <- stats::var(x_before)

  ll_null <- ll_fun(x = x_after, mu = mu_hat, sig_sq = sig_sq_hat)

  tibble::tibble(perc = perc_range) %>%
    dplyr::mutate(
      mu = mu_hat * perc,
      ll = purrr::map_dbl(mu, ~ll_fun(x = x_after, mu = .x, sig_sq = sig_sq_hat)),
      log_bf = ll - ll_null,
      bayes_factor = exp(log_bf)
    )
}
