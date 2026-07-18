# Monitoring-index framing of the slowdown analysis.
#
# Peer review established that the "Bayes factor" reported elsewhere in this
# repository is a maximised (profile) likelihood ratio over 101 slowdown
# scenarios, not a Bayes factor: there is no prior and no integration. Its
# maximising scenario has the closed form
#
#     slowdown_prop* = mean(post-period change) / mean(calibration change),
#
# so the estimator is a two-sample comparison of mean annual e0 change. This
# file provides that honest framing: a point estimate of the improvement-rate
# ratio, and a likelihood-ratio test of "some slowdown" vs "no slowdown"
# with a proper p-value, alongside the profile-likelihood-ratio value for
# continuity with the earlier analysis.
#
# Depends on get_ll() from R/bayes_factors.R.

#' Slowdown monitoring index for one population/sex series.
#'
#' Compares mean annual e0 change in an observation window against a
#' calibration window. Reports the improvement-rate ratio (and the implied
#' slowdown percentage), and tests H0: post-period mean = calibration mean
#' against H1: post-period mean free, via a likelihood-ratio test with the
#' calibration variance treated as known (chi-square, 1 df). The profile
#' likelihood ratio (the quantity previously mislabelled a Bayes factor) is
#' returned as `plr` for continuity.
#'
#' @param dta Data frame with `year` and the outcome column.
#' @param before_period c(first, last) calibration years.
#' @param after_period c(first, last) (or single start year) observation window.
#' @param outcome_var Unquoted annual-change column.
#' @return One-row tibble: n_before, n_after, mean_before, mean_after,
#'   rate_ratio, slowdown_pct, lr_stat, df, p_value, plr, log_plr.
slowdown_index <- function(dta, before_period, after_period, outcome_var) {
  before_dta <- dta %>%
    dplyr::filter(dplyr::between(year, before_period[1], before_period[2]))
  if (length(after_period) == 1) {
    after_dta <- dta %>% dplyr::filter(year >= after_period)
  } else {
    after_dta <- dta %>%
      dplyr::filter(dplyr::between(year, after_period[1], after_period[2]))
  }

  x_before <- before_dta %>% dplyr::pull({{ outcome_var }})
  x_after  <- after_dta  %>% dplyr::pull({{ outcome_var }})
  mu0     <- mean(x_before)
  sig_sq  <- stats::var(x_before)
  mu_hat  <- mean(x_after)

  # LR test (variance known from calibration): 2*(logL(mu_hat) - logL(mu0)),
  # ~ chi-square(1) under H0. The profile LR equals the earlier "Bayes
  # factor" maximised over the slowdown grid (clipped to [0,1]*mu0).
  ll_free <- get_ll(x_after, mu = mu_hat, sig_sq = sig_sq)
  ll_null <- get_ll(x_after, mu = mu0,    sig_sq = sig_sq)
  lr_stat <- 2 * (ll_free - ll_null)
  p_value <- stats::pchisq(lr_stat, df = 1, lower.tail = FALSE)

  # Slowdown scenario mean is constrained to [0, mu0]; the maximising
  # proportion is the clipped mean ratio, matching calc_bayes_factors().
  ratio      <- mu_hat / mu0
  ratio_clip <- min(max(ratio, 0), 1)
  ll_best    <- get_ll(x_after, mu = ratio_clip * mu0, sig_sq = sig_sq)
  log_plr    <- ll_best - ll_null

  tibble::tibble(
    n_before = length(x_before), n_after = length(x_after),
    mean_before = mu0, mean_after = mu_hat,
    rate_ratio = ratio, slowdown_pct = 100 * (1 - ratio_clip),
    lr_stat = lr_stat, df = 1, p_value = p_value,
    plr = exp(log_plr), log_plr = log_plr
  )
}
