
# Load packages, functions,  and data for UK as a whole 



pacman::p_load(
  tidyverse,
  HMDHFDplus
)


# Functions 
get_ll <- function(x, mu, sig_sq){
  sig <- sqrt(sig_sq)
  n <- length(x)
  
  - n * log(sig)  - (n/2) * log(2 * pi) - (1 / 2 * sig_sq) * sum((x - mu)^2)
}

calc_bayes_factors <- function(dta, before_period, after_period, outcome_var, perc_range = seq(from = 1, to = 0, by = -0.01)){
  before_dta <- dta %>% 
    filter(between(year, before_period[1], before_period[2]))
  
  if(length(after_period) == 1){
    after_dta <- dta %>% 
      filter(year >= after_period)
  } else if (length(after_period) == 2){
    after_dta <- dta %>% 
      filter(between(year, after_period[1], after_period[2]))
  }
  
  prev_params <- before_dta %>% 
    summarise(
      mu  = mean({{ outcome_var }}),
      sig = sd({{ outcome_var }}) 
    )
  
  ll_noslowdown <- get_ll(x = after_dta %>% pull({{ outcome_var }}), 
                          mu = pull(prev_params, "mu"), 
                          sig_sq = pull(prev_params, "sig") ^ 2
  )
  
  scenarios_df <- 
    tibble(
      perc = perc_range
    ) %>% 
    mutate(
      mu = pull(prev_params, "mu") * perc
    ) %>% 
    mutate(
      ll = map_dbl(mu, ~get_ll(mu = ., x = after_dta %>% pull({{ outcome_var }}), 
                               sig_sq = pull(prev_params, "sig") ^ 2))
    ) %>% 
    mutate(bayes_factor = exp(ll) /  exp(ll_noslowdown))
  
  
  
  return(scenarios_df)
}


