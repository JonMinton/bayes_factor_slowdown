
# To do

# 1. write ll function [done]
# 2. get data from hmd 
# 3. get differences for uk 
# 4. calc ll for different extents of slowdown 

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

# getHMDcountries()

dta_uk <- HMDHFDplus::readHMDweb("GBR_NP", item = "E0per", username = userInput(), password = userInput())

# Now 201&
# link here: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables/2017singleyearlifetables.xls

# Saved to data 
# The only items needed are 
# e0 male   : 77.13
# e0 female : 81.16

# Population from here: 

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool


# For total, need male and female population in 2017

# According to wolfram alpha:
# https://www.wolframalpha.com/input/?i=UK+population+2017+by+sex
# male: 31.9 million
# female: 32.82 million

e0_total <- (79.23 * 31.9 + 82.98 * 32.82) / (31.9 + 32.82)


dta_tidy <- dta_uk %>% 
  as_tibble() %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  gather(-year, key = "sex", value = "e0")

dta_tidy <- 
  dta_tidy %>% 
  bind_rows(
    tribble(
      ~year, ~sex, ~e0,
      2017, "female", 82.98,
      2017, "male"  , 79.23, 
      2017, "total" , e0_total 
    )
  )


# by gender

dta_tidy %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  nest() %>% 
  crossing(after_end = 2013:2017) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(2013, .x), before_period = c(1990, 2012), outcome_var = ch_e0, dta = .y)
    )
  ) %>%
  select(sex, after_end, bayes_df) %>% 
  unnest() %>% 
  ggplot(aes(x = perc, y = bayes_factor, group = after_end, colour = factor(after_end))) + 
  geom_line() +
  facet_wrap(~sex) + 
  geom_hline(yintercept = 1)


# So, the evidence is still at the 'anecdotal' level, but growing

# USA - since 1999? 

dta_usa <- HMDHFDplus::readHMDweb("USA", item = "E0per", username = userInput(), password = userInput()) 

dta_usa %>% 
  as_tibble() %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  gather(-year, key = "sex", value = "e0") %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  nest() %>% 
  crossing(after_end = 1999:2017) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(1999, .x), before_period = c(1960, 1998), 
                                           outcome_var = ch_e0, dta = .y)
    )
  ) %>%
  select(sex, after_end, bayes_df) %>% 
  unnest() %>% 
  ggplot(aes(x = perc, y = bayes_factor, group = after_end, colour = after_end)) + 
  geom_line() +
  facet_wrap(~sex) + 
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(1, 1.002))

# Now England/Wales alone 

dta_enw <- HMDHFDplus::readHMDweb("GBRTENW", item = "E0per", username = userInput(), password = userInput()) 

# e0 male: 79.53
# e0 female: 83.19

# population in 2017

# male: 29021253 
# female: 29723342 

pop_male_2017   <- 29021253
pop_female_2017 <- 29723342
e0_enw_total    <- (79.53 * pop_male_2017 + 83.19 * pop_female_2017 ) / (pop_male_2017 + pop_female_2017)

dta_enw %>% 
  as_tibble() %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  gather(-year, key = "sex", value = "e0") %>% 
  bind_rows(
    tribble(
      ~year, ~sex, ~e0,
      2017, "male", 79.53,
      2017, "female", 83.19,
      2017, "total", e0_enw_total
    )
  ) %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  nest() %>% 
  crossing(after_end = 2013:2017) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(2013, .x), before_period = c(1990, 2012), 
                                           outcome_var = ch_e0, dta = .y)
    )
  ) %>%
  select(sex, after_end, bayes_df) %>% 
  unnest() %>% 
  ggplot(aes(x = perc, y = bayes_factor, group = after_end, colour = factor(after_end))) + 
  geom_line() +
  facet_wrap(~sex) + 
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(1, 1.005))

dta_sco <- HMDHFDplus::readHMDweb("GBR_SCO", item = "E0per", username = userInput(), password = userInput()) 

dta_sco %>% 
  as_tibble() %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  gather(-year, key = "sex", value = "e0") %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  nest() %>% 
  crossing(after_end = 2012:2017) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(2012, .x), before_period = c(1990, 2011), dta = .y)
    )
  ) %>%
  select(sex, after_end, bayes_df) %>% 
  unnest() %>% 
  ggplot(aes(x = perc, y = bayes_factor, group = after_end, colour = after_end)) + 
  geom_line() +
  facet_wrap(~sex) + 
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(1, 1.005))


# Now to look at same by age in single years 


dta_mx <- HMDHFDplus::readHMDweb(
  "GBR_NP", item = "Mx_1x1", 
  username = userInput(), password = userInput() 
  ) %>% 
  as_tibble() %>% 
  filter(!OpenInterval) %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  select(-openinterval) %>% 
  gather(key = "sex", value = "mx", -year, - age) 


dta_mx %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))   %>% 
  filter(between(year, 1990, 2012)) %>% 
  ggplot(aes(x = year, y = ch_lnmx, group = age, colour = age)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  scale_colour_distiller(palette = "Paired") 
  
# Maybe change max age to 90

dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))   %>% 
  filter(between(year, 1990, 2012)) %>% 
  ggplot(aes(x = year, y = ch_lnmx, group = age, colour = age)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  scale_colour_distiller(palette = "Paired")  + 
  geom_hline(yintercept = 0)

# So, with the exception of very old males, the highest variances are in childhood (few events)
# and the general tendency is negative 

# And if absolute rather than relative (logged) used? 

dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ch_mx = mx - lag(mx))   %>% 
  filter(between(year, 1990, 2012)) %>% 
  ggplot(aes(x = year, y = ch_mx, group = age, colour = age)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  scale_colour_distiller(palette = "Paired")  + 
  geom_hline(yintercept = 0)

# This doesn't work as well. 

# So use logged 
# How about recent years? 

dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))   %>% 
  filter(between(year, 1990, 2017)) %>% 
  ggplot(aes(x = year, y = ch_lnmx, group = age, colour = age)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  scale_colour_distiller(palette = "Paired")  + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 2012) +
  stat_smooth(se = F, linetype = "dashed", colour = "black", size = 1.2, mapping = aes(x = year, y = ch_lnmx), inherit.aes = FALSE)

# So something like a general uptick looks apparent here. 

# Now since 1960? 


dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))   %>% 
  filter(between(year, 1960, 2017)) %>% 
  ggplot(aes(x = year, y = ch_lnmx, group = age, colour = age)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  scale_colour_distiller(palette = "Paired")  + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 2012) +
  stat_smooth(se = F, linetype = "dashed", colour = "black", size = 1.2, mapping = aes(x = year, y = ch_lnmx), inherit.aes = FALSE)

# So over this longer term perspective it looks like the average % rates of improvement 
# have been slowly increasingly somewhat, mainly driven by male improvment rates. However, 
# the slowdown in recent years for both genders is apparent. 


# n.b. it would be interesting to see this as a 3D surface, but let's keep on track! 


# Bayes Factors by age groups 

bf_mx_uk <- dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))  %>% 
  nest() %>% 
  mutate(after_end = 2016) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(2012, .x), before_period = c(1990, 2011), 
                                           outcome_var = ch_lnmx, 
                                           dta = .y)
    )
  ) %>%
  select(age, sex, after_end, bayes_df)

bf_mx_uk %>% 
  filter(after_end == 2016) %>% 
  unnest() %>% 
  ggplot(aes(x = age, y = perc, fill = bayes_factor)) + 
  geom_tile() + 
  facet_wrap(~sex)


bf_mx_uk %>% 
  unnest() %>% 
  group_by(sex, age) %>% 
  mutate(scaled_factor = (bayes_factor - min(bayes_factor))/ (max(bayes_factor) - min(bayes_factor))) %>% 
  ungroup() %>% 
  ggplot(aes(x = age, y = perc, fill = scaled_factor)) + 
  geom_tile() + 
  facet_wrap(~sex)


  

bf_mx_uk_55 <- dta_mx %>% 
  filter(age <= 90) %>% 
  group_by(age, sex) %>% 
  arrange(year) %>%
  mutate(ln_mx = log(mx)) %>% 
  mutate(ch_lnmx = ln_mx - lag(ln_mx))  %>% 
  nest() %>% 
  mutate(after_end = 2016) %>% 
  mutate(
    bayes_df = map2(
      after_end, data, ~calc_bayes_factors(after_period = c(2012, .x), before_period = c(1955, 2011), 
                                           outcome_var = ch_lnmx, 
                                           dta = .y)
    )
  ) %>%
  select(age, sex, after_end, bayes_df)


bf_mx_uk_55 %>% 
  unnest() %>% 
  group_by(sex, age) %>% 
  mutate(scaled_factor = (bayes_factor - min(bayes_factor))/ (max(bayes_factor) - min(bayes_factor))) %>% 
  ungroup() %>% 
  ggplot(aes(x = age, y = perc, fill = scaled_factor)) + 
  geom_tile() + 
  facet_wrap(~sex)

