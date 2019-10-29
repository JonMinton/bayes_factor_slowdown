

pacman::p_load(HMDHFDplus, tidyverse, broom)

# Single year life tables for Scotland available here:

# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/singleyearlifetablesuk1980to2018

# Downloaded to data folder (see singleyearlifetablesscland.xls)

# load e0 for scotland
# dta_scot <- HMDHFDplus::readHMDweb("GBR_SCO", item = "E0per", username = userInput(), password = userInput())
# dta_scot

# load mx for scotland
# dta_scot_mx <- HMDHFDplus::readHMDweb("GBR_SCO", item = "Mx_1x1", username = userInput(), password = userInput())
# HMDHFDplus::getHMDitemavail("GBR_SCO", username = userInput(), password = userInput())

# bring to tidy format
# dta_scot_tidy <- 
#   dta_scot %>% 
#     as_tibble() %>% 
#     rename_all(tolower) %>% 
#     gather(-year, key = "sex", value = "e0")

# write e0 to directory
# write_csv(dta_scot_tidy, "data/e0_scot.csv")

dta_scot_tidy <- read_csv("data/e0_scot.csv")
# manually adding last two years from table listed above
dta_scot_tidy <- 
  dta_scot_tidy %>% 
    bind_rows(
      tribble(
        ~year, ~sex, ~e0,
        2017, "male", 77.13,
        2017, "female", 81.16,
        2018, "male", 77.05,
        2018, "female", 81.01
      )
    )
# n.b. does not include total

# visualise e0
dta_scot_tidy %>% 
  ggplot(aes(x = year, y = e0, group = sex, colour = sex)) + 
  geom_line() + 
  labs(
    x = "Year", 
    y = "Life expectancy at birth", 
    title = "Life expectancy at birth by year in Scotland", 
    caption = "Source: HMD"
  )

ggsave("figures/scot_e0.png", height = 15, width = 25, dpi = 300, units = "cm")

dta_scot_tidy %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(delta_e0 = e0 - lag(e0)) %>%
  ungroup () %>% 
  ggplot(aes(x = year, y = delta_e0, group = sex, colour = sex, shape = sex)) + 
  geom_point() + geom_line() +
  labs(
    x = "Year", y = "Change in life expectancy from last year", 
    title = "Annual change in life expectancy in Scotland"
  ) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype = "dashed")

ggsave("figures/scot_e0_annual_change.png", height = 15, width= 25, dpi = 300, units = "cm")

# From this it appears clear that 1950+ should be the main point of comparison 

# Let's look at 1950 onwards 

dta_scot_tidy %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(delta_e0 = e0 - lag(e0)) %>%
  ungroup() %>% 
  filter(year >= 1950) %>% 
  ggplot(aes(x = year, y = delta_e0, group = sex, colour = sex, shape = sex)) + 
  geom_point() + geom_line() +
  labs(
    x = "Year", y = "Change in life expectancy from last year", 
    title = "Annual change in life expectancy in Scotland"
  ) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype = "dashed")

ggsave("figures/scot_e0_annual_change_since_1950.png", height = 15, width = 25, dpi = 300, units = "cm")

  
# # move mx to tidy format
# mx_scot_tidy <- 
#   dta_scot_mx %>% 
#     as_tibble() %>% 
#     rename_all(tolower) %>% 
#     select(-openinterval) %>% 
#     gather(-year, -age, key ="sex", value = "mx")
# 
# # write mx to directory  
# write_csv(mx_scot_tidy, "data/mx_scot.csv")


#### 

# Now to start building a RWD and projecting a few schedules 


# Let's take 1950-2011 as the pre period 
# and project up to 2018

e0_ch_summary <- 
  dta_scot_tidy %>% 
    group_by(sex) %>% 
    arrange(year) %>% 
    mutate(delta_e0 = e0 - lag(e0)) %>% 
    ungroup() %>% 
    filter(between(year, 1950, 2011)) %>% 
    group_by(sex) %>% 
    summarise(
      mean_de0 = mean(delta_e0),
      sd_de0   = sd(delta_e0)
    ) %>% 
    ungroup()

e0_ch_summary


# So now to take these values and take, then accumulate, seven draws (2012-2018) 

set.seed(20)
ten_k_runs <- 
  e0_ch_summary %>% 
    mutate(
      draws = map2(
        .x = mean_de0, .y = sd_de0, 
        .f = ~replicate(10000, rnorm(n = 7, mean = .x, sd = .y))
        )
      ) %>% 
  mutate(
    draw_df = map(
      draws,
      ~.x %>% 
        data.frame() %>% 
        mutate(k = 1:n()) %>% 
        gather(-k, key = "rep", value = "change") %>% 
        mutate(rep = str_remove(rep, "X") %>% 
                 as.numeric()
        )               
      )
  ) %>% 
  select(sex, draw_df) %>% 
  unnest()


# a 1% sample 


ten_k_runs %>% 
  filter(rep %in% sample(1:10000, 100)) %>% 
  ggplot(aes(x = k, y = change, group = rep)) +
  facet_wrap(~sex) + 
  geom_line(alpha = 0.2)

# to accumulate with 2011 observation as 0th value 

ten_k_all_accumulated <- 
  ten_k_runs %>% 
    left_join(dta_scot_tidy %>% filter(year == 2011) %>% select(-year)) %>% 
    group_by(sex, rep) %>% 
    arrange(k) %>% 
    mutate(
      cumulative_change = cumsum(change),
      prediction        = e0 + cumulative_change
    ) %>% 
    ungroup() %>% 
    mutate(year = k + 2011) %>% 
  left_join(
    dta_scot_tidy %>% rename(obs_e0 = e0)
  )

ten_k_all_qis <- 
  ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_e0 = mean(prediction),
    med_e0  = median(prediction),
    low_95   = quantile(prediction, 0.025),
    high_95  = quantile(prediction, 0.975),
    low_90   = quantile(prediction, 0.050),
    high_90  = quantile(prediction, 0.950)
  ) %>% 
  ungroup()

ten_k_2pc_accumulated <- 
  ten_k_all_accumulated %>% 
  filter(rep %in% sample(1:10000, 500)) 
  
ten_k_2pc_accumulated %>%  
  ggplot(aes(x = year, y = prediction, group = rep)) + 
  facet_wrap(~sex) + 
  geom_line(alpha = 0.025) + 
  geom_ribbon(aes(x = year, ymin = low_90, ymax = high_90),
              data = ten_k_all_qis,
              inherit.aes = FALSE,
              alpha = 0.2
  ) + 
  geom_ribbon(aes(x = year, ymin = low_95, ymax = high_95),
              data = ten_k_all_qis,
              inherit.aes = FALSE,
              alpha = 0.2
  ) + 
  geom_line(
    aes(x = year, y = med_e0),
    data = ten_k_all_qis,
    inherit.aes = FALSE,
    colour = "red", size = 1.2
  ) + 
  geom_line(
    aes(x = year, y = mean_e0),
    data = ten_k_all_qis,
    inherit.aes = FALSE,
    colour = "darkgreen", size = 1.2, linetype = "dashed"
  ) + 
  geom_point(aes(y = obs_e0), size = 2) +
  labs(
    x = "Year", y = "Predicted/observed life expectancy",
    title = "Life expectancy against 1950-2011 trend",
    subtitle = "Red: Median of predictions; Green: Mean of predictions\nShaded regions: 90% and 95% credible intervals",
    caption = "Source: HMD; ONS/NRS for 2017 and 2018 (male/female only); faint lines show 5% of the 10,000 predicted values"
  )
ggsave("figures/obs_vs_pred_e0_1950_2011.png", height = 15, width = 25, units = "cm", dpi = 300)

# Empirical quantiles - as a table

ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  spread(sex, empirical_p)

#as a figure
ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  ggplot(aes(x = year, y = empirical_p, group = sex, colour = sex, shape = sex)) + 
  geom_point() + geom_line() +
  scale_y_continuous(limits = c(0, 0.6)) +
  geom_hline(yintercept = 0.5) +
  labs(
    x = "Year", 
    y = "Observation as percentile of predicted long-term trend distribution",
    title = "Observed life expectancies against range of predictions based on 1950-2011 trend",
    subtitle = "Horizontal line: Average improvement rate"
  )

ggsave("figures/trend_observed_percentiles_1950_2011.png", height = 10, width = 12, units = "cm", dpi = 300)

#Save for later

obs_percentiles_1950_2011 <- 
  ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  ungroup() %>% 
  mutate(fit_period = "1950-2011")


# To do:
#   - Add 2017 and 2018 e0 estimates [ done - but not for total]
#   - redo with 1990-2011 as the prior period


# USING 1990-2011 as the trend period

e0_ch_summary <- 
  dta_scot_tidy %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(delta_e0 = e0 - lag(e0)) %>% 
  ungroup() %>% 
  filter(between(year, 1990, 2011)) %>% # NOTE THE CHANGE HERE
  group_by(sex) %>% 
  summarise(
    mean_de0 = mean(delta_e0),
    sd_de0   = sd(delta_e0)
  ) %>% 
  ungroup()

e0_ch_summary


# So now to take these values and take, then accumulate, seven draws (2012-2018) 

set.seed(20)
ten_k_runs <- 
  e0_ch_summary %>% 
  mutate(
    draws = map2(
      .x = mean_de0, .y = sd_de0, 
      .f = ~replicate(10000, rnorm(n = 7, mean = .x, sd = .y))
    )
  ) %>% 
  mutate(
    draw_df = map(
      draws,
      ~.x %>% 
        data.frame() %>% 
        mutate(k = 1:n()) %>% 
        gather(-k, key = "rep", value = "change") %>% 
        mutate(rep = str_remove(rep, "X") %>% 
                 as.numeric()
        )               
    )
  ) %>% 
  select(sex, draw_df) %>% 
  unnest()


# a 1% sample 


ten_k_runs %>% 
  filter(rep %in% sample(1:10000, 100)) %>% 
  ggplot(aes(x = k, y = change, group = rep)) +
  facet_wrap(~sex) + 
  geom_line(alpha = 0.2)

# to accumulate with 2011 observation as 0th value 

ten_k_all_accumulated <- 
  ten_k_runs %>% 
  left_join(dta_scot_tidy %>% filter(year == 2011) %>% select(-year)) %>% 
  group_by(sex, rep) %>% 
  arrange(k) %>% 
  mutate(
    cumulative_change = cumsum(change),
    prediction        = e0 + cumulative_change
  ) %>% 
  ungroup() %>% 
  mutate(year = k + 2011) %>% 
  left_join(
    dta_scot_tidy %>% rename(obs_e0 = e0)
  )

ten_k_all_qis <- 
  ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_e0 = mean(prediction),
    med_e0  = median(prediction),
    low_95   = quantile(prediction, 0.025),
    high_95  = quantile(prediction, 0.975),
    low_90   = quantile(prediction, 0.050),
    high_90  = quantile(prediction, 0.950)
  ) %>% 
  ungroup()

ten_k_2pc_accumulated <- 
  ten_k_all_accumulated %>% 
  filter(rep %in% sample(1:10000, 500)) 

ten_k_2pc_accumulated %>%  
  ggplot(aes(x = year, y = prediction, group = rep)) + 
  facet_wrap(~sex) + 
  geom_line(alpha = 0.025) + 
  geom_ribbon(aes(x = year, ymin = low_90, ymax = high_90),
              data = ten_k_all_qis,
              inherit.aes = FALSE,
              alpha = 0.2
  ) + 
  geom_ribbon(aes(x = year, ymin = low_95, ymax = high_95),
              data = ten_k_all_qis,
              inherit.aes = FALSE,
              alpha = 0.2
  ) + 
  geom_line(
    aes(x = year, y = med_e0),
    data = ten_k_all_qis,
    inherit.aes = FALSE,
    colour = "red", size = 1.2
  ) + 
  geom_line(
    aes(x = year, y = mean_e0),
    data = ten_k_all_qis,
    inherit.aes = FALSE,
    colour = "darkgreen", size = 1.2, linetype = "dashed"
  ) + 
  geom_point(aes(y = obs_e0), size = 2) +
  labs(
    x = "Year", y = "Predicted/observed life expectancy",
    title = "Life expectancy against 1990-2011 trend",
    subtitle = "Red: Median of predictions; Green: Mean of predictions\nShaded regions: 90% and 95% credible intervals",
    caption = "Source: HMD; ONS/NRS for 2017 and 2018 (male/female only); faint lines show 5% of the 10,000 predicted values"
  )

ggsave("figures/obs_vs_pred_e0_1990_2011.png", height = 15, width = 25, units = "cm", dpi = 300)

ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  spread(sex, empirical_p)

#as a figure
ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  ggplot(aes(x = year, y = empirical_p, group = sex, colour = sex, shape = sex)) + 
  geom_point() + geom_line() +
  scale_y_continuous(limits = c(0, 0.7)) +
  geom_hline(yintercept = 0.5) +
  labs(
    x = "Year", 
    y = "Observation as percentile of predicted long-term trend distribution",
    title = "Observed life expectancies against range of predictions based on 1990-2011 trend",
    subtitle = "Horizontal line: Average improvement rate"
  )

ggsave("figures/trend_observed_percentiles_1990_2011.png", height = 10, width = 12, units = "cm", dpi = 300)



obs_percentiles_1990_2011 <- 
  ten_k_all_accumulated %>% 
  group_by(sex, year) %>% 
  summarise(empirical_p = mean(prediction < obs_e0)) %>% 
  ungroup() %>% 
  mutate(fit_period = "1990-2011")

obs_percentiles_1950_2011 %>% 
  bind_rows(
    obs_percentiles_1990_2011
  ) %>% 
  ggplot(aes(x = year, y = empirical_p, group = sex, colour = sex, shape = sex)) + 
  facet_wrap(~fit_period) + 
  geom_point() + geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  labs(
    x = "Year", y = "Percentile",
    title = "Observed e0 against range of expectations, by trend fit period"
  )

ggsave("figures/trend_observed_percentiles_bothperiods.png", height = 15, width = 20, units = "cm", dpi = 300)



## Now to something similar using mx instead of e0



