

pacman::p_load(HMDHFDplus, tidyverse, broom)

# load e0 for scotland
dta_scot <- HMDHFDplus::readHMDweb("GBR_SCO", item = "E0per", username = userInput(), password = userInput())
dta_scot

# load mx for scotland
dta_scot_mx <- HMDHFDplus::readHMDweb("GBR_SCO", item = "Mx_1x1", username = userInput(), password = userInput())
HMDHFDplus::getHMDitemavail("GBR_SCO", username = userInput(), password = userInput())

# bring to tidy format
dta_scot_tidy <- 
  dta_scot %>% 
    as_tibble() %>% 
    rename_all(tolower) %>% 
    gather(-year, key = "sex", value = "e0")

# write e0 to directory
write_csv(dta_scot_tidy, "data/e0_scot.csv")

# visualise e0
dta_scot_tidy %>% 
  ggplot(aes(x = year, y = e0, group = sex, colour = sex)) + 
  geom_line()

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


  
# move mx to tidy format
mx_scot_tidy <- 
  dta_scot_mx %>% 
    as_tibble() %>% 
    rename_all(tolower) %>% 
    select(-openinterval) %>% 
    gather(-year, -age, key ="sex", value = "mx")

# write mx to directory  
write_csv(mx_scot_tidy, "data/mx_scot.csv")


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

set.seed(20)

# a 1% sample 


ten_k_runs %>% 
  filter(rep %in% sample(1:10000, 100)) %>% 
  ggplot(aes(x = k, y = change, group = rep)) +
  facet_wrap(~sex) + 
  geom_line(alpha = 0.2)

# to accumulate with 2011 observation as 0th value 

set.seed(20)

ten_k_runs %>% 
  left_join(dta_scot_tidy %>% filter(year == 2011) %>% select(-year)) %>% 
  group_by(sex, rep) %>% 
  arrange(k) %>% 
  mutate(
    cumulative_change = cumsum(change),
    prediction        = e0 + cumulative_change
  ) %>% 
  ungroup() %>% 
  filter(rep %in% sample(1:10000, 500)) %>% 
  mutate(year = k + 2011) %>% 
  left_join(
    dta_scot_tidy %>% rename(obs_e0 = e0)
  ) %>% 
  ggplot(aes(x = year, y = prediction, group = rep)) + 
  facet_wrap(~sex) + 
  geom_line(alpha = 0.025) + 
  geom_point(aes(y = obs_e0))



