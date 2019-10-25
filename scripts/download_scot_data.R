

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
    mutate(draws = map2(.x = mean_de0, .y = sd_de0, .f = ~rerun(10000, rnorm(n = 7, mean = .x, sd = .y))))


