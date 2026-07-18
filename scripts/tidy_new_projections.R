# Tidy the 2020-, 2022- and 2024-based NPP expectation-of-life releases
# (downloaded to data/ons_projections/<geography>/proj_20{20,22,24}.xlsx)
# and combine them with the previously tidied 2012-2018 rounds.
#
# Outputs:
# - data/ons_projections/all_tidied_2012_2024.rds : all rounds 2012-2024,
#   same structure as all_tidied.rds (country, sex, proj_year, age, year, ex)
# - data/e0_from_ons_allnations_extended.csv : the actual e0 series
#   (e0_from_ons_allnations.csv, from single-year life tables, ending 2018)
#   extended to 2024 using the historic estimates embedded in the
#   2024-based NPP files, with a source column.
#
# Also prints a QA comparison of the two actual-e0 sources over their
# overlap years.

pacman::p_load(tidyverse, readxl, here)
source(here("R", "parse_ons_projections.R"))

geographies <- tribble(
  ~dir,               ~country_old, ~population,
  "united_kingdom",   "UK",         "United Kingdom",
  "england",          "England",    "England",
  "wales",            "Wales",      "Wales",
  "scotland",         "Scotland",   "Scotland",
  "northern_ireland", "Northern Ireland", "Northern Ireland"
)

new_rounds <- crossing(geographies, proj_year = c(2020, 2022, 2024)) %>%
  mutate(
    path = map2_chr(dir, proj_year,
                    ~here("data", "ons_projections", .x, str_glue("proj_{.y}.xlsx"))),
    data = map(path, read_npp_period_ex)
  ) %>%
  select(-path, -dir)

new_tidied <- new_rounds %>%
  select(country = country_old, proj_year, data) %>%
  unnest(cols = data) %>%
  mutate(sex = case_when(sex == "male" ~ "m", sex == "female" ~ "f")) %>%
  select(country, sex, proj_year, age, year, ex)

# match the country coding used in all_tidied.rds
old_tidied <- read_rds(here("data", "ons_projections", "all_tidied.rds"))
stopifnot(setequal(unique(new_tidied$country), unique(old_tidied$country)))

bind_rows(old_tidied, new_tidied) %>%
  write_rds(here("data", "ons_projections", "all_tidied_2012_2024.rds"))

# ---- Extend the actual e0 series -----------------------------------------

e0_old <- read_csv(here("data", "e0_from_ons_allnations.csv"), show_col_types = FALSE)

e0_npp_hist <- new_rounds %>%
  filter(proj_year == 2024) %>%
  select(population, data) %>%
  unnest(cols = data) %>%
  filter(age == 0, year <= 2024) %>%
  select(population, year, sex, e0 = ex)

# QA: how close are the two sources where they overlap?
overlap_qa <- e0_old %>%
  inner_join(e0_npp_hist, by = c("population", "year", "sex"),
             suffix = c("_lifetable", "_npp")) %>%
  mutate(diff = e0_npp - e0_lifetable)

overlap_qa %>%
  group_by(population, sex) %>%
  summarise(
    n = n(),
    mean_diff = mean(diff),
    max_abs_diff = max(abs(diff)),
    .groups = "drop"
  ) %>%
  print(n = Inf)

e0_extended <- bind_rows(
  e0_old %>% mutate(source = "single_year_lifetables"),
  e0_npp_hist %>%
    filter(year > max(e0_old$year)) %>%
    mutate(source = "npp_2024_based_historic")
) %>%
  arrange(population, sex, year)

write_csv(e0_extended, here("data", "e0_from_ons_allnations_extended.csv"))

e0_extended %>%
  group_by(population, source) %>%
  summarise(years = str_glue("{min(year)}-{max(year)}"), .groups = "drop") %>%
  print(n = Inf)
