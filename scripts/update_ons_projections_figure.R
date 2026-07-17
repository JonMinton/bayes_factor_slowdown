# Update of bayes_paper/figures/M02_ons_lt_projns.png (originally built in
# markdown/bayes_factor_ons_e0.Rmd), extending the ONS life expectancy
# projections figure with the 2020-, 2022- and 2024-based NPP rounds and
# actual e0 estimates through 2024.
#
# Sources:
# - data/ons_projections/historic_comparison.xls : ONS NPP accuracy report
#   underlying data (ad-hoc 006372), 1971-2012 projection rounds + actuals.
# - data/ons_projections/all_tidied.rds : tidied 2012-2018 based rounds
#   (built in bayes_factor_ons_e0.Rmd).
# - data/ons_projections/united_kingdom/proj_20{20,22,24}.xlsx : ONS
#   "Expectation of life, principal projection, UK" (period ex sheets).
#   Historic years in the 2024-based file (<= 2024) are used to extend the
#   "Actual" series beyond the accuracy report. NPP years are mid-year based.

pacman::p_load(tidyverse, readxl, here)

# 1. Historic rounds and actuals from the accuracy report -----------------

read_accuracy_half <- function(range, sex_label) {
  read_excel(
    path = here("data", "ons_projections", "historic_comparison.xls"),
    sheet = "Data", range = range
  ) %>%
    slice(-1) %>%
    select(-1) %>%
    rename(projection = ...2) %>%
    gather(-projection, key = "year", value = "e0") %>%
    filter(!is.na(e0)) %>%
    mutate(sex = sex_label, year = as.numeric(year)) %>%
    select(sex, projection, year, e0)
}

older_projns <- bind_rows(
  read_accuracy_half("A2:BO26",  "male"),
  read_accuracy_half("A30:BO54", "female")
)

# 2. 2014-2018 based rounds from the previously tidied projections --------

ons_projections_tidied <- read_rds(here("data", "ons_projections", "all_tidied.rds"))

mid_projns <- ons_projections_tidied %>%
  filter(country == "UK", age == 0, year >= 2010) %>%
  filter(proj_year != 2012) %>%  # the accuracy report already has the 2012 round
  mutate(sex = case_when(sex == "m" ~ "male", sex == "f" ~ "female")) %>%
  transmute(sex, projection = paste(proj_year, "proj"), year, e0 = ex)

# 3. 2020-2024 based rounds from the new xlsx releases --------------------

read_new_proj_sheet <- function(path, sheet, sex_label) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE,
                    .name_repair = "unique_quiet")
  # header row label varies between releases ("age", "Exact age (years)"),
  # so find the row that is mostly four-digit years
  n_years_in_row <- function(i) {
    sum(!is.na(suppressWarnings(
      as.numeric(str_extract(as.character(raw[i, ]), "^[0-9]{4}"))
    )))
  }
  hdr_row <- which(map_int(seq_len(min(10, nrow(raw))), n_years_in_row) > 5)[1]
  e0_row  <- which(raw[[1]] == "0")[1]
  years <- suppressWarnings(as.numeric(str_extract(as.character(raw[hdr_row, ]), "[0-9]{4}")))
  e0    <- suppressWarnings(as.numeric(as.character(raw[e0_row, ])))
  tibble(sex = sex_label, year = years, e0 = e0) %>%
    filter(!is.na(year), !is.na(e0))
}

read_new_proj <- function(proj_year) {
  path <- here("data", "ons_projections", "united_kingdom",
               str_glue("proj_{proj_year}.xlsx"))
  bind_rows(
    read_new_proj_sheet(path, "males period ex",   "male"),
    read_new_proj_sheet(path, "females period ex", "female")
  ) %>%
    mutate(proj_year = proj_year)
}

new_projns_full <- map_dfr(c(2020, 2022, 2024), read_new_proj)

new_projns <- new_projns_full %>%
  filter(year >= 2010) %>%
  transmute(sex, projection = paste(proj_year, "proj"), year, e0)

# 4. Actual e0 series -----------------------------------------------------
# Accuracy report actuals run to 2012; extend with the historic (estimate)
# years of the 2024-based NPP file. The 2024 value is base-year (mid-2024).

actual <- bind_rows(
  older_projns %>% filter(projection == "Actual"),
  new_projns_full %>%
    filter(proj_year == 2024, year > 2012, year <= 2024) %>%
    transmute(sex, projection = "Actual", year, e0)
)

# 5. Figure ---------------------------------------------------------------
# Projection vintage is ordinal, so rounds are coloured on a sequential
# (viridis) ramp rather than the cycled categorical hues of the original:
# dark = older rounds, bright = recent rounds; actuals in black.

all_projns <- bind_rows(
  older_projns %>% filter(projection != "Actual"),
  mid_projns,
  new_projns
) %>%
  mutate(
    round_year = as.numeric(str_extract(projection, "[0-9]{4}")),
    projection = fct_reorder(projection, round_year)
  )

p <- all_projns %>%
  filter(year <= 2040) %>%
  ggplot(aes(x = year, y = e0, group = projection, colour = projection)) +
  geom_line() +
  facet_wrap(~sex) +
  geom_line(
    aes(x = year, y = e0), size = 1.2, inherit.aes = FALSE,
    data = actual %>% filter(year <= 2040)
  ) +
  scale_colour_viridis_d(option = "viridis", end = 0.95) +
  labs(
    x = "Year", y = "Life expectancy at birth in years",
    title = "Life expectancy projections, 1971-2024",
    subtitle = "ONS national population projection rounds (colour, dark = older) against actual estimates (black)",
    caption = paste(
      "Sources: ONS NPP accuracy report underlying data (1971-2012 rounds);",
      "ONS expectation of life, principal projections (2014-2024 rounds).",
      "Actuals to 2012 from accuracy report, 2013-2024 from 2024-based NPP historic estimates.",
      sep = "\n"
    )
  )

ggsave(here("bayes_paper", "figures", "M02_ons_lt_projns_updated.png"),
       plot = p, height = 20, width = 25, units = "cm", dpi = 300)
