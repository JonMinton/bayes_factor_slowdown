# Refresh data/e0_hmd.csv to a current HMD vintage.
#
# Pulls period life expectancy at birth (E0per.txt) for the paper's full
# comparator country list, using HMD's session-cookie authentication. The
# cookie is created separately by the author (see scripts/hmd_login.sh);
# this script only consumes it, so no credentials appear here.
#
# Approach adapted from ../hmd-population-discrepancies/scripts/01_download_hmd.R
# (which uses the same GetDocument/hmd.v6/<country>/STATS/<item> endpoint).
#
# Usage:
#   HMD_COOKIE_FILE=/tmp/hmd_cookies4.txt Rscript scripts/refresh_hmd_e0.R
#
# Output: data/e0_hmd_2024vintage.csv (country, year, sex, e0), written
# alongside the old data/e0_hmd.csv rather than overwriting it, so the
# refresh can be QA'd against the stale extract before adoption.

suppressMessages({library(tidyverse); library(here)})

cookie_file <- Sys.getenv("HMD_COOKIE_FILE", "/tmp/hmd_cookies4.txt")
if (!file.exists(cookie_file)) {
  stop("No HMD cookie file at '", cookie_file,
       "'. Run scripts/hmd_login.sh first to authenticate.")
}

# The paper's 50 populations (codes from data/e0_hmd.csv).
countries <- c(
  "AUS","AUT","BEL","BGR","BLR","CAN","CHL","HRV","HKG","CHE","CZE",
  "DEUTNP","DEUTE","DEUTW","DNK","ESP","EST","FIN","FRATNP","FRACNP",
  "GRC","HUN","IRL","ISL","ISR","ITA","JPN","KOR","LTU","LUX","LVA",
  "NLD","NOR","NZL_NP","NZL_MA","NZL_NM","POL","PRT","RUS","SVK","SVN",
  "SWE","TWN","UKR","GBR_NP","GBRTENW","GBRCENW","GBR_SCO","GBR_NIR","USA"
)

# E0per.txt is a fixed-width table: metadata line, blank, header
# (Year Female Male Total), then rows. Parse robustly.
fetch_e0per <- function(country) {
  url <- paste0("https://www.mortality.org/File/GetDocument/hmd.v6/",
                country, "/STATS/E0per.txt")
  tmp <- tempfile()
  status <- system2("curl", c("-s", "-L", "-b", shQuote(cookie_file),
                              "-o", shQuote(tmp), shQuote(url)))
  lines <- readLines(tmp, warn = FALSE)
  unlink(tmp)
  # An auth failure returns HTML, not a data table; detect and skip.
  if (length(lines) < 4 || any(grepl("<html", lines[1:3], ignore.case = TRUE))) {
    warning("No data for ", country, " (auth expired or country code invalid?)")
    return(NULL)
  }
  hdr <- grep("Year", lines)[1]
  read_table(paste(lines[(hdr):length(lines)], collapse = "\n"),
             show_col_types = FALSE) %>%
    transmute(
      country = country,
      year = suppressWarnings(as.integer(Year)),
      female = suppressWarnings(as.numeric(Female)),
      male   = suppressWarnings(as.numeric(Male))
    ) %>%
    filter(!is.na(year)) %>%
    pivot_longer(c(female, male), names_to = "sex", values_to = "e0")
}

message("Pulling E0per for ", length(countries), " countries...")
e0_new <- map(countries, function(c) {
  Sys.sleep(0.4)
  out <- tryCatch(fetch_e0per(c), error = function(e) {
    warning("Failed ", c, ": ", conditionMessage(e)); NULL
  })
  if (!is.null(out)) message("  ", c, ": ", nrow(out), " rows to ", max(out$year))
  out
}) %>% compact() %>% bind_rows()

write_csv(e0_new, here("data", "e0_hmd_2024vintage.csv"))

# QA against the stale extract.
old <- read_csv(here("data", "e0_hmd.csv"), show_col_types = FALSE)
message("\n--- QA: max year per country, old vs new ---")
full_join(
  old %>% group_by(country) %>% summarise(old_max = max(year)),
  e0_new %>% group_by(country) %>% summarise(new_max = max(year)),
  by = "country"
) %>% arrange(country) %>% print(n = Inf)
