# Functions for parsing ONS "expectation of life, principal projection"
# xlsx releases (2020-based interim, 2022-based, 2024-based).
#
# These releases embed historic period ex estimates (from 1981/1982 up to the
# base year) alongside projected values, in one wide table per sex:
# rows = exact age, columns = year. Sheet layout details (header labels,
# leading metadata rows) vary between releases, so rows are located by
# content rather than position.

#' Find the header row of a wide period-ex sheet.
#'
#' The header row is identified as the first row in which more than five
#' cells begin with a four-digit year, which is robust to the varying row
#' labels ("age", "Exact age (years)") and metadata-row counts across
#' releases.
#'
#' @param raw A tibble read with `col_names = FALSE`.
#' @return Integer row index, or NA if no such row is found.
find_year_header_row <- function(raw) {
  n_years_in_row <- function(i) {
    sum(!is.na(suppressWarnings(
      as.numeric(stringr::str_extract(as.character(raw[i, ]), "^[0-9]{4}"))
    )))
  }
  candidates <- purrr::map_int(seq_len(min(15, nrow(raw))), n_years_in_row)
  which(candidates > 5)[1]
}

#' Read one sex's period-ex sheet from an NPP expectation-of-life file.
#'
#' @param path Path to the xlsx file.
#' @param sheet Sheet name (e.g. "males period ex").
#' @param sex_label Label to attach ("male" / "female").
#' @return Long tibble: sex, age, year, ex.
read_npp_period_ex_sheet <- function(path, sheet, sex_label) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                            .name_repair = "unique_quiet")
  hdr_row <- find_year_header_row(raw)
  stopifnot(!is.na(hdr_row))
  years <- suppressWarnings(
    as.numeric(stringr::str_extract(as.character(raw[hdr_row, ]), "^[0-9]{4}"))
  )
  age_rows <- which(stringr::str_detect(raw[[1]], "^[0-9]+$"))
  age_rows <- age_rows[age_rows > hdr_row]

  purrr::map_dfr(age_rows, function(r) {
    vals <- suppressWarnings(as.numeric(as.character(raw[r, ])))
    tibble::tibble(
      sex  = sex_label,
      age  = as.numeric(raw[[1]][r]),
      year = years,
      ex   = vals
    )
  }) %>%
    dplyr::filter(!is.na(year), !is.na(ex))
}

#' Read both sexes' period ex from an NPP expectation-of-life file.
#'
#' Sheet names are matched case-insensitively ("Males period ex" in the
#' pre-2020 releases, "males period ex" thereafter).
#'
#' @param path Path to the xlsx file.
#' @return Long tibble: sex, age, year, ex.
read_npp_period_ex <- function(path) {
  sheets <- readxl::excel_sheets(path)
  male_sheet   <- sheets[stringr::str_detect(tolower(sheets), "^males period ex")][1]
  female_sheet <- sheets[stringr::str_detect(tolower(sheets), "^females period ex")][1]
  dplyr::bind_rows(
    read_npp_period_ex_sheet(path, male_sheet,   "male"),
    read_npp_period_ex_sheet(path, female_sheet, "female")
  )
}
