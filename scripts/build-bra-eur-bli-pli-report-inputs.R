suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(here)
  library(devtools)
})

devtools::load_all(here::here(), quiet = TRUE)

report_dir <- here::here("..", "BRA-EUR-2025")
report_data_dir <- file.path(report_dir, "data")
old_report_data_dir <- here::here("..", "BRA-EUR-2024", "data")

study_airports <- list(
  bra = c("SBGR", "SBGL", "SBRJ", "SBCF", "SBBR", "SBSV", "SBKP", "SBSP", "SBCT", "SBPA", "SBRF", "SBEG"),
  eur = c("EGLL", "EGKK", "EHAM", "EDDF", "EDDM", "LSZH", "LFPG", "LEMD", "LEBL", "LPPT", "LGAV", "LTFM")
)

carry_forward_capacity <- function(capacities, from_year, to_year) {
  capacities |>
    bind_rows(
      capacities |>
        filter(YEAR == from_year) |>
        mutate(YEAR = to_year)
    )
}

bra_cap_history <- tibble::tribble(
  ~APT_ICAO, ~YEAR, ~MAX_CAP,
  "SBCT", 2019, 28,
  "SBCT", 2020, 32,
  "SBCT", 2021, 32,
  "SBCT", 2022, 32,
  "SBPA", 2019, 30,
  "SBPA", 2020, 36,
  "SBPA", 2021, 36,
  "SBPA", 2022, 36,
  "SBSV", 2019, 32,
  "SBSV", 2020, 36,
  "SBSV", 2021, 36,
  "SBSV", 2022, 36,
  "SBRJ", 2019, 29,
  "SBRJ", 2020, 29,
  "SBRJ", 2021, 29,
  "SBRJ", 2022, 29,
  "SBKP", 2019, 35,
  "SBKP", 2020, 40,
  "SBKP", 2021, 40,
  "SBKP", 2022, 40,
  "SBCF", 2019, 35,
  "SBCF", 2020, 37,
  "SBCF", 2021, 37,
  "SBCF", 2022, 37,
  "SBSP", 2019, 41,
  "SBSP", 2020, 42,
  "SBSP", 2021, 44,
  "SBSP", 2022, 44,
  "SBGL", 2019, 54,
  "SBGL", 2020, 60,
  "SBGL", 2021, 60,
  "SBGL", 2022, 60,
  "SBGR", 2019, 57,
  "SBGR", 2020, 58,
  "SBGR", 2021, 60,
  "SBGR", 2022, 60,
  "SBBR", 2019, 57,
  "SBBR", 2020, 80,
  "SBBR", 2021, 80,
  "SBBR", 2022, 80
) |>
  carry_forward_capacity(2022, 2023) |>
  carry_forward_capacity(2023, 2024) |>
  bind_rows(
    tibble::tribble(
      ~APT_ICAO, ~YEAR, ~MAX_CAP,
      "SBGR", 2025, 60,
      "SBGL", 2025, 60,
      "SBRJ", 2025, 29,
      "SBCF", 2025, 37,
      "SBBR", 2025, 80,
      "SBSV", 2025, 36,
      "SBKP", 2025, 40,
      "SBSP", 2025, 44,
      "SBCT", 2025, 32,
      "SBPA", 2025, 36,
      "SBRF", 2025, 38,
      "SBEG", 2025, 38
    )
  )

eur_cap_history <- tibble::tribble(
  ~APT_ICAO, ~YEAR, ~MAX_CAP,
  "EDDF", 2019, 106,
  "EDDF", 2020, 106,
  "EDDF", 2021, 106,
  "EDDF", 2022, 106,
  "EDDM", 2019, 90,
  "EDDM", 2020, 90,
  "EDDM", 2021, 90,
  "EDDM", 2022, 90,
  "EGKK", 2019, 55,
  "EGKK", 2020, 55,
  "EGKK", 2021, 55,
  "EGKK", 2022, 55,
  "EGLL", 2019, 88,
  "EGLL", 2020, 88,
  "EGLL", 2021, 88,
  "EGLL", 2022, 88,
  "EHAM", 2019, 112,
  "EHAM", 2020, 112,
  "EHAM", 2021, 112,
  "EHAM", 2022, 112,
  "LEBL", 2019, 78,
  "LEBL", 2020, 78,
  "LEBL", 2021, 78,
  "LEBL", 2022, 78,
  "LEMD", 2019, 100,
  "LEMD", 2020, 100,
  "LEMD", 2021, 100,
  "LEMD", 2022, 100,
  "LFPG", 2019, 120,
  "LFPG", 2020, 120,
  "LFPG", 2021, 120,
  "LFPG", 2022, 120,
  "LSZH", 2019, 66,
  "LSZH", 2020, 66,
  "LSZH", 2021, 66,
  "LSZH", 2022, 66,
  "LPPT", 2019, 40,
  "LPPT", 2020, 40,
  "LPPT", 2021, 40,
  "LPPT", 2022, 40
) |>
  carry_forward_capacity(2022, 2023) |>
  carry_forward_capacity(2023, 2024) |>
  bind_rows(
    tibble::tribble(
      ~APT_ICAO, ~YEAR, ~MAX_CAP,
      "EGLL", 2025, 92,
      "EGKK", 2025, 55,
      "EHAM", 2025, 112,
      "EDDF", 2025, 106,
      "EDDM", 2025, 90,
      "LSZH", 2025, 66,
      "LFPG", 2025, 120,
      "LEMD", 2025, 100,
      "LEBL", 2025, 85,
      "LPPT", 2025, 40,
      "LGAV", 2025, 44,
      "LTFM", 2025, 120
    )
  )

capacity_history <- bind_rows(bra_cap_history, eur_cap_history)

bra_hist_thru <- readr::read_csv(
  file.path(old_report_data_dir, "BRA-THRU-analytic.csv"),
  show_col_types = FALSE
) |>
  mutate(REG = "BRA") |>
  filter(ICAO %in% study_airports$bra, year(BIN) >= 2019) |>
  select(REG, ICAO, BIN, ARRS, DEPS)

eur_hist_thru <- list.files(
  old_report_data_dir,
  pattern = "^EUR-THRU-",
  full.names = TRUE
) |>
  purrr::map_dfr(readr::read_csv, show_col_types = FALSE) |>
  mutate(
    REG = "EUR",
    ARRS = ifelse(is.na(ARRS), ARR_THRU, ARRS),
    DEPS = ifelse(is.na(DEPS), DEP_THRU, DEPS)
  ) |>
  select(REG, ICAO, BIN, ARRS, DEPS) |>
  filter(ICAO %in% study_airports$eur, !is.na(BIN)) |>
  mutate(BIN = floor_date(BIN, unit = "hour")) |>
  group_by(REG, ICAO, BIN) |>
  summarise(
    ARRS = sum(ARRS, na.rm = TRUE),
    DEPS = sum(DEPS, na.rm = TRUE),
    .groups = "drop"
  )

thru_2025 <- readr::read_csv(
  file.path(report_data_dir, "PBWG-BRA-EUR-thru-rwy-15min-2025.csv"),
  show_col_types = FALSE
) |>
  group_by(REG, ICAO, BIN) |>
  summarise(
    ARRS = sum(ARRS, na.rm = TRUE),
    DEPS = sum(DEPS, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(BIN = floor_date(BIN, unit = "hour")) |>
  group_by(REG, ICAO, BIN) |>
  summarise(
    ARRS = sum(ARRS, na.rm = TRUE),
    DEPS = sum(DEPS, na.rm = TRUE),
    .groups = "drop"
  )

hourly_throughput <- bind_rows(bra_hist_thru, eur_hist_thru, thru_2025) |>
  distinct() |>
  arrange(REG, ICAO, BIN)

throughput_loads <- prepare_throughput_load_characteristics(
  throughput = hourly_throughput,
  capacities = capacity_history,
  base_threshold = 0.2,
  peak_threshold = 0.8
)

bli_pli_summary <- summarise_load_indices(throughput_loads) |>
  left_join(
    hourly_throughput |>
      distinct(ICAO, REG),
    by = "ICAO"
  ) |>
  arrange(REG, ICAO, YEAR)

ordered_example <- throughput_loads |>
  filter(ICAO %in% c("LPPT", "SBGR"), YEAR %in% c(2019, 2023, 2024, 2025)) |>
  prepare_ordered_throughput() |>
  arrange(REG, ICAO, YEAR, RANK)

readr::write_csv(
  bli_pli_summary,
  file.path(report_data_dir, "PBWG-BRA-EUR-bli-pli-2019-2025.csv")
)

readr::write_csv(
  ordered_example,
  file.path(report_data_dir, "PBWG-BRA-EUR-ordered-throughput-LPPT-SBGR-2019-2025.csv")
)

message("Wrote BLI/PLI report inputs to: ", report_data_dir)
