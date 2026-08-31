suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(devtools)
})

devtools::load_all(".", quiet = TRUE)

year_filter <- 2026L
year_label <- "2026p"
region <- "EUR"

data_root <- "/Users/rainerkoelle/RProjects/__DATA"
archive_root <- "/Users/rainerkoelle/RProjects/PBWG-data"

apdf_zip <- path(data_root, "APDF", "apdf-2026-pJune.zip")
nm_zip <- path(data_root, "NM-flight-table", "nm-flt-2026-pJune.zip")

statfor_rules_path <- "/Users/rainerkoelle/Downloads/eurocontrol-statfor-market-segments-rules-for-sid-2025-definition.xlsx"

study_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
  "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM"
)

stopifnot(file_exists(apdf_zip))
stopifnot(file_exists(nm_zip))
stopifnot(file_exists(statfor_rules_path))

apdf_files <- check_zip_content(
  path = path_dir(apdf_zip),
  archive = path_file(apdf_zip)
) |>
  filter(str_detect(.data$Name, str_c("^(", str_c(study_airports, collapse = "|"), ")_APDF_2026[.]parquet$"))) |>
  pull("Name")

if (length(apdf_files) != length(study_airports)) {
  missing_airports <- setdiff(study_airports, str_extract(apdf_files, "^[A-Z]{4}"))
  stop("APDF archive is missing study airports: ", str_c(missing_airports, collapse = ", "))
}

write_airport_traffic_2026p <- function(apdf_zip, files, airports) {
  output_dir <- path(archive_root, "airport-traffic")
  traffic <- prepare_apdf_daily_traffic_zip(
    zipped_archive_path = apdf_zip,
    files = files,
    type = "parquet",
    year = year_filter
  ) |>
    filter(.data$ICAO %in% airports)

  airport_paths <- map_chr(
    sort(unique(traffic$ICAO)),
    \(airport) write_pbwg_airport_traffic(
      data = traffic,
      airport = airport,
      year = year_label,
      output_dir = output_dir,
      region = region
    )
  )

  aggregate_path <- path(output_dir, str_c("PBWG-", region, "-airport-traffic-", year_label, ".csv"))
  write_csv(traffic, aggregate_path)

  list(paths = c(airport_paths, aggregate_path), data = traffic)
}

write_punctuality_2026p <- function(apdf_zip, files, airports) {
  output_dir <- path(archive_root, "airport-punctuality")
  punctuality <- prepare_apdf_punctuality_zip(
    zipped_archive_path = apdf_zip,
    files = files,
    type = "parquet",
    year = year_filter
  ) |>
    filter(.data$ICAO %in% airports)

  airport_paths <- map_chr(
    sort(unique(punctuality$ICAO)),
    \(airport) write_pbwg_punctuality(
      data = punctuality,
      year = year_label,
      output_dir = output_dir,
      airport = airport,
      region = region
    )
  )

  aggregate_path <- write_pbwg_punctuality(
    data = punctuality,
    year = year_label,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )

  list(paths = c(airport_paths, aggregate_path), data = punctuality)
}

write_throughput_2026p <- function(apdf_zip, files, airports) {
  output_dir <- path(archive_root, "airport-throughput")
  throughput <- prepare_apdf_throughput_zip(
    zipped_archive_path = apdf_zip,
    files = files,
    type = "parquet",
    year = year_filter,
    unit = "15 mins",
    by_runway = TRUE
  ) |>
    filter(.data$ICAO %in% airports) |>
    mutate(BIN = format(.data$BIN, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

  airport_paths <- map_chr(
    sort(unique(throughput$ICAO)),
    \(airport) write_pbwg_throughput(
      data = throughput,
      year = year_label,
      output_dir = output_dir,
      airport = airport,
      region = region
    )
  )

  aggregate_path <- write_pbwg_throughput(
    data = throughput,
    year = year_label,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )

  list(paths = c(airport_paths, aggregate_path), data = throughput)
}

write_network_traffic_2026p <- function(nm_zip, rules_path) {
  output_dir <- path(archive_root, "network-traffic")
  rules <- read_statfor_market_segment_rules(rules_path)

  network <- prepare_nm_regional_traffic_zip(
    zipped_archive_path = nm_zip,
    type = "parquet",
    airport_classifier = is_ectrl_member_state_airport,
    market_segment_rules = rules
  ) |>
    filter(lubridate::year(.data$DATE) == year_filter)

  output_path <- write_pbwg_network_traffic(
    data = network,
    year = year_label,
    output_dir = output_dir,
    region = region
  )

  list(paths = output_path, data = network)
}

summarise_output <- function(data, date_column) {
  dates <- data[[date_column]]
  tibble(
    rows = nrow(data),
    min_date = as.character(min(dates, na.rm = TRUE)),
    max_date = as.character(max(dates, na.rm = TRUE)),
    n_dates = length(unique(dates[!is.na(dates)])),
    n_airports = if ("ICAO" %in% names(data)) length(unique(data$ICAO)) else NA_integer_
  )
}

message("Writing airport traffic...")
airport_traffic <- write_airport_traffic_2026p(apdf_zip, apdf_files, study_airports)

message("Writing punctuality...")
punctuality <- write_punctuality_2026p(apdf_zip, apdf_files, study_airports)

message("Writing throughput...")
throughput <- write_throughput_2026p(apdf_zip, apdf_files, study_airports)

message("Writing network traffic...")
network <- write_network_traffic_2026p(nm_zip, statfor_rules_path)

summary <- bind_rows(
  mutate(summarise_output(airport_traffic$data, "DATE"), family = "airport-traffic", files = length(airport_traffic$paths)),
  mutate(summarise_output(punctuality$data, "DATE"), family = "airport-punctuality", files = length(punctuality$paths)),
  mutate(summarise_output(throughput$data, "BIN"), family = "airport-throughput", files = length(throughput$paths)),
  mutate(summarise_output(network$data, "DATE"), family = "network-traffic", files = length(network$paths))
) |>
  select("family", "rows", "min_date", "max_date", "n_dates", "n_airports", "files")

print(summary)
