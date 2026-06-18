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

apdf_zip <- path(data_root, "APDF", "apdf-2026-pMay.zip")
nm_zip <- path(data_root, "NM-flight-table", "nm-flt-2026-pJune.zip")

statfor_rules_path <- "/Users/rainerkoelle/Downloads/eurocontrol-statfor-market-segments-rules-for-sid-2025-definition.xlsx"

study_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
  "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM"
)

stopifnot(file_exists(apdf_zip))
stopifnot(file_exists(nm_zip))
stopifnot(file_exists(statfor_rules_path))

write_airport_traffic_2026p <- function(apdf_zip, airports) {
  output_dir <- path(archive_root, "airport-traffic")
  traffic <- prepare_apdf_daily_traffic_zip(
    zipped_archive_path = apdf_zip,
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

write_punctuality_2026p <- function(apdf_zip, airports) {
  output_dir <- path(archive_root, "airport-punctuality")
  punctuality <- prepare_apdf_punctuality_zip(
    zipped_archive_path = apdf_zip,
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

write_throughput_2026p <- function(apdf_zip, airports) {
  output_dir <- path(archive_root, "airport-throughput")
  throughput <- prepare_apdf_throughput_zip(
    zipped_archive_path = apdf_zip,
    type = "parquet",
    year = year_filter,
    unit = "hour"
  ) |>
    filter(.data$ICAO %in% airports)

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

write_tma_2026p <- function(apdf_zip, airports) {
  reference_dir <- path(archive_root, "airport-tma", "reference-2024")
  reference_paths <- dir_ls(reference_dir, regexp = "ref-tma-2024-icao_ganp_p20-n5[.]csv$")

  reference_data <- map_dfr(
    reference_paths,
    \(path) read_csv(
      path,
      col_types = cols(
        .default = col_guess(),
        RWY = col_character()
      ),
      show_col_types = FALSE
    )
  )
  tma_airports <- intersect(airports, sort(unique(reference_data$ICAO)))

  augmented <- prepare_apdf_tma_augmented_zip(
    zipped_archive_path = apdf_zip,
    reference_data = reference_data,
    type = "parquet",
    year = year_filter,
    ranges = c(40, 100),
    max_tma = 180,
    valid_reference_only = TRUE
  ) |>
    filter(.data$ICAO %in% tma_airports)

  daily <- summarise_pbwg_tma_daily(
    augmented_tma = augmented,
    year = year_filter,
    valid_only = TRUE
  )

  ref_period <- unique(reference_data$REF_PERIOD)
  ref_variant <- unique(reference_data$REF_VARIANT)

  if (length(ref_period) != 1 || length(ref_variant) != 1) {
    stop("TMA reference data must contain exactly one reference period and variant.")
  }

  daily_dir <- path(archive_root, "airport-tma", str_c("daily-", year_label))
  augmented_dir <- path(archive_root, "airport-tma", str_c("augmented-", year_label))

  daily_paths <- map_chr(
    sort(unique(daily$ICAO)),
    \(airport) write_pbwg_tma(
      data = daily,
      year = year_label,
      ref_period = ref_period,
      variant = ref_variant,
      output_dir = daily_dir,
      airport = airport,
      region = region
    )
  )

  augmented_paths <- map_chr(
    sort(unique(augmented$ICAO)),
    \(airport) write_pbwg_tma_augmented(
      data = augmented,
      year = year_label,
      airport = airport,
      ref_period = ref_period,
      variant = ref_variant,
      output_dir = augmented_dir,
      region = region
    )
  )

  aggregate_path <- write_pbwg_tma(
    data = daily,
    year = year_label,
    ref_period = ref_period,
    variant = ref_variant,
    output_dir = path(archive_root, "airport-tma"),
    airport = NULL,
    region = region
  )

  skipped <- setdiff(airports, tma_airports)

  list(
    paths = c(daily_paths, augmented_paths, aggregate_path),
    data = daily,
    skipped_airports = skipped
  )
}

message("Writing airport traffic...")
airport_traffic <- write_airport_traffic_2026p(apdf_zip, study_airports)

message("Writing punctuality...")
punctuality <- write_punctuality_2026p(apdf_zip, study_airports)

message("Writing throughput...")
throughput <- write_throughput_2026p(apdf_zip, study_airports)

message("Writing network traffic...")
network <- write_network_traffic_2026p(nm_zip, statfor_rules_path)

message("Writing TMA...")
tma <- write_tma_2026p(apdf_zip, study_airports)

summary <- tribble(
  ~family, ~rows, ~files,
  "airport-traffic", nrow(airport_traffic$data), length(airport_traffic$paths),
  "airport-punctuality", nrow(punctuality$data), length(punctuality$paths),
  "airport-throughput", nrow(throughput$data), length(throughput$paths),
  "network-traffic", nrow(network$data), length(network$paths),
  "airport-tma", nrow(tma$data), length(tma$paths)
)

print(summary)

if (length(tma$skipped_airports) > 0) {
  message("TMA skipped airports without reference data: ", str_flatten(tma$skipped_airports, ", "))
}
