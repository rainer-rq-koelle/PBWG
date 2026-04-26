library(devtools)
library(dplyr)
library(fs)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

devtools::load_all("/Users/rainerkoelle/RProjects/PBWG", quiet = TRUE)

study_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL",
  "LEMD", "LFPG", "LGAV", "LIRF", "LSZH", "LTFM"
)

years <- 2023:2025
ref_year <- 2024L
ranges <- c(40, 100)
variant <- "icao_ganp_p20"
min_n <- 5L
max_tma <- 180
n_sectors <- 6L

apdf_dir <- "/Users/rainerkoelle/RProjects/__DATA/APDF"
archive_root <- "/Users/rainerkoelle/RProjects/PBWG-data/airport-tma"
report_data_dir <- "/Users/rainerkoelle/RProjects/BRA-EUR-2025/data"

reference_dir <- path(archive_root, paste0("reference-", ref_year))
plot_dir <- path(archive_root, paste0("sector-plots-", ref_year))
sector_path <- path(archive_root, paste0("EUR-tma-sector-definitions-", ref_year, ".csv"))
reference_coverage_path <- path(archive_root, paste0("EUR-tma-reference-coverage-", ref_year, ".csv"))
quality_path <- path(archive_root, paste0("EUR-tma-input-quality-", ref_year, ".csv"))
coverage_path <- path(archive_root, "EUR-tma-coverage-summary-2023-2025.csv")
combined_path <- path(
  archive_root,
  build_pbwg_tma_filename(
    years = years,
    ref_period = ref_year,
    variant = variant,
    airport = NULL,
    region = "EUR"
  )
)

dir_create(archive_root)
dir_create(reference_dir)
dir_create(plot_dir)
dir_create(report_data_dir)
walk(years, ~ dir_create(path(archive_root, paste0("daily-", .x))))
walk(years, ~ dir_create(path(archive_root, paste0("augmented-", .x))))

zip_path_for <- function(year) {
  path(apdf_dir, paste0("apdf-", year, ".zip"))
}

zip_candidates_for <- function(year, airport) {
  candidates <- c(zip_path_for(year))

  if (airport %in% "LGAV" && year <= 2024) {
    candidates <- c(candidates, path(apdf_dir, "apdf-LGAV-2019-2024.zip"))
  }

  unique(candidates[file_exists(candidates)])
}

member_for <- function(zip_path, airport, year) {
  listing <- check_zip_content(
    path = dirname(zip_path),
    archive = basename(zip_path)
  )$Name

  hit <- listing[
    stringr::str_detect(listing, airport) &
      stringr::str_detect(listing, as.character(year))
  ]

  if (!length(hit)) {
    return(NA_character_)
  }

  hit[[1]]
}

resolve_zip_member <- function(year, airport) {
  candidates <- zip_candidates_for(year, airport)

  for (zip_path in candidates) {
    member <- member_for(zip_path, airport, year)

    if (!is.na(member)) {
      return(list(zip_path = zip_path, member = member))
    }
  }

  list(zip_path = NA_character_, member = NA_character_)
}

read_airport_year_tma <- function(year, airport) {
  resolved <- resolve_zip_member(year, airport)
  zip_path <- resolved$zip_path
  member <- resolved$member

  if (is.na(member)) {
    message("Skipping ", airport, " ", year, " because no APDF member was found.")
    return(tibble::tibble())
  }

  message("Reading ", airport, " ", year, " from ", basename(zip_path), " / ", member)

  read_apdf_zip(
    zipped_archive_path = zip_path,
    files = member,
    type = "parquet"
  ) |>
    prepare_apdf_tma_input() |>
    prepare_tma_reference_input(
      ranges = ranges,
      max_tma = max_tma
    )
}

reference_samples <- purrr::map(
  study_airports,
  function(airport) read_airport_year_tma(ref_year, airport)
) |>
  dplyr::bind_rows()

stopifnot(nrow(reference_samples) > 0)

sector_definitions <- suggest_tma_sector_definitions(
  tma_samples = reference_samples,
  n_sectors = n_sectors,
  valid_only = TRUE
)

write_csv(sector_definitions, sector_path)
write_tma_sector_diagnostic_plots(
  tma_samples = reference_samples,
  sector_definitions = sector_definitions,
  output_dir = plot_dir
)
write_csv(
  summarise_tma_reference_input_quality(reference_samples),
  quality_path
)

reference_samples_assigned <- reference_samples |>
  assign_tma_sector(sector_definitions = sector_definitions)

reference_data <- reference_samples_assigned |>
  build_tma_reference(
    ref_start = ymd_hms(paste0(ref_year, "-01-01 00:00:00"), tz = "UTC"),
    ref_end = ymd_hms(paste0(ref_year, "-12-31 23:59:59"), tz = "UTC"),
    variant = variant,
    min_n = min_n,
    keep_below_threshold = TRUE,
    include_unknown = FALSE
  )

write_csv(
  check_tma_reference_coverage(reference_data, analysis_samples = reference_samples_assigned),
  reference_coverage_path
)

walk(
  unique(reference_data$ICAO),
  function(airport) {
    write_pbwg_tma_reference(
      data = reference_data,
      airport = airport,
      ref_period = ref_year,
      variant = variant,
      min_n = min_n,
      output_dir = reference_dir,
      region = "EUR"
    )
  }
)

annual_outputs <- purrr::map(
  years,
  function(year) {
    groups <- split(
      study_airports,
      purrr::map_chr(
        study_airports,
        function(airport) {
          resolved <- resolve_zip_member(year, airport)
          resolved$zip_path %||% "missing"
        }
      )
    )

    outputs <- purrr::imap(
      groups,
      function(airports_for_zip, zip_path) {
        if (zip_path %in% "missing") {
          return(list(daily_paths = character(0), augmented_paths = character(0)))
        }

        create_pbwg_tma_annual_file(
          zipped_archive_path = zip_path,
          year = year,
          reference_data = reference_data,
          output_dir = path(archive_root, paste0("daily-", year)),
          airports = airports_for_zip,
          type = "parquet",
          ranges = ranges,
          region = "EUR",
          max_tma = max_tma,
          valid_reference_only = TRUE,
          valid_only = TRUE,
          save_augmented = TRUE,
          augmented_dir = path(archive_root, paste0("augmented-", year))
        )
      }
    )

    list(
      daily_paths = purrr::map(outputs, "daily_paths") |> unlist(use.names = TRUE),
      augmented_paths = purrr::map(outputs, "augmented_paths") |> unlist(use.names = TRUE)
    )
  }
)

combined_paths <- purrr::map(annual_outputs, "daily_paths") |>
  unlist(use.names = FALSE)

combined_daily <- write_pbwg_tma(
  data = read_pbwg_tma_files(combined_paths),
  year = years,
  ref_period = ref_year,
  variant = variant,
  output_dir = archive_root,
  airport = NULL,
  region = "EUR"
)

all_augmented <- purrr::map2(
  annual_outputs,
  years,
  function(outputs, year) {
    if (!length(outputs$augmented_paths)) {
      return(tibble::tibble())
    }

    purrr::map(outputs$augmented_paths, readr::read_csv, show_col_types = FALSE) |>
      dplyr::bind_rows() |>
      dplyr::mutate(YEAR = year)
  }
) |>
  dplyr::bind_rows()

if (nrow(all_augmented) > 0) {
  coverage_summary <- all_augmented |>
    dplyr::summarise(
      N_TOTAL = dplyr::n(),
      N_VALID_TMA = sum(.data$VALID_TMA, na.rm = TRUE),
      N_WITH_REFERENCE = sum(.data$HAS_REFERENCE, na.rm = TRUE),
      N_TMA_NA = sum(.data$TMA_NA, na.rm = TRUE),
      PCT_VALID_TMA = .data$N_VALID_TMA / .data$N_TOTAL,
      PCT_WITH_REFERENCE = .data$N_WITH_REFERENCE / .data$N_TOTAL,
      .by = c("YEAR", "ICAO", "PHASE", "RANGE_NM")
    ) |>
    dplyr::arrange(.data$YEAR, .data$ICAO, .data$PHASE, .data$RANGE_NM)

  write_csv(coverage_summary, coverage_path)
}

file_copy(sector_path, path(report_data_dir, path_file(sector_path)), overwrite = TRUE)
file_copy(reference_coverage_path, path(report_data_dir, path_file(reference_coverage_path)), overwrite = TRUE)
file_copy(quality_path, path(report_data_dir, path_file(quality_path)), overwrite = TRUE)
if (file_exists(coverage_path)) {
  file_copy(coverage_path, path(report_data_dir, path_file(coverage_path)), overwrite = TRUE)
}
if (file_exists(combined_daily)) {
  file_copy(combined_daily, path(report_data_dir, path_file(combined_daily)), overwrite = TRUE)
}

message("Wrote sector definitions to: ", sector_path)
message("Wrote sector plots to: ", plot_dir)
message("Wrote reference files to: ", reference_dir)
message("Wrote combined daily file to: ", combined_daily)
