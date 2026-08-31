suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(lubridate)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(devtools)
})

devtools::load_all(".", quiet = TRUE)

region <- "EUR"
archive_root <- "/Users/rainerkoelle/RProjects/PBWG-data"
apdf_root <- "/Users/rainerkoelle/RProjects/__DATA/APDF"
output_dir <- path(archive_root, "airport-throughput")

study_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
  "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM"
)

make_source_spec <- function(year) {
  base_zip <- path(apdf_root, str_c("apdf-", year, ".zip"))

  if (year == 2024L) {
    base_files <- check_zip_content(path_dir(base_zip), path_file(base_zip)) |>
      filter(str_detect(.data$Name, "^[A-Z]{4}_01-JAN-2024_31-DEC-2024[.]gz[.]parquet$")) |>
      transmute(
        ICAO = str_extract(.data$Name, "^[A-Z]{4}"),
        zip = base_zip,
        file = .data$Name
      ) |>
      filter(.data$ICAO %in% study_airports)

    extra_files <- tribble(
      ~ICAO, ~zip, ~file,
      "LGAV", path(apdf_root, "apdf-LGAV-2019-2024.zip"), "LGAV_01-JAN-2024_31-DEC-2024.parquet",
      "LPPT", path(apdf_root, "LPPT-apdf.zip"), "LPPT_01-JAN-2024_31-DEC-2024.gz.parquet"
    )

    return(bind_rows(base_files, extra_files) |> arrange(.data$ICAO))
  }

  if (year == 2025L) {
    files <- check_zip_content(path_dir(base_zip), path_file(base_zip)) |>
      filter(str_detect(.data$Name, "^[A-Z]{4}_01-JAN-2025_31-DEC-2025[.]parquet$")) |>
      transmute(
        ICAO = str_extract(.data$Name, "^[A-Z]{4}"),
        zip = base_zip,
        file = .data$Name
      ) |>
      filter(.data$ICAO %in% study_airports)

    return(files |> arrange(.data$ICAO))
  }

  if (year == 2026L) {
    zip <- path(apdf_root, "apdf-2026-pJune.zip")
    files <- check_zip_content(path_dir(zip), path_file(zip)) |>
      filter(str_detect(.data$Name, "^[A-Z]{4}_APDF_2026[.]parquet$")) |>
      transmute(
        ICAO = str_extract(.data$Name, "^[A-Z]{4}"),
        zip = zip,
        file = .data$Name
      ) |>
      filter(.data$ICAO %in% study_airports)

    return(files |> arrange(.data$ICAO))
  }

  stop("Unsupported year: ", year)
}

prepare_runway_throughput <- function(zip, file, year) {
  read_apdf_zip(
    zipped_archive_path = zip,
    files = file,
    type = "parquet"
  ) |>
    prepare_apdf_traffic_input() |>
    mutate(
      BIN = floor_date(.data$MVT_TIME, unit = "15 mins"),
      RWY = as.character(.data$RWY)
    ) |>
    filter(year(.data$BIN) == year) |>
    summarise(
      ARRS = sum(.data$PHASE %in% "ARR", na.rm = TRUE),
      DEPS = sum(.data$PHASE %in% "DEP", na.rm = TRUE),
      .by = c("ICAO", "BIN", "RWY")
    ) |>
    mutate(
      FLTS = .data$ARRS + .data$DEPS,
      BIN = format(.data$BIN, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) |>
    arrange(.data$ICAO, .data$BIN, .data$RWY)
}

write_year <- function(year, year_label = as.character(year)) {
  spec <- make_source_spec(year)
  missing_airports <- setdiff(study_airports, spec$ICAO)

  if (length(missing_airports) > 0) {
    stop("Missing APDF source files for: ", str_c(missing_airports, collapse = ", "))
  }

  message("Writing ", year_label, " 15-minute runway throughput...")

  annual <- pmap_dfr(
    select(spec, "zip", "file"),
    \(zip, file) prepare_runway_throughput(zip = zip, file = file, year = year)
  )

  airport_paths <- map_chr(
    sort(unique(annual$ICAO)),
    \(airport) write_pbwg_throughput(
      data = annual,
      year = year_label,
      output_dir = output_dir,
      airport = airport,
      region = region
    )
  )

  aggregate_path <- write_pbwg_throughput(
    data = annual,
    year = year_label,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )

  tibble(
    year = year_label,
    rows = nrow(annual),
    min_bin = min(annual$BIN, na.rm = TRUE),
    max_bin = max(annual$BIN, na.rm = TRUE),
    n_airports = length(unique(annual$ICAO)),
    files = length(c(airport_paths, aggregate_path))
  )
}

summary <- bind_rows(
  write_year(2024L),
  write_year(2025L),
  write_year(2026L, year_label = "2026p")
)

print(summary)
