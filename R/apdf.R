#' List Available APDF Archives
#'
#' Lists files in a directory containing APDF archives.
#'
#' @param path Directory containing APDF archives.
#' @param glob Optional glob pattern used to filter the listed files.
#'
#' @return A character vector of file paths.
#' @export
list_apdf_archives <- function(path, glob = "*.zip") {
  fs::dir_ls(path = path, glob = glob, type = "file")
}

#' Read APDF Data from a ZIP Archive
#'
#' Reads one or more files from a zipped APDF extract.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of files to extract from the archive.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#'
#' @return A tibble when one file is read, otherwise a named list of tibbles.
#' @export
read_apdf_zip <- function(zipped_archive_path, files = NULL, type = c("parquet", "csv", "csv_auto")) {
  type <- base::match.arg(type)

  read_zip_content(
    path = base::dirname(zipped_archive_path),
    archive = base::basename(zipped_archive_path),
    files = files,
    type = type
  )
}

#' APDF Traffic Dictionary
#'
#' Returns the default fields used for airport traffic preparation.
#'
#' @return A character vector of source field names.
#' @export
apdf_traffic_fields <- function() {
  c(
    "AP_C_FLTID",
    "ADEP_ICAO", "ADES_ICAO",
    "AP_C_REG", "ARCTYP", "AC_CLASS",
    "AP_C_RWY", "AP_C_STND",
    "MVT_TIME_UTC", "BLOCK_TIME_UTC", "SCHED_TIME_UTC",
    "C40_CROSS_TIME", "C40_BEARING",
    "C100_CROSS_TIME", "C100_BEARING",
    "SRC_PHASE",
    "IM_SAMAD_ID",
    "SRC_AIRPORT"
  )
}

#' Trim APDF Data for Airport Traffic Preparation
#'
#' Keeps only the fields needed for the PBWG airport traffic preparation flow.
#'
#' @param apdf An APDF tibble.
#' @param fields Character vector of source field names to keep.
#'
#' @return A tibble.
#' @export
trim_apdf <- function(apdf, fields = apdf_traffic_fields()) {
  dplyr::select(tibble::as_tibble(apdf), dplyr::any_of(fields))
}

#' Add the Operating Date to APDF Data
#'
#' Adds a `DATE` column using `BLOCK_TIME` when available and `MVT_TIME`
#' otherwise.
#'
#' @param apdf A decoded APDF tibble.
#'
#' @return A tibble.
#' @export
add_apdf_date <- function(apdf) {
  tibble::as_tibble(apdf) |>
    dplyr::mutate(
      DATE = dplyr::case_when(
        !is.na(.data$BLOCK_TIME) ~ lubridate::date(.data$BLOCK_TIME),
        is.na(.data$BLOCK_TIME) & !is.na(.data$MVT_TIME) ~ lubridate::date(.data$MVT_TIME),
        .default = as.Date(NA)
      )
    )
}

#' Prepare APDF Data for Airport Traffic Processing
#'
#' Trims source fields, decodes them to harmonised names, and adds a `DATE`
#' column used for daily airport traffic summaries.
#'
#' @param apdf An APDF tibble.
#' @param dictionary A tibble with `SOURCE_NAME` and `TARGET_NAME`.
#' @param fields Character vector of source fields to keep before decoding.
#'
#' @return A tibble.
#' @export
prepare_apdf_traffic_input <- function(
    apdf,
    dictionary = apdf_dictionary(),
    fields = apdf_traffic_fields()
) {
  apdf |>
    trim_apdf(fields = fields) |>
    decode_apdf(dictionary = dictionary) |>
    add_apdf_date()
}

#' Prepare Daily PBWG Airport Traffic from APDF Data
#'
#' Aggregates prepared APDF data to daily airport traffic counts, including
#' total arrivals and departures, D/A/I regional splits, and class counts.
#'
#' @param apdf A prepared APDF tibble.
#' @param year Optional reporting year filter.
#' @param region_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return A tibble with daily airport traffic summaries.
#' @export
prepare_apdf_daily_traffic <- function(
    apdf,
    year = NULL,
    region_classifier = is_eurocontrol_airport
) {
  stop_if_apdf_columns_missing(
    apdf,
    required_columns = c("AERODROME", "DATE", "PHASE", "ADEP", "ADES", "CLASS")
  )

  daily_traffic <- tibble::as_tibble(apdf) |>
    dplyr::mutate(
      ADEP_IN_REGION = region_classifier(.data$ADEP),
      ADES_IN_REGION = region_classifier(.data$ADES)
    ) |>
    dplyr::group_by(.data$AERODROME, .data$DATE) |>
    dplyr::summarise(
      ARRS = sum(.data$PHASE %in% "ARR", na.rm = TRUE),
      DEPS = sum(.data$PHASE %in% "DEP", na.rm = TRUE),
      SRC_NA = sum(is.na(.data$PHASE)),
      A = sum(.data$PHASE %in% "ARR" & !.data$ADEP_IN_REGION, na.rm = TRUE),
      D = sum(.data$PHASE %in% "DEP" & !.data$ADES_IN_REGION, na.rm = TRUE),
      I = sum(
        (.data$PHASE %in% "ARR" & .data$ADEP_IN_REGION) |
          (.data$PHASE %in% "DEP" & .data$ADES_IN_REGION),
        na.rm = TRUE
      ),
      HEL = sum(.data$CLASS %in% "HEL", na.rm = TRUE),
      H = sum(.data$CLASS %in% "H", na.rm = TRUE),
      M = sum(.data$CLASS %in% c("M", "MJ", "MT"), na.rm = TRUE),
      L = sum(.data$CLASS %in% c("L", "LJ", "LT", "LP"), na.rm = TRUE),
      CLASS_NA = sum(is.na(.data$CLASS)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$AERODROME, .data$DATE)

  if (is.null(year)) {
    return(daily_traffic)
  }

  dplyr::filter(daily_traffic, lubridate::year(.data$DATE) == year)
}

#' Prepare Daily PBWG Airport Traffic Directly from an APDF ZIP Archive
#'
#' Reads APDF files from an archive, prepares each airport-year file, and
#' combines the daily airport traffic summaries.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param region_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return A tibble with daily airport traffic summaries.
#' @export
prepare_apdf_daily_traffic_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    region_classifier = is_eurocontrol_airport
) {
  type <- base::match.arg(type)

  if (base::is.null(files)) {
    files <- check_zip_content(
      path = base::dirname(zipped_archive_path),
      archive = base::basename(zipped_archive_path)
    )$Name
  }

  purrr::map(
    files,
    function(file_name) {
      read_apdf_zip(
        zipped_archive_path = zipped_archive_path,
        files = file_name,
        type = type
      ) |>
        prepare_apdf_traffic_input() |>
        prepare_apdf_daily_traffic(
          year = year,
          region_classifier = region_classifier
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$AERODROME, .data$DATE)
}

#' Build a PBWG Airport Traffic File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' airport traffic products for a specific airport.
#'
#' @param airport ICAO location indicator.
#' @param years Integer or character vector of years.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_airport_traffic_filename <- function(airport, years, region = "EUR", ext = "csv") {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  stringr::str_c(
    stringr::str_c("PBWG", region, airport, "tfc", year_label, sep = "-"),
    ".",
    ext
  )
}

#' Write PBWG Airport Traffic Output
#'
#' Writes a PBWG airport traffic table for one airport to an explicit output
#' directory.
#'
#' @param data A tibble to write.
#' @param airport ICAO location indicator.
#' @param year Reporting year or year range encoded in the output file name.
#' @param output_dir Directory where the output file will be written.
#' @param region Region label added to the file name.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_airport_traffic <- function(data, airport, year, output_dir, region = "EUR") {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_airport_traffic_filename(
    airport = airport,
    years = year,
    region = region
  )
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data) |>
    dplyr::filter(.data$AERODROME %in% airport)

  readr::write_csv(output_data, output_path)

  invisible(output_path)
}

#' Create a Canonical Annual PBWG Airport Traffic File from an APDF Archive
#'
#' Processes a yearly APDF archive and writes canonical annual PBWG airport
#' traffic files, one per airport, to the requested output directory.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param year Reporting year for the canonical output file.
#' @param output_dir Directory where the annual file will be written.
#' @param airports Optional character vector of ICAO airport codes to keep. If
#'   `NULL`, all airports in the prepared summary are written.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param region Region label included in the output file name.
#' @param region_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return A named character vector of output file paths.
#' @export
create_pbwg_airport_traffic_annual_file <- function(
    zipped_archive_path,
    year,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    region = "EUR",
    region_classifier = is_eurocontrol_airport
) {
  summary_data <- prepare_apdf_daily_traffic_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    year = year,
    region_classifier = region_classifier
  )

  if (!base::is.null(airports)) {
    summary_data <- dplyr::filter(summary_data, .data$AERODROME %in% airports)
  }

  airports_to_write <- unique(summary_data$AERODROME)

  purrr::map_chr(
    airports_to_write,
    function(airport) {
      write_pbwg_airport_traffic(
        data = summary_data,
        airport = airport,
        year = year,
        output_dir = output_dir,
        region = region
      )
    }
  ) |>
    stats::setNames(airports_to_write)
}

#' Read PBWG Airport Traffic Files
#'
#' Reads one or more annual PBWG airport traffic files and returns a single
#' combined tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_airport_traffic_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Annual PBWG Airport Traffic Files into a Project Summary
#'
#' Reads canonical annual PBWG airport traffic files for one airport, combines
#' them into a multi-year summary file, and writes the derived product to the
#' requested output directory.
#'
#' @param airport ICAO location indicator.
#' @param years Integer or character vector of years to combine.
#' @param annual_dir Directory containing the canonical annual files.
#' @param output_dir Directory where the combined file will be written.
#' @param region Region label included in the file name.
#'
#' @return The output file path, invisibly.
#' @export
combine_pbwg_airport_traffic_years <- function(
    airport,
    years,
    annual_dir,
    output_dir = annual_dir,
    region = "EUR"
) {
  annual_paths <- purrr::map_chr(
    years,
    function(year) {
      fs::path(
        annual_dir,
        build_pbwg_airport_traffic_filename(
          airport = airport,
          years = year,
          region = region
        )
      )
    }
  )

  missing_paths <- annual_paths[!fs::file_exists(annual_paths)]

  if (base::length(missing_paths) > 0) {
    rlang::abort(
      stringr::str_c(
        "Missing annual PBWG airport traffic files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  combined_data <- read_pbwg_airport_traffic_files(annual_paths) |>
    dplyr::arrange(.data$AERODROME, .data$DATE)

  write_pbwg_airport_traffic(
    data = combined_data,
    airport = airport,
    year = years,
    output_dir = output_dir,
    region = region
  )
}

stop_if_apdf_columns_missing <- function(data, required_columns) {
  missing_columns <- base::setdiff(required_columns, names(data))

  if (base::length(missing_columns) > 0) {
    rlang::abort(
      stringr::str_c(
        "APDF data is missing required columns: ",
        stringr::str_flatten(missing_columns, ", ")
      )
    )
  }
}
