#' List Available NM Flight Archives
#'
#' Lists files in a directory containing NM flight table extracts.
#'
#' @param path Directory containing NM flight table archives.
#' @param glob Optional glob pattern used to filter the listed files.
#'
#' @return A character vector of file paths.
#' @export
list_nm_flight_archives <- function(path, glob = "*.zip") {
  fs::dir_ls(path = path, glob = glob, type = "file")
}

#' Read NM Flight Data from a ZIP Archive
#'
#' Reads one or more files from a zipped NM flight table extract.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of files to extract from the archive.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#'
#' @return A tibble when one file is read, otherwise a named list of tibbles.
#' @export
read_nm_flights_zip <- function(zipped_archive_path, files = NULL, type = c("parquet", "csv", "csv_auto")) {
  type <- base::match.arg(type)

  read_zip_content(
    path = base::dirname(zipped_archive_path),
    archive = base::basename(zipped_archive_path),
    files = files,
    type = type
  )
}

#' Check Whether Airports Match the EUROCONTROL Member State Pattern
#'
#' @param icao_vec Character vector of ICAO airport indicators.
#' @param icao_pattern Regular expression defining the airport scope.
#'
#' @return A logical vector.
#' @export
is_ectrl_member_state_airport <- function(
    icao_vec,
    icao_pattern = NULL
) {
  is_eurocontrol_airport(icao_vec)
}

#' Prepare PBWG Regional Traffic from NM Flight Data
#'
#' Aggregates an NM flight table to daily PBWG-style regional traffic counts.
#'
#' @param nm_flights A decoded NM flight table tibble.
#' @param airport_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return A tibble with daily movement and wake-category summaries.
#' @export
prepare_nm_regional_traffic <- function(
    nm_flights,
    airport_classifier = is_ectrl_member_state_airport
) {
  stop_if_nm_columns_missing(
    nm_flights,
    required_columns = c("LOBT", "ADEP", "ADES", "WK_TBL_CAT")
  )

  tibble::as_tibble(nm_flights) |>
    dplyr::mutate(
      DATE = lubridate::date(.data$LOBT),
      ADEP_IN_REGION = airport_classifier(.data$ADEP),
      ADES_IN_REGION = airport_classifier(.data$ADES)
    ) |>
    dplyr::group_by(.data$DATE) |>
    dplyr::summarise(
      FLTS = dplyr::n(),
      D = sum(.data$ADEP_IN_REGION & !.data$ADES_IN_REGION, na.rm = TRUE),
      A = sum(!.data$ADEP_IN_REGION & .data$ADES_IN_REGION, na.rm = TRUE),
      I = sum(.data$ADEP_IN_REGION & .data$ADES_IN_REGION, na.rm = TRUE),
      O = sum(!.data$ADEP_IN_REGION & !.data$ADES_IN_REGION, na.rm = TRUE),
      J = sum(.data$WK_TBL_CAT %in% "J", na.rm = TRUE),
      H = sum(.data$WK_TBL_CAT %in% "H", na.rm = TRUE),
      M = sum(.data$WK_TBL_CAT %in% "M", na.rm = TRUE),
      L = sum(.data$WK_TBL_CAT %in% "L", na.rm = TRUE),
      NN = sum(is.na(.data$WK_TBL_CAT)),
      .groups = "drop"
    )
}

#' Prepare PBWG Regional Traffic Directly from an NM ZIP Archive
#'
#' Reads files from a zipped NM flight table archive, aggregates each file
#' separately, and combines the daily summaries. This avoids materialising the
#' full archive contents in memory before aggregation.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param airport_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return A tibble with daily movement and wake-category summaries.
#' @export
prepare_nm_regional_traffic_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    airport_classifier = is_ectrl_member_state_airport
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
      nm_flights <- read_nm_flights_zip(
        zipped_archive_path = zipped_archive_path,
        files = file_name,
        type = type
      )

      prepare_nm_regional_traffic(
        nm_flights = nm_flights,
        airport_classifier = airport_classifier
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$DATE) |>
    dplyr::summarise(
      dplyr::across(
        c("FLTS", "D", "A", "I", "O", "J", "H", "M", "L", "NN"),
        sum
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$DATE)
}

#' Write PBWG Network Traffic Output
#'
#' Writes a PBWG network traffic table to an explicit output directory. This
#' function does not write to package data folders.
#'
#' @param data A tibble to write.
#' @param year Reporting year encoded in the output file name.
#' @param output_dir Directory where the output file will be written.
#' @param region Optional region label added both to the file name and the
#'   output data.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_network_traffic <- function(data, year, output_dir, region = "EUR") {
  fs::dir_create(output_dir)
  output_name <- build_pbwg_network_traffic_filename(years = year, region = region)
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data)

  if (!base::is.null(region) && nzchar(region)) {
    output_data <- dplyr::mutate(output_data, REG = region)
  }

  readr::write_csv(output_data, file = output_path)

  invisible(output_path)
}

#' Build a PBWG Network Traffic File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' network traffic products.
#'
#' @param years Integer or character vector of years.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_network_traffic_filename <- function(years, region = "EUR", ext = "csv") {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  stringr::str_c(
    stringr::str_c("PBWG", region, "network-traffic", year_label, sep = "-"),
    ".",
    ext
  )
}

#' Create a Canonical Annual PBWG Network Traffic File from an NM Archive
#'
#' Processes a yearly NM archive and writes the canonical annual PBWG network
#' traffic file to the requested output directory.
#'
#' @param zipped_archive_path Full path to the NM ZIP archive.
#' @param year Reporting year for the canonical output file.
#' @param output_dir Directory where the annual file will be written.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param region Region label added to the output file name and output data.
#' @param airport_classifier Function that classifies ICAO aerodromes as inside
#'   or outside the target region.
#'
#' @return The output file path, invisibly.
#' @export
create_pbwg_network_traffic_annual_file <- function(
    zipped_archive_path,
    year,
    output_dir,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    region = "EUR",
    airport_classifier = is_ectrl_member_state_airport
) {
  summary_data <- prepare_nm_regional_traffic_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    airport_classifier = airport_classifier
  ) |>
    dplyr::filter(lubridate::year(.data$DATE) == year)

  write_pbwg_network_traffic(
    data = summary_data,
    year = year,
    output_dir = output_dir,
    region = region
  )
}

#' Read PBWG Network Traffic Files
#'
#' Reads one or more annual PBWG network traffic files and returns a single
#' combined tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_network_traffic_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Annual PBWG Network Traffic Files into a Project Summary
#'
#' Reads canonical annual PBWG network traffic files, combines them into a
#' multi-year summary file, and writes the derived product to the requested
#' output directory.
#'
#' @param years Integer or character vector of years to combine.
#' @param annual_dir Directory containing the canonical annual files.
#' @param output_dir Directory where the combined file will be written.
#' @param region Region label included in the file name and output data.
#'
#' @return The output file path, invisibly.
#' @export
combine_pbwg_network_traffic_years <- function(
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
        build_pbwg_network_traffic_filename(years = year, region = region)
      )
    }
  )

  missing_paths <- annual_paths[!fs::file_exists(annual_paths)]

  if (base::length(missing_paths) > 0) {
    rlang::abort(
      stringr::str_c(
        "Missing annual PBWG network traffic files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  combined_data <- read_pbwg_network_traffic_files(annual_paths) |>
    dplyr::arrange(.data$DATE)

  write_pbwg_network_traffic(
    data = combined_data,
    year = years,
    output_dir = output_dir,
    region = region
  )
}

stop_if_nm_columns_missing <- function(data, required_columns) {
  missing_columns <- base::setdiff(required_columns, names(data))

  if (base::length(missing_columns) > 0) {
    rlang::abort(
      stringr::str_c(
        "NM flight data is missing required columns: ",
        stringr::str_flatten(missing_columns, ", ")
      )
    )
  }
}
