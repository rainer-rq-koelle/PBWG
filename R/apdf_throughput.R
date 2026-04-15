#' Calculate Airport Throughput
#'
#' Calculates airport throughput by time bin from prepared APDF data.
#'
#' @param apdf A prepared APDF tibble.
#' @param unit Time bin passed to [lubridate::floor_date()], for example
#'   `"hour"` or `"15 min"`.
#'
#' @return A tibble with airport, bin, arrivals, departures, and total flights.
#' @export
calc_throughput <- function(apdf, unit = "hour") {
  stop_if_apdf_columns_missing(apdf, required_columns = c("ICAO", "PHASE", "MVT_TIME"))
  assert_single_apdf_icao(apdf, what = "calc_throughput() input")

  tibble::as_tibble(apdf) |>
    dplyr::mutate(
      BIN = lubridate::floor_date(.data$MVT_TIME, unit = unit)
    ) |>
    dplyr::summarise(
      ARRS = sum(.data$PHASE %in% "ARR", na.rm = TRUE),
      DEPS = sum(.data$PHASE %in% "DEP", na.rm = TRUE),
      .by = c("ICAO", "BIN")
    ) |>
    dplyr::mutate(FLTS = .data$ARRS + .data$DEPS) |>
    dplyr::arrange(.data$ICAO, .data$BIN)
}

#' Prepare APDF Throughput Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares each airport-year file, computes
#' throughput bins, and combines the results.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter on the resulting bins.
#' @param unit Time bin passed to [lubridate::floor_date()].
#'
#' @return A tibble with airport throughput bins.
#' @export
prepare_apdf_throughput_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    unit = "hour"
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
      data <- read_apdf_zip(
        zipped_archive_path = zipped_archive_path,
        files = file_name,
        type = type
      ) |>
        prepare_apdf_traffic_input() |>
        calc_throughput(unit = unit)

      if (is.null(year)) {
        return(data)
      }

      dplyr::filter(data, lubridate::year(.data$BIN) == year)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$ICAO, .data$BIN)
}

#' Build a PBWG Throughput File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' throughput products. When `airport` is `NULL`, the file name is for the
#' project-level aggregate.
#'
#' @param years Integer or character vector of years.
#' @param airport Optional ICAO location indicator.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_throughput_filename <- function(years, airport = NULL, region = "EUR", ext = "csv") {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  pieces <- c("PBWG", region, airport, "thru-analytic", year_label)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]

  stringr::str_c(stringr::str_c(pieces, collapse = "-"), ".", ext)
}

#' Write PBWG Throughput Output
#'
#' Writes a PBWG throughput table either for one airport or for the project
#' aggregate to an explicit output directory.
#'
#' @param data A tibble to write.
#' @param year Reporting year or year range encoded in the output file name.
#' @param output_dir Directory where the output file will be written.
#' @param airport Optional ICAO location indicator. When `NULL`, the function
#'   writes the project aggregate.
#' @param region Region label added to the file name.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_throughput <- function(data, year, output_dir, airport = NULL, region = "EUR") {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_throughput_filename(
    years = year,
    airport = airport,
    region = region
  )
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data)

  if (!base::is.null(airport)) {
    output_data <- dplyr::filter(output_data, .data$ICAO %in% airport)
  }

  readr::write_csv(output_data, output_path)

  invisible(output_path)
}

#' Create Canonical Annual PBWG Throughput Files from an APDF Archive
#'
#' Processes a yearly APDF archive and writes canonical annual PBWG throughput
#' files, one per airport, to the requested output directory.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param year Reporting year for the canonical output files.
#' @param output_dir Directory where the annual files will be written.
#' @param airports Optional character vector of ICAO airport codes to keep. If
#'   `NULL`, all airports in the prepared summary are written.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param region Region label included in the output file names.
#' @param unit Time bin passed to [lubridate::floor_date()].
#'
#' @return A named character vector of output file paths.
#' @export
create_pbwg_throughput_annual_file <- function(
    zipped_archive_path,
    year,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    region = "EUR",
    unit = "hour"
) {
  summary_data <- prepare_apdf_throughput_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    year = year,
    unit = unit
  )

  if (!base::is.null(airports)) {
    summary_data <- dplyr::filter(summary_data, .data$ICAO %in% airports)
  }

  airports_to_write <- unique(summary_data$ICAO)

  purrr::map_chr(
    airports_to_write,
    function(airport) {
      write_pbwg_throughput(
        data = summary_data,
        year = year,
        output_dir = output_dir,
        airport = airport,
        region = region
      )
    }
  ) |>
    stats::setNames(airports_to_write)
}

#' Read PBWG Throughput Files
#'
#' Reads one or more PBWG throughput files and returns a single combined tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_throughput_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Canonical PBWG Throughput Files into a Project Summary
#'
#' Reads canonical annual PBWG throughput files for the requested airports and
#' years, combines them into a project-level summary file, and writes the
#' derived product to the requested output directory.
#'
#' @param airports Character vector of ICAO location indicators.
#' @param years Integer or character vector of years to combine.
#' @param annual_dir Directory containing the canonical annual files.
#' @param output_dir Directory where the combined file will be written.
#' @param region Region label included in the file name.
#' @param strict When `TRUE`, error if any requested airport-year file is
#'   missing. When `FALSE`, combine the files that are available.
#'
#' @return The output file path, invisibly.
#' @export
combine_pbwg_throughput_project <- function(
    airports,
    years,
    annual_dir,
    output_dir = annual_dir,
    region = "EUR",
    strict = FALSE
) {
  requested <- tidyr::expand_grid(AIRPORT = airports, YEAR = years)

  annual_paths <- purrr::pmap_chr(
    requested,
    function(AIRPORT, YEAR) {
      fs::path(
        annual_dir,
        build_pbwg_throughput_filename(
          years = YEAR,
          airport = AIRPORT,
          region = region
        )
      )
    }
  )

  missing_paths <- annual_paths[!fs::file_exists(annual_paths)]

  if (strict && base::length(missing_paths) > 0) {
    rlang::abort(
      stringr::str_c(
        "Missing annual PBWG throughput files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  available_paths <- annual_paths[fs::file_exists(annual_paths)]

  if (base::length(available_paths) == 0) {
    rlang::abort("No matching annual PBWG throughput files were found.")
  }

  combined_data <- read_pbwg_throughput_files(available_paths) |>
    dplyr::arrange(.data$ICAO, .data$BIN)

  write_pbwg_throughput(
    data = combined_data,
    year = years,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )
}
