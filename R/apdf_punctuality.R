#' Default PBWG Punctuality Delay Groups
#'
#' Returns the default ordered delay-group labels used for APDF punctuality
#' outputs. The default setup reproduces the established 5-minute binning with
#' tails below `-60` and above `60`.
#'
#' @param positive_upper Upper bound for the positive explicit bins.
#' @param negative_lower Lower bound for the negative explicit bins.
#' @param step Bin width in minutes.
#'
#' @return A character vector of ordered delay-group labels.
#' @export
pbwg_punctuality_groups <- function(
    positive_upper = 60,
    negative_lower = -60,
    step = 5
) {
  stop_if_invalid_delay_grid(
    positive_upper = positive_upper,
    negative_lower = negative_lower,
    step = step
  )

  negative_breaks <- seq(negative_lower, 0, by = step)
  positive_breaks <- seq(step, positive_upper, by = step)

  negative_labels <- c(
    stringr::str_c("(-INF,", negative_lower, "]"),
    purrr::map_chr(
      seq_len(length(negative_breaks) - 1),
      function(i) {
        stringr::str_c(
          "(",
          negative_breaks[i],
          ",",
          negative_breaks[i + 1],
          "]"
        )
      }
    )
  )

  positive_labels <- purrr::map_chr(
    positive_breaks,
    function(upper) {
      lower <- upper - step

      if (upper == step) {
        return(stringr::str_c("(", lower, ",", upper, ")"))
      }

      if (upper == positive_upper) {
        return(stringr::str_c("[", lower, ",", upper, ")"))
      }

      stringr::str_c("[", lower, ",", upper, ")")
    }
  )

  c(
    negative_labels,
    positive_labels,
    stringr::str_c("[", positive_upper, ",INF)")
  )
}

#' Add Block Delay and Delay Group to APDF Data
#'
#' Computes block delay in minutes as `BLOCK_TIME - SCHED_TIME` and assigns each
#' record to a configured delay group.
#'
#' @param apdf A prepared APDF tibble.
#' @param positive_upper Upper bound for the positive explicit bins.
#' @param negative_lower Lower bound for the negative explicit bins.
#' @param step Bin width in minutes.
#'
#' @return A tibble with `BLOCK_DLY` and `DLY_GRP`.
#' @export
add_delay_and_dlygrp <- function(
    apdf,
    positive_upper = 60,
    negative_lower = -60,
    step = 5
) {
  stop_if_apdf_columns_missing(
    apdf,
    required_columns = c("BLOCK_TIME", "SCHED_TIME")
  )

  tibble::as_tibble(apdf) |>
    dplyr::mutate(
      BLOCK_DLY = as.numeric(difftime(.data$BLOCK_TIME, .data$SCHED_TIME, units = "mins")),
      DLY_GRP = assign_delay_group(
        block_dly = .data$BLOCK_DLY,
        positive_upper = positive_upper,
        negative_lower = negative_lower,
        step = step
      )
    )
}

#' Package PBWG Punctuality Analytics
#'
#' Aggregates APDF data with delay groups to daily airport-phase punctuality
#' distributions using ordered delay-group columns.
#'
#' @param punc_with_dly A tibble containing `AERODROME`, `BLOCK_TIME`, `PHASE`,
#'   and `DLY_GRP`.
#' @param group_labels Ordered delay-group labels. Defaults to the output of
#'   `pbwg_punctuality_groups()`.
#'
#' @return A tibble in wide punctuality format.
#' @export
package_pbwg_punctuality <- function(
    punc_with_dly,
    group_labels = pbwg_punctuality_groups()
) {
  stop_if_apdf_columns_missing(
    punc_with_dly,
    required_columns = c("AERODROME", "BLOCK_TIME", "PHASE", "DLY_GRP")
  )

  tibble::as_tibble(punc_with_dly) |>
    dplyr::mutate(
      DATE = lubridate::date(.data$BLOCK_TIME),
      COUNT = 1L,
      DLY_GRP = factor(.data$DLY_GRP, levels = group_labels)
    ) |>
    dplyr::group_by(.data$AERODROME, .data$DATE, .data$PHASE, .data$DLY_GRP) |>
    dplyr::summarise(COUNT = sum(.data$COUNT), .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols = c("AERODROME", "DATE", "PHASE"),
      names_from = "DLY_GRP",
      values_from = "COUNT",
      values_fill = 0
    ) |>
    ensure_punctuality_columns(group_labels = group_labels) |>
    dplyr::mutate(
      N_VALID = rowSums(dplyr::pick(dplyr::all_of(group_labels))),
      .after = "PHASE"
    )
}

#' Prepare APDF Punctuality Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares each airport-year file, computes
#' block delay groups, and combines the punctuality summaries.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param positive_upper Upper bound for the positive explicit bins.
#' @param negative_lower Lower bound for the negative explicit bins.
#' @param step Bin width in minutes.
#'
#' @return A tibble in wide punctuality format.
#' @export
prepare_apdf_punctuality_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    positive_upper = 60,
    negative_lower = -60,
    step = 5
) {
  type <- base::match.arg(type)
  group_labels <- pbwg_punctuality_groups(
    positive_upper = positive_upper,
    negative_lower = negative_lower,
    step = step
  )

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
        add_delay_and_dlygrp(
          positive_upper = positive_upper,
          negative_lower = negative_lower,
          step = step
        ) |>
        package_pbwg_punctuality(group_labels = group_labels)

      if (is.null(year)) {
        return(data)
      }

      dplyr::filter(data, lubridate::year(.data$DATE) == year)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$AERODROME, .data$DATE, .data$PHASE)
}

#' Build a PBWG Punctuality File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' punctuality products. When `airport` is `NULL`, the file name is for the
#' project-level aggregate.
#'
#' @param years Integer or character vector of years.
#' @param airport Optional ICAO location indicator.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_punctuality_filename <- function(years, airport = NULL, region = "EUR", ext = "csv") {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  pieces <- c("PBWG", region, airport, "punc", year_label)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]

  stringr::str_c(stringr::str_c(pieces, collapse = "-"), ".", ext)
}

#' Write PBWG Punctuality Output
#'
#' Writes a PBWG punctuality table either for one airport or for the project
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
write_pbwg_punctuality <- function(data, year, output_dir, airport = NULL, region = "EUR") {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_punctuality_filename(
    years = year,
    airport = airport,
    region = region
  )
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data)

  if (!base::is.null(airport)) {
    output_data <- dplyr::filter(output_data, .data$AERODROME %in% airport)
  }

  output_data <- dplyr::rename(output_data, ICAO = "AERODROME")
  readr::write_csv(output_data, output_path)

  invisible(output_path)
}

#' Create Canonical Annual PBWG Punctuality Files from an APDF Archive
#'
#' Processes a yearly APDF archive and writes canonical annual PBWG punctuality
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
#' @param positive_upper Upper bound for the positive explicit bins.
#' @param negative_lower Lower bound for the negative explicit bins.
#' @param step Bin width in minutes.
#'
#' @return A named character vector of output file paths.
#' @export
create_pbwg_punctuality_annual_file <- function(
    zipped_archive_path,
    year,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    region = "EUR",
    positive_upper = 60,
    negative_lower = -60,
    step = 5
) {
  summary_data <- prepare_apdf_punctuality_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    year = year,
    positive_upper = positive_upper,
    negative_lower = negative_lower,
    step = step
  )

  if (!base::is.null(airports)) {
    summary_data <- dplyr::filter(summary_data, .data$AERODROME %in% airports)
  }

  airports_to_write <- unique(summary_data$AERODROME)

  purrr::map_chr(
    airports_to_write,
    function(airport) {
      write_pbwg_punctuality(
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

#' Read PBWG Punctuality Files
#'
#' Reads one or more PBWG punctuality files and returns a single combined tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_punctuality_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Canonical PBWG Punctuality Files into a Project Summary
#'
#' Reads canonical annual PBWG punctuality files for the requested airports and
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
combine_pbwg_punctuality_project <- function(
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
        build_pbwg_punctuality_filename(
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
        "Missing annual PBWG punctuality files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  available_paths <- annual_paths[fs::file_exists(annual_paths)]

  if (base::length(available_paths) == 0) {
    rlang::abort("No matching annual PBWG punctuality files were found.")
  }

  combined_data <- read_pbwg_punctuality_files(available_paths) |>
    dplyr::arrange(.data$ICAO, .data$DATE, .data$PHASE)

  write_pbwg_punctuality(
    data = dplyr::rename(combined_data, AERODROME = "ICAO"),
    year = years,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )
}

assign_delay_group <- function(
    block_dly,
    positive_upper,
    negative_lower,
    step
) {
  labels <- pbwg_punctuality_groups(
    positive_upper = positive_upper,
    negative_lower = negative_lower,
    step = step
  )

  purrr::map_chr(
    block_dly,
    function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }

      if (x <= negative_lower) {
        return(labels[1])
      }

      if (x <= 0) {
        upper <- step * ceiling(x / step)
        lower <- upper - step
        return(stringr::str_c("(", lower, ",", upper, "]"))
      }

      if (x <= positive_upper) {
        if (x == step) {
          lower <- 0
          upper <- step
        } else if ((x %% step) == 0) {
          lower <- x
          upper <- x + step
        } else {
          upper <- step * ceiling(x / step)
          lower <- upper - step
        }

        if (upper == step) {
          return(stringr::str_c("(", lower, ",", upper, ")"))
        }

        if (lower >= positive_upper) {
          return(stringr::str_c("[", positive_upper, ",INF)"))
        }

        return(stringr::str_c("[", lower, ",", upper, ")"))
      }

      stringr::str_c("[", positive_upper, ",INF)")
    }
  )
}

ensure_punctuality_columns <- function(data, group_labels) {
  missing_columns <- setdiff(group_labels, names(data))

  if (length(missing_columns) > 0) {
    data[missing_columns] <- 0L
  }

  dplyr::select(data, dplyr::all_of(c("AERODROME", "DATE", "PHASE", group_labels)))
}

stop_if_invalid_delay_grid <- function(positive_upper, negative_lower, step) {
  if (step <= 0) {
    rlang::abort("step must be a positive number of minutes.")
  }

  if ((positive_upper %% step) != 0 || (abs(negative_lower) %% step) != 0) {
    rlang::abort("positive_upper and abs(negative_lower) must be divisible by step.")
  }
}
