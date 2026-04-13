#' APDF Taxi-Time Reference Fields
#'
#' Returns the default fields used for taxi-time reference preparation.
#'
#' @return A character vector of source field names.
#' @export
apdf_txxt_fields <- function() {
  c(
    "ADEP_ICAO", "ADES_ICAO",
    "AP_C_RWY", "AP_C_STND",
    "MVT_TIME_UTC", "BLOCK_TIME_UTC",
    "SRC_PHASE", "SRC_AIRPORT"
  )
}

#' Taxi-Time Reference Variants
#'
#' Returns the supported taxi-time reference variants and their descriptions.
#'
#' @return A tibble describing the available variants.
#' @export
txxt_reference_variants <- function() {
  tibble::tibble(
    REF_VARIANT = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    DESCRIPTION = c(
      "ICAO GANP algorithm using the 20th percentile",
      "PBWG algorithm using the average of the 5th and 15th percentiles"
    )
  )
}

#' Prepare APDF Data for Taxi-Time Reference Building
#'
#' Trims source fields, decodes them to harmonised names, and ensures an `ICAO`
#' column is present for airport-level reference generation.
#'
#' @param apdf An APDF tibble.
#' @param dictionary A tibble with `SOURCE_NAME` and `TARGET_NAME`.
#' @param fields Character vector of source fields to keep before decoding.
#'
#' @return A tibble.
#' @export
prepare_apdf_txxt_input <- function(
    apdf,
    dictionary = apdf_dictionary(),
    fields = apdf_txxt_fields()
) {
  apdf |>
    trim_apdf(fields = fields) |>
    decode_apdf(dictionary = dictionary) |>
    ensure_apdf_icao()
}

#' Prepare Taxi-Time Samples from APDF Data
#'
#' Extracts the movement fields needed for taxi-time reference calculation and
#' computes the observed taxi duration `TXXT`.
#'
#' @param txxt_input A prepared APDF tibble.
#' @param max_txxt Maximum taxi time in minutes kept as a candidate sample.
#'
#' @return A tibble containing sample-level taxi-time information.
#' @export
prepare_txxt_reference_input <- function(txxt_input, max_txxt = 120) {
  stop_if_apdf_columns_missing(
    txxt_input,
    required_columns = c("ICAO", "PHASE", "RWY", "STND", "MVT_TIME", "BLOCK_TIME")
  )

  tibble::as_tibble(txxt_input) |>
    dplyr::transmute(
      ICAO = .data$ICAO,
      PHASE = .data$PHASE,
      RWY = .data$RWY,
      STND = .data$STND,
      MVT_TIME = .data$MVT_TIME,
      BLOCK_TIME = .data$BLOCK_TIME,
      DIRECTION_OK = dplyr::case_when(
        .data$PHASE %in% "DEP" ~ .data$MVT_TIME > .data$BLOCK_TIME,
        .data$PHASE %in% "ARR" ~ .data$MVT_TIME < .data$BLOCK_TIME,
        .default = FALSE
      ),
      TXXT = dplyr::case_when(
        .data$PHASE %in% "DEP" ~ as.numeric(difftime(.data$MVT_TIME, .data$BLOCK_TIME, units = "mins")),
        .data$PHASE %in% "ARR" ~ as.numeric(difftime(.data$BLOCK_TIME, .data$MVT_TIME, units = "mins")),
        .default = NA_real_
      ),
      RWY_KNOWN = !is.na(.data$RWY) & nzchar(.data$RWY),
      STND_KNOWN = !is.na(.data$STND) & nzchar(.data$STND),
      EXTREME_TXXT = !is.na(.data$TXXT) & .data$TXXT > max_txxt,
      NONPOSITIVE_TXXT = !is.na(.data$TXXT) & .data$TXXT <= 0,
      VALID_TXXT = .data$DIRECTION_OK & !is.na(.data$TXXT) & .data$TXXT > 0 & .data$TXXT <= max_txxt
    ) |>
    dplyr::filter(.data$PHASE %in% c("ARR", "DEP"))
}

#' Summarise Taxi-Time Reference Input Quality
#'
#' Produces airport-phase level quality metrics for taxi-time reference samples.
#'
#' @param txxt_samples A taxi-time sample tibble from
#'   `prepare_txxt_reference_input()`.
#'
#' @return A tibble with quality indicators.
#' @export
summarise_txxt_reference_input_quality <- function(txxt_samples) {
  stop_if_apdf_columns_missing(
    txxt_samples,
    required_columns = c(
      "ICAO", "PHASE", "RWY_KNOWN", "STND_KNOWN", "VALID_TXXT",
      "DIRECTION_OK", "EXTREME_TXXT", "NONPOSITIVE_TXXT"
    )
  )

  tibble::as_tibble(txxt_samples) |>
    dplyr::summarise(
      N_TOTAL = dplyr::n(),
      N_VALID_TXXT = sum(.data$VALID_TXXT, na.rm = TRUE),
      N_DIRECTION_ISSUES = sum(!.data$DIRECTION_OK, na.rm = TRUE),
      N_NONPOSITIVE_TXXT = sum(.data$NONPOSITIVE_TXXT, na.rm = TRUE),
      N_EXTREME_TXXT = sum(.data$EXTREME_TXXT, na.rm = TRUE),
      N_UNKNOWN_RWY = sum(!.data$RWY_KNOWN, na.rm = TRUE),
      N_UNKNOWN_STND = sum(!.data$STND_KNOWN, na.rm = TRUE),
      .by = c("ICAO", "PHASE")
    ) |>
    dplyr::mutate(
      PCT_VALID_TXXT = .data$N_VALID_TXXT / .data$N_TOTAL,
      PCT_UNKNOWN_RWY = .data$N_UNKNOWN_RWY / .data$N_TOTAL,
      PCT_UNKNOWN_STND = .data$N_UNKNOWN_STND / .data$N_TOTAL
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE)
}

#' Build a Taxi-Time Reference Dataset
#'
#' Builds a taxi-time reference table by `ICAO`, `PHASE`, `RWY`, and `STND` for
#' a selected time window and algorithm variant.
#'
#' @param txxt_samples A taxi-time sample tibble from
#'   `prepare_txxt_reference_input()`.
#' @param ref_start Start timestamp of the reference window.
#' @param ref_end End timestamp of the reference window.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements to consider a sample valid.
#' @param keep_below_threshold When `TRUE`, keep groups below `min_n` and flag
#'   them as invalid.
#' @param include_unknown When `TRUE`, keep groups with unknown runway or stand.
#'
#' @return A tibble containing taxi-time reference values and metadata.
#' @export
build_txxt_reference <- function(
    txxt_samples,
    ref_start,
    ref_end,
    variant = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    min_n = 5,
    keep_below_threshold = TRUE,
    include_unknown = FALSE
) {
  variant <- base::match.arg(variant)

  stop_if_apdf_columns_missing(
    txxt_samples,
    required_columns = c("ICAO", "PHASE", "RWY", "STND", "BLOCK_TIME", "TXXT", "VALID_TXXT", "RWY_KNOWN", "STND_KNOWN")
  )

  filtered_samples <- tibble::as_tibble(txxt_samples) |>
    dplyr::filter(
      .data$BLOCK_TIME >= ref_start,
      .data$BLOCK_TIME <= ref_end,
      .data$VALID_TXXT
    )

  if (!include_unknown) {
    filtered_samples <- dplyr::filter(
      filtered_samples,
      .data$RWY_KNOWN,
      .data$STND_KNOWN
    )
  }

  reference <- filtered_samples |>
    dplyr::summarise(
      N = dplyr::n(),
      REF_TXXT = calc_txxt_reference_value(.data$TXXT, variant = variant),
      .by = c("ICAO", "PHASE", "RWY", "STND")
    ) |>
    dplyr::mutate(
      REF_START = as.POSIXct(ref_start, tz = lubridate::tz(ref_start)),
      REF_END = as.POSIXct(ref_end, tz = lubridate::tz(ref_end)),
      REF_PERIOD = build_reference_period_label(ref_start, ref_end),
      REF_VARIANT = variant,
      MIN_N = min_n,
      IS_VALID_SAMPLE = .data$N >= min_n
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RWY, .data$STND)

  if (!keep_below_threshold) {
    reference <- dplyr::filter(reference, .data$IS_VALID_SAMPLE)
  }

  reference
}

#' Check Taxi-Time Reference Coverage
#'
#' Summarises the coverage of a taxi-time reference dataset and optionally
#' compares it with the combinations needed for an analysis sample.
#'
#' @param reference_data A taxi-time reference tibble from
#'   `build_txxt_reference()`.
#' @param analysis_samples Optional taxi-time sample tibble for the analysis
#'   period.
#'
#' @return A tibble with coverage indicators.
#' @export
check_txxt_reference_coverage <- function(reference_data, analysis_samples = NULL) {
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c("ICAO", "PHASE", "RWY", "STND", "IS_VALID_SAMPLE")
  )

  coverage <- tibble::as_tibble(reference_data) |>
    dplyr::summarise(
      N_GROUPS = dplyr::n(),
      N_VALID_GROUPS = sum(.data$IS_VALID_SAMPLE, na.rm = TRUE),
      .by = c("ICAO", "PHASE")
    )

  if (is.null(analysis_samples)) {
    return(coverage)
  }

  needed <- tibble::as_tibble(analysis_samples) |>
    dplyr::distinct(.data$ICAO, .data$PHASE, .data$RWY, .data$STND) |>
    dplyr::left_join(
      dplyr::select(
        reference_data,
        "ICAO", "PHASE", "RWY", "STND", "IS_VALID_SAMPLE"
      ),
      by = c("ICAO", "PHASE", "RWY", "STND")
    ) |>
    dplyr::mutate(
      HAS_REFERENCE = !is.na(.data$IS_VALID_SAMPLE),
      HAS_VALID_REFERENCE = .data$IS_VALID_SAMPLE %in% TRUE
    ) |>
    dplyr::summarise(
      N_NEEDED_GROUPS = dplyr::n(),
      N_MATCHED_GROUPS = sum(.data$HAS_REFERENCE, na.rm = TRUE),
      N_VALID_MATCHED_GROUPS = sum(.data$HAS_VALID_REFERENCE, na.rm = TRUE),
      .by = c("ICAO", "PHASE")
    )

  dplyr::left_join(coverage, needed, by = c("ICAO", "PHASE"))
}

#' Build a PBWG Taxi-Time Reference File Name
#'
#' Builds a file name using the PBWG convention for taxi-time reference files.
#'
#' @param airport ICAO location indicator.
#' @param ref_period Reference year or year range.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements used in the validity rule.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_txxt_reference_filename <- function(
    airport,
    ref_period,
    variant,
    min_n,
    region = "EUR",
    ext = "csv"
) {
  stringr::str_c(
    stringr::str_c(
      "PBWG", region, airport, "ref-txxt", ref_period, variant, stringr::str_c("n", min_n, sep = ""),
      sep = "-"
    ),
    ".",
    ext
  )
}

#' Write PBWG Taxi-Time Reference Output
#'
#' Writes a PBWG taxi-time reference table for one airport.
#'
#' @param data A reference tibble to write.
#' @param airport ICAO location indicator.
#' @param ref_period Reference year or year range.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements used in the validity rule.
#' @param output_dir Directory where the output file will be written.
#' @param region Region label added to the file name.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_txxt_reference <- function(
    data,
    airport,
    ref_period,
    variant,
    min_n,
    output_dir,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_txxt_reference_filename(
    airport = airport,
    ref_period = ref_period,
    variant = variant,
    min_n = min_n,
    region = region
  )
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data) |>
    dplyr::filter(.data$ICAO %in% airport)

  readr::write_csv(output_data, output_path)

  invisible(output_path)
}

#' Create Canonical Annual PBWG Taxi-Time Reference Files from an APDF Archive
#'
#' Processes a reference-year APDF archive and writes canonical taxi-time
#' reference files, one per airport, to the requested output directory.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param ref_year Reference year used to build the reference dataset.
#' @param output_dir Directory where the reference files will be written.
#' @param airports Optional character vector of ICAO airport codes to keep.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements to consider a sample valid.
#' @param max_txxt Maximum taxi time in minutes kept as a candidate sample.
#' @param keep_below_threshold When `TRUE`, keep groups below `min_n` and flag
#'   them as invalid.
#' @param include_unknown When `TRUE`, keep groups with unknown runway or stand.
#' @param region Region label included in the output file names.
#'
#' @return A named character vector of output file paths.
#' @export
create_pbwg_txxt_reference_annual_file <- function(
    zipped_archive_path,
    ref_year,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    variant = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    min_n = 5,
    max_txxt = 120,
    keep_below_threshold = TRUE,
    include_unknown = FALSE,
    region = "EUR"
) {
  variant <- base::match.arg(variant)
  type <- base::match.arg(type)

  txxt_samples <- prepare_apdf_txxt_reference_input_from_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    max_txxt = max_txxt
  )

  if (!base::is.null(airports)) {
    txxt_samples <- dplyr::filter(txxt_samples, .data$ICAO %in% airports)
  }

  ref_start <- lubridate::ymd_hms(stringr::str_c(ref_year, "-01-01 00:00:00"), tz = "UTC")
  ref_end <- lubridate::ymd_hms(stringr::str_c(ref_year, "-12-31 23:59:59"), tz = "UTC")

  reference <- build_txxt_reference(
    txxt_samples = txxt_samples,
    ref_start = ref_start,
    ref_end = ref_end,
    variant = variant,
    min_n = min_n,
    keep_below_threshold = keep_below_threshold,
    include_unknown = include_unknown
  )

  airports_to_write <- unique(reference$ICAO)

  purrr::map_chr(
    airports_to_write,
    function(airport) {
      write_pbwg_txxt_reference(
        data = reference,
        airport = airport,
        ref_period = as.character(ref_year),
        variant = variant,
        min_n = min_n,
        output_dir = output_dir,
        region = region
      )
    }
  ) |>
    stats::setNames(airports_to_write)
}

#' Apply Taxi-Time Reference Data to APDF Taxi Samples
#'
#' Joins taxi-time reference values to movement-level taxi samples and computes
#' additional taxi time as actual observed taxi time minus the chosen reference
#' time for each `ICAO` / `PHASE` / `RWY` / `STND` combination.
#'
#' @param txxt_samples A taxi-time sample tibble from
#'   `prepare_txxt_reference_input()`.
#' @param reference_data A taxi-time reference tibble from
#'   `build_txxt_reference()`.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#'
#' @return A movement-level tibble with joined reference values and additional
#'   taxi time.
#' @export
apply_txxt_reference <- function(
    txxt_samples,
    reference_data,
    valid_reference_only = TRUE
) {
  stop_if_apdf_columns_missing(
    txxt_samples,
    required_columns = c(
      "ICAO", "PHASE", "RWY", "STND", "BLOCK_TIME", "TXXT", "VALID_TXXT"
    )
  )
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c(
      "ICAO", "PHASE", "RWY", "STND", "REF_TXXT", "REF_VARIANT",
      "REF_PERIOD", "MIN_N", "IS_VALID_SAMPLE"
    )
  )

  reference_lookup <- tibble::as_tibble(reference_data)

  if (valid_reference_only) {
    reference_lookup <- dplyr::filter(reference_lookup, .data$IS_VALID_SAMPLE)
  }

  tibble::as_tibble(txxt_samples) |>
    dplyr::left_join(
      dplyr::select(
        reference_lookup,
        "ICAO", "PHASE", "RWY", "STND",
        "REF_TXXT", "REF_VARIANT", "REF_PERIOD", "MIN_N", "IS_VALID_SAMPLE"
      ),
      by = c("ICAO", "PHASE", "RWY", "STND")
    ) |>
    dplyr::mutate(
      DATE = lubridate::date(.data$BLOCK_TIME),
      HAS_REFERENCE = !is.na(.data$REF_TXXT),
      TX_NA = !.data$HAS_REFERENCE,
      ADD_TXXT = dplyr::if_else(
        .data$VALID_TXXT & .data$HAS_REFERENCE,
        .data$TXXT - .data$REF_TXXT,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$BLOCK_TIME)
}

#' Summarise Daily Additional Taxi Time
#'
#' Aggregates augmented taxi samples to the daily PBWG output format used for
#' taxi-in and taxi-out additional time analyses.
#'
#' @param augmented_txxt A tibble from `apply_txxt_reference()`.
#' @param year Optional reporting year filter.
#' @param valid_only When `TRUE`, only movements with valid taxi times are
#'   counted.
#'
#' @return A tibble with daily taxi-time summary metrics.
#' @export
summarise_pbwg_txxt_daily <- function(
    augmented_txxt,
    year = NULL,
    valid_only = TRUE
) {
  stop_if_apdf_columns_missing(
    augmented_txxt,
    required_columns = c(
      "ICAO", "PHASE", "DATE", "TXXT", "REF_TXXT", "ADD_TXXT",
      "TX_NA", "VALID_TXXT"
    )
  )

  summary_input <- tibble::as_tibble(augmented_txxt)

  if (valid_only) {
    summary_input <- dplyr::filter(summary_input, .data$VALID_TXXT)
  }

  daily_summary <- summary_input |>
    dplyr::summarise(
      MVTS = dplyr::n(),
      TOT_REF = sum(.data$REF_TXXT, na.rm = TRUE),
      TOT_ADD_TIME = sum(.data$ADD_TXXT, na.rm = TRUE),
      TX_NA = sum(.data$TX_NA, na.rm = TRUE),
      .by = c("ICAO", "PHASE", "DATE")
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$DATE)

  if (is.null(year)) {
    return(daily_summary)
  }

  dplyr::filter(daily_summary, lubridate::year(.data$DATE) == year)
}

#' Prepare Augmented APDF Taxi Data Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares taxi samples, and joins a chosen
#' taxi-time reference dataset to produce movement-level augmented taxi data.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param reference_data A taxi-time reference tibble from
#'   `build_txxt_reference()`.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param max_txxt Maximum taxi time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#'
#' @return A movement-level tibble with reference values and additional taxi
#'   time.
#' @export
prepare_apdf_txxt_augmented_zip <- function(
    zipped_archive_path,
    reference_data,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    max_txxt = 120,
    valid_reference_only = TRUE
) {
  type <- base::match.arg(type)

  if (base::is.null(files)) {
    files <- check_zip_content(
      path = base::dirname(zipped_archive_path),
      archive = base::basename(zipped_archive_path)
    )$Name
  }

  augmented <- purrr::map(
    files,
    function(file_name) {
      read_apdf_zip(
        zipped_archive_path = zipped_archive_path,
        files = file_name,
        type = type
      ) |>
        prepare_apdf_txxt_input() |>
        prepare_txxt_reference_input(max_txxt = max_txxt) |>
        apply_txxt_reference(
          reference_data = reference_data,
          valid_reference_only = valid_reference_only
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$BLOCK_TIME)

  if (is.null(year)) {
    return(augmented)
  }

  dplyr::filter(augmented, lubridate::year(.data$DATE) == year)
}

#' Prepare Daily Additional Taxi Time Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares taxi samples, applies a chosen
#' taxi-time reference dataset, and returns daily taxi-time summary metrics.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param reference_data A taxi-time reference tibble from
#'   `build_txxt_reference()`.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param max_txxt Maximum taxi time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#' @param valid_only When `TRUE`, only movements with valid taxi times are
#'   counted in the daily summary.
#'
#' @return A tibble with daily taxi-time summary metrics.
#' @export
prepare_apdf_txxt_daily_zip <- function(
    zipped_archive_path,
    reference_data,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    max_txxt = 120,
    valid_reference_only = TRUE,
    valid_only = TRUE
) {
  prepare_apdf_txxt_augmented_zip(
    zipped_archive_path = zipped_archive_path,
    reference_data = reference_data,
    files = files,
    type = type,
    year = year,
    max_txxt = max_txxt,
    valid_reference_only = valid_reference_only
  ) |>
    summarise_pbwg_txxt_daily(year = year, valid_only = valid_only)
}

#' Build a PBWG Daily Taxi-Time File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' additional taxi-time products. When `airport` is `NULL`, the file name is
#' for the project-level aggregate.
#'
#' @param years Integer or character vector of years.
#' @param ref_period Reference year or year range.
#' @param variant Reference algorithm variant.
#' @param airport Optional ICAO location indicator.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_txxt_filename <- function(
    years,
    ref_period,
    variant,
    airport = NULL,
    region = "EUR",
    ext = "csv"
) {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  pieces <- c("PBWG", region, airport, "txxt-analytic", year_label, stringr::str_c("ref", ref_period), variant)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]

  stringr::str_c(stringr::str_c(pieces, collapse = "-"), ".", ext)
}

#' Build a PBWG Augmented Taxi-Time File Name
#'
#' Builds a file name for the movement-level augmented taxi dataset used for
#' verification and troubleshooting.
#'
#' @param years Integer or character vector of years.
#' @param airport ICAO location indicator.
#' @param ref_period Reference year or year range.
#' @param variant Reference algorithm variant.
#' @param region Region label included in the file name.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_txxt_augmented_filename <- function(
    years,
    airport,
    ref_period,
    variant,
    region = "EUR",
    ext = "csv"
) {
  year_label <- if (base::length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  stringr::str_c(
    stringr::str_c(
      "PBWG", region, airport, "txxt-augmented", year_label,
      stringr::str_c("ref", ref_period), variant,
      sep = "-"
    ),
    ".",
    ext
  )
}

#' Write PBWG Daily Additional Taxi-Time Output
#'
#' Writes a PBWG taxi-time summary either for one airport or for the project
#' aggregate to an explicit output directory.
#'
#' @param data A tibble to write.
#' @param year Reporting year or year range encoded in the output file name.
#' @param ref_period Reference year or year range encoded in the output file
#'   name.
#' @param variant Reference algorithm variant encoded in the output file name.
#' @param output_dir Directory where the output file will be written.
#' @param airport Optional ICAO location indicator. When `NULL`, the function
#'   writes the project aggregate.
#' @param region Region label added to the file name.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_txxt <- function(
    data,
    year,
    ref_period,
    variant,
    output_dir,
    airport = NULL,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_txxt_filename(
    years = year,
    ref_period = ref_period,
    variant = variant,
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

#' Write PBWG Augmented Taxi-Time Output
#'
#' Writes the movement-level augmented taxi dataset for one airport to an
#' explicit output directory.
#'
#' @param data A tibble to write.
#' @param year Reporting year or year range encoded in the output file name.
#' @param airport ICAO location indicator.
#' @param ref_period Reference year or year range encoded in the output file
#'   name.
#' @param variant Reference algorithm variant encoded in the output file name.
#' @param output_dir Directory where the output file will be written.
#' @param region Region label added to the file name.
#'
#' @return The output file path, invisibly.
#' @export
write_pbwg_txxt_augmented <- function(
    data,
    year,
    airport,
    ref_period,
    variant,
    output_dir,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_txxt_augmented_filename(
    years = year,
    airport = airport,
    ref_period = ref_period,
    variant = variant,
    region = region
  )
  output_path <- fs::path(output_dir, output_name)

  output_data <- tibble::as_tibble(data) |>
    dplyr::filter(.data$ICAO %in% airport)

  readr::write_csv(output_data, output_path)

  invisible(output_path)
}

#' Create Canonical Annual PBWG Daily Taxi-Time Files from an APDF Archive
#'
#' Processes a yearly APDF archive, applies a chosen taxi-time reference
#' dataset, and writes canonical annual PBWG daily taxi-time files, one per
#' airport, to the requested output directory. Optionally, the movement-level
#' augmented taxi data can be written as well for verification purposes.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param year Reporting year for the canonical output files.
#' @param reference_data A taxi-time reference tibble from
#'   `build_txxt_reference()`.
#' @param output_dir Directory where the daily output files will be written.
#' @param airports Optional character vector of ICAO airport codes to keep. If
#'   `NULL`, all airports in the prepared summary are written.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param region Region label included in the output file names.
#' @param max_txxt Maximum taxi time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#' @param valid_only When `TRUE`, only movements with valid taxi times are
#'   counted in the daily summary.
#' @param save_augmented When `TRUE`, write the augmented movement-level taxi
#'   data per airport.
#' @param augmented_dir Directory where augmented movement-level files will be
#'   written. Defaults to `output_dir`.
#'
#' @return A named list with `daily_paths` and `augmented_paths`.
#' @export
create_pbwg_txxt_annual_file <- function(
    zipped_archive_path,
    year,
    reference_data,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    region = "EUR",
    max_txxt = 120,
    valid_reference_only = TRUE,
    valid_only = TRUE,
    save_augmented = FALSE,
    augmented_dir = output_dir
) {
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c("REF_PERIOD", "REF_VARIANT")
  )

  augmented <- prepare_apdf_txxt_augmented_zip(
    zipped_archive_path = zipped_archive_path,
    reference_data = reference_data,
    files = files,
    type = type,
    year = year,
    max_txxt = max_txxt,
    valid_reference_only = valid_reference_only
  )

  if (!base::is.null(airports)) {
    augmented <- dplyr::filter(augmented, .data$ICAO %in% airports)
  }

  summary_data <- summarise_pbwg_txxt_daily(
    augmented_txxt = augmented,
    year = year,
    valid_only = valid_only
  )

  airports_to_write <- unique(summary_data$ICAO)

  ref_period <- unique(reference_data$REF_PERIOD)
  ref_variant <- unique(reference_data$REF_VARIANT)

  if (base::length(ref_period) != 1 || base::length(ref_variant) != 1) {
    rlang::abort("Reference data must contain exactly one REF_PERIOD and one REF_VARIANT.")
  }

  daily_paths <- purrr::map_chr(
    airports_to_write,
    function(airport) {
      write_pbwg_txxt(
        data = summary_data,
        year = year,
        ref_period = ref_period,
        variant = ref_variant,
        output_dir = output_dir,
        airport = airport,
        region = region
      )
    }
  ) |>
    stats::setNames(airports_to_write)

  augmented_paths <- character(0)

  if (save_augmented) {
    augmented_paths <- purrr::map_chr(
      airports_to_write,
      function(airport) {
        write_pbwg_txxt_augmented(
          data = augmented,
          year = year,
          airport = airport,
          ref_period = ref_period,
          variant = ref_variant,
          output_dir = augmented_dir,
          region = region
        )
      }
    ) |>
      stats::setNames(airports_to_write)
  }

  list(
    daily_paths = daily_paths,
    augmented_paths = augmented_paths
  )
}

#' Read PBWG Daily Taxi-Time Files
#'
#' Reads one or more PBWG daily taxi-time files and returns a single combined
#' tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_txxt_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Canonical PBWG Daily Taxi-Time Files into a Project Summary
#'
#' Reads canonical annual PBWG daily taxi-time files for the requested airports
#' and years, combines them into a project-level summary file, and writes the
#' derived product to the requested output directory.
#'
#' @param airports Character vector of ICAO location indicators.
#' @param years Integer or character vector of years to combine.
#' @param annual_dir Directory containing the canonical annual files.
#' @param ref_period Reference year or year range encoded in the file names.
#' @param variant Reference algorithm variant encoded in the file names.
#' @param output_dir Directory where the combined file will be written.
#' @param region Region label included in the file name.
#' @param strict When `TRUE`, error if any requested airport-year file is
#'   missing. When `FALSE`, combine the files that are available.
#'
#' @return The output file path, invisibly.
#' @export
combine_pbwg_txxt_project <- function(
    airports,
    years,
    annual_dir,
    ref_period,
    variant,
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
        build_pbwg_txxt_filename(
          years = YEAR,
          airport = AIRPORT,
          ref_period = ref_period,
          variant = variant,
          region = region
        )
      )
    }
  )

  existing_paths <- annual_paths[fs::file_exists(annual_paths)]
  missing_paths <- annual_paths[!fs::file_exists(annual_paths)]

  if (strict && base::length(missing_paths) > 0) {
    rlang::abort(
      stringr::str_c(
        "Missing annual PBWG daily taxi-time files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  if (base::length(existing_paths) == 0) {
    rlang::abort("No annual PBWG daily taxi-time files were found to combine.")
  }

  combined_data <- read_pbwg_txxt_files(existing_paths) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$DATE)

  write_pbwg_txxt(
    data = combined_data,
    year = years,
    ref_period = ref_period,
    variant = variant,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )
}

prepare_apdf_txxt_reference_input_from_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    max_txxt = 180
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
        prepare_apdf_txxt_input() |>
        prepare_txxt_reference_input(max_txxt = max_txxt)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RWY, .data$STND, .data$BLOCK_TIME)
}

ensure_apdf_icao <- function(data) {
  if ("ICAO" %in% names(data)) {
    return(tibble::as_tibble(data))
  }

  if ("AERODROME" %in% names(data)) {
    return(dplyr::rename(tibble::as_tibble(data), ICAO = "AERODROME"))
  }

  rlang::abort("APDF data must contain either ICAO or AERODROME.")
}

calc_txxt_reference_value <- function(txxt, variant) {
  switch(
    variant,
    icao_ganp_p20 = as.numeric(stats::quantile(txxt, probs = 0.20, names = FALSE, na.rm = TRUE)),
    pbwg_avg_p05_p15 = {
      q05 <- as.numeric(stats::quantile(txxt, probs = 0.05, names = FALSE, na.rm = TRUE))
      q15 <- as.numeric(stats::quantile(txxt, probs = 0.15, names = FALSE, na.rm = TRUE))
      (q05 + q15) / 2
    }
  )
}

build_reference_period_label <- function(ref_start, ref_end) {
  start_year <- lubridate::year(ref_start)
  end_year <- lubridate::year(ref_end)

  if (start_year == end_year) {
    return(as.character(start_year))
  }

  stringr::str_c(start_year, end_year, sep = "-")
}
