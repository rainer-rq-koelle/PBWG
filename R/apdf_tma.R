#' APDF Terminal-Airspace Reference Fields
#'
#' Returns the default fields used for terminal-airspace reference preparation.
#'
#' @return A character vector of source field names.
#' @export
apdf_tma_fields <- function() {
  c(
    "AP_C_FLTID",
    "ADEP_ICAO", "ADES_ICAO",
    "AC_CLASS", "AP_C_RWY",
    "MVT_TIME_UTC",
    "C40_CROSS_TIME", "C40_BEARING",
    "C100_CROSS_TIME", "C100_BEARING",
    "SRC_PHASE"
  )
}

#' Terminal-Airspace Reference Variants
#'
#' Returns the supported reference variants for additional time in terminal
#' airspace. The same variants apply to arrivals (`ASMA`) and departures
#' (`DSMA`), with `PHASE` distinguishing the operational context.
#'
#' @return A tibble describing the available variants.
#' @export
tma_reference_variants <- function() {
  tibble::tibble(
    REF_VARIANT = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    DESCRIPTION = c(
      "ICAO GANP algorithm using the 20th percentile",
      "PBWG algorithm using the average of the 5th and 15th percentiles"
    )
  )
}

#' Prepare APDF Data for Terminal-Airspace Reference Building
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
prepare_apdf_tma_input <- function(
    apdf,
    dictionary = apdf_dictionary(),
    fields = apdf_tma_fields()
) {
  apdf |>
    trim_apdf(fields = fields) |>
    decode_apdf(dictionary = dictionary) |>
    derive_apdf_icao()
}

#' Prepare Terminal-Airspace Samples from APDF Data
#'
#' Extracts the movement fields needed for terminal-airspace reference
#' calculation and computes the observed travel time between the movement event
#' and the configured terminal-airspace range crossing.
#'
#' `PHASE` differentiates arrival (`ASMA`) and departure (`DSMA`) behaviour.
#'
#' @param tma_input A prepared APDF tibble.
#' @param ranges Numeric vector of ranges in nautical miles to keep. Supported
#'   values currently are `40` and `100`.
#' @param max_tma Maximum travel time in minutes kept as a candidate sample.
#'
#' @return A tibble containing sample-level terminal-airspace information.
#' @export
prepare_tma_reference_input <- function(
    tma_input,
    ranges = c(40, 100),
    max_tma = 180
) {
  stop_if_apdf_columns_missing(
    tma_input,
    required_columns = c(
      "ICAO", "PHASE", "CLASS", "RWY", "MVT_TIME",
      "C40_CROSS_TIME", "C40_BEARING", "C100_CROSS_TIME", "C100_BEARING"
    )
  )

  crossing_spec <- build_tma_crossing_spec(ranges = ranges)
  sample_input <- tibble::as_tibble(tma_input)

  for (optional_col in c("FLTID", "ADEP", "ADES")) {
    if (!optional_col %in% names(sample_input)) {
      sample_input[[optional_col]] <- NA_character_
    }
  }

  purrr::pmap_dfr(
    crossing_spec,
    function(RANGE_NM, CROSS_TIME_COL, BEARING_COL) {
      dplyr::transmute(
        sample_input,
        FLTID = .data$FLTID %||% NA_character_,
        ICAO = .data$ICAO,
        PHASE = .data$PHASE,
        ADEP = .data$ADEP %||% NA_character_,
        ADES = .data$ADES %||% NA_character_,
        CLASS_RAW = .data$CLASS,
        CLASS = normalise_pbwg_aircraft_class(.data$CLASS),
        RWY = as.character(.data$RWY),
        MVT_TIME = .data$MVT_TIME,
        RANGE_NM = RANGE_NM,
        CROSS_TIME = .data[[CROSS_TIME_COL]],
        BEARING = .data[[BEARING_COL]]
      )
    }
  ) |>
    dplyr::mutate(
      BEARING = dplyr::if_else(is.na(.data$BEARING), NA_real_, .data$BEARING %% 360),
      DIRECTION_OK = dplyr::case_when(
        .data$PHASE %in% "DEP" ~ .data$CROSS_TIME > .data$MVT_TIME,
        .data$PHASE %in% "ARR" ~ .data$MVT_TIME > .data$CROSS_TIME,
        .default = FALSE
      ),
      TMA_TIME = dplyr::case_when(
        .data$PHASE %in% "DEP" ~ as.numeric(difftime(.data$CROSS_TIME, .data$MVT_TIME, units = "mins")),
        .data$PHASE %in% "ARR" ~ as.numeric(difftime(.data$MVT_TIME, .data$CROSS_TIME, units = "mins")),
        .default = NA_real_
      ),
      CROSS_TIME_KNOWN = !is.na(.data$CROSS_TIME),
      BEARING_KNOWN = !is.na(.data$BEARING),
      RWY_KNOWN = !is.na(.data$RWY) & nzchar(.data$RWY),
      CLASS_KNOWN = !is.na(.data$CLASS) & nzchar(.data$CLASS),
      EXTREME_TMA = !is.na(.data$TMA_TIME) & .data$TMA_TIME > max_tma,
      NONPOSITIVE_TMA = !is.na(.data$TMA_TIME) & .data$TMA_TIME <= 0,
      VALID_TMA = .data$DIRECTION_OK &
        .data$CROSS_TIME_KNOWN &
        .data$BEARING_KNOWN &
        .data$RWY_KNOWN &
        .data$CLASS_KNOWN &
        !is.na(.data$TMA_TIME) &
        .data$TMA_TIME > 0 &
        .data$TMA_TIME <= max_tma
    ) |>
    dplyr::filter(.data$PHASE %in% c("ARR", "DEP")) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$MVT_TIME)
}

#' Summarise Terminal-Airspace Reference Input Quality
#'
#' Produces airport-phase-range level quality metrics for terminal-airspace
#' reference samples.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#'
#' @return A tibble with quality indicators.
#' @export
summarise_tma_reference_input_quality <- function(tma_samples) {
  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "RWY_KNOWN", "CLASS_KNOWN",
      "BEARING_KNOWN", "CROSS_TIME_KNOWN", "VALID_TMA", "DIRECTION_OK",
      "EXTREME_TMA", "NONPOSITIVE_TMA"
    )
  )

  tibble::as_tibble(tma_samples) |>
    dplyr::summarise(
      N_TOTAL = dplyr::n(),
      N_VALID_TMA = sum(.data$VALID_TMA, na.rm = TRUE),
      N_DIRECTION_ISSUES = sum(!.data$DIRECTION_OK, na.rm = TRUE),
      N_NONPOSITIVE_TMA = sum(.data$NONPOSITIVE_TMA, na.rm = TRUE),
      N_EXTREME_TMA = sum(.data$EXTREME_TMA, na.rm = TRUE),
      N_UNKNOWN_RWY = sum(!.data$RWY_KNOWN, na.rm = TRUE),
      N_UNKNOWN_CLASS = sum(!.data$CLASS_KNOWN, na.rm = TRUE),
      N_UNKNOWN_BEARING = sum(!.data$BEARING_KNOWN, na.rm = TRUE),
      N_UNKNOWN_CROSS_TIME = sum(!.data$CROSS_TIME_KNOWN, na.rm = TRUE),
      .by = c("ICAO", "PHASE", "RANGE_NM")
    ) |>
    dplyr::mutate(
      PCT_VALID_TMA = .data$N_VALID_TMA / .data$N_TOTAL,
      PCT_UNKNOWN_RWY = .data$N_UNKNOWN_RWY / .data$N_TOTAL,
      PCT_UNKNOWN_CLASS = .data$N_UNKNOWN_CLASS / .data$N_TOTAL,
      PCT_UNKNOWN_BEARING = .data$N_UNKNOWN_BEARING / .data$N_TOTAL,
      PCT_UNKNOWN_CROSS_TIME = .data$N_UNKNOWN_CROSS_TIME / .data$N_TOTAL
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM)
}

#' Build a Human-Readable Terminal-Airspace Sector Label
#'
#' Returns a human-readable sector identifier from the start and end bearings.
#'
#' @param bearing_from Start bearing in degrees.
#' @param bearing_to End bearing in degrees.
#'
#' @return A character vector.
#' @export
build_tma_sector_label <- function(bearing_from, bearing_to) {
  stringr::str_c(
    "BRG",
    sprintf("%03d", as.integer(round(bearing_from)) %% 360),
    "-",
    sprintf("%03d", as.integer(round(bearing_to)) %% 360)
  )
}

#' Suggest Terminal-Airspace Sector Definitions
#'
#' Builds a first-stab sectorisation by airport, phase, and range from the
#' observed bearing distribution. The algorithm rotates the circle to the
#' largest gap and then splits the rotated bearings into approximately equal
#' quantile slices.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#' @param n_sectors Number of sectors to suggest per airport-phase-range group.
#' @param valid_only When `TRUE`, only valid terminal-airspace samples are used.
#' @param min_quantile_n Minimum number of bearings required to use the quantile
#'   method. Below this threshold, equal-width sectors are returned.
#'
#' @return A tibble of suggested sector definitions.
#' @export
suggest_tma_sector_definitions <- function(
    tma_samples,
    n_sectors = 6,
    valid_only = TRUE,
    min_quantile_n = 100
) {
  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c("ICAO", "PHASE", "RANGE_NM", "BEARING", "VALID_TMA")
  )

  sector_input <- tibble::as_tibble(tma_samples)

  if (valid_only) {
    sector_input <- dplyr::filter(sector_input, .data$VALID_TMA)
  }

  sector_input |>
    dplyr::filter(!is.na(.data$BEARING)) |>
    dplyr::summarise(
      sector_definition = list(
        suggest_tma_sector_definitions_one(
          bearings = .data$BEARING,
          n_sectors = n_sectors,
          min_quantile_n = min_quantile_n
        )
      ),
      .by = c("ICAO", "PHASE", "RANGE_NM")
    ) |>
    tidyr::unnest(.data$sector_definition) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$SECTOR_SEQ)
}

#' Assign Terminal-Airspace Sectors
#'
#' Assigns a human-readable sector to each sample based on airport-specific
#' bearing ranges. Sector definitions can wrap across north by setting
#' `BEARING_FROM > BEARING_TO`.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#' @param sector_definitions A tibble containing at least `ICAO`, `PHASE`,
#'   `RANGE_NM`, `SECTOR`, `BEARING_FROM`, and `BEARING_TO`.
#' @param keep_unmatched When `TRUE`, keep samples that do not match any sector
#'   and leave sector fields as `NA`.
#'
#' @return A tibble with assigned sector information.
#' @export
assign_tma_sector <- function(
    tma_samples,
    sector_definitions,
    keep_unmatched = TRUE
) {
  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c("ICAO", "PHASE", "RANGE_NM", "BEARING")
  )

  defs <- validate_tma_sector_definitions(sector_definitions)

  samples <- tibble::as_tibble(tma_samples) |>
    dplyr::mutate(.ROW_ID = dplyr::row_number())

  matched <- dplyr::left_join(
    samples,
    defs,
    by = c("ICAO", "PHASE", "RANGE_NM")
  ) |>
    dplyr::filter(
      bearing_is_in_tma_sector(
        bearing = .data$BEARING,
        bearing_from = .data$BEARING_FROM,
        bearing_to = .data$BEARING_TO,
        start_rotation = .data$START_ROTATION %||% rep(NA_real_, dplyr::n()),
        cut_from = .data$CUT_FROM %||% rep(NA_real_, dplyr::n()),
        cut_to = .data$CUT_TO %||% rep(NA_real_, dplyr::n())
      )
    )

  overlaps <- matched |>
    dplyr::summarise(N_MATCHES = dplyr::n(), .by = c(".ROW_ID")) |>
    dplyr::filter(.data$N_MATCHES > 1)

  if (nrow(overlaps) > 0) {
    rlang::abort("Sector definitions overlap for at least one airport-phase-range sample.")
  }

  assigned <- dplyr::left_join(
    samples,
    dplyr::select(
      matched,
      ".ROW_ID", "SECTOR", "SECTOR_LABEL", "SECTOR_SEQ",
      "BEARING_FROM", "BEARING_TO", "START_ROTATION", "CUT_FROM", "CUT_TO"
    ),
    by = ".ROW_ID"
  )

  if (!keep_unmatched) {
    assigned <- dplyr::filter(assigned, !is.na(.data$SECTOR))
  }

  dplyr::select(assigned, -.data$.ROW_ID)
}

#' Build a Terminal-Airspace Reference Dataset
#'
#' Builds a reference table by `ICAO`, `PHASE`, `RANGE_NM`, `CLASS`, `RWY`,
#' and `SECTOR` for a selected time window and algorithm variant.
#'
#' @param tma_samples A sector-assigned sample tibble from
#'   `assign_tma_sector()`.
#' @param ref_start Start timestamp of the reference window.
#' @param ref_end End timestamp of the reference window.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements to consider a sample valid.
#' @param keep_below_threshold When `TRUE`, keep groups below `min_n` and flag
#'   them as invalid.
#' @param include_unknown When `TRUE`, keep groups with unknown runway, class,
#'   or sector.
#'
#' @return A tibble containing reference values and metadata.
#' @export
build_tma_reference <- function(
    tma_samples,
    ref_start,
    ref_end,
    variant = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    min_n = 5,
    keep_below_threshold = TRUE,
    include_unknown = FALSE
) {
  variant <- base::match.arg(variant)

  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR",
      "SECTOR_LABEL", "BEARING_FROM", "BEARING_TO", "MVT_TIME",
      "TMA_TIME", "VALID_TMA", "RWY_KNOWN", "CLASS_KNOWN"
    )
  )

  filtered_samples <- tibble::as_tibble(tma_samples)

  for (optional_col in c("START_ROTATION", "CUT_FROM", "CUT_TO")) {
    if (!optional_col %in% names(filtered_samples)) {
      filtered_samples[[optional_col]] <- NA_real_
    }
  }

  filtered_samples <- filtered_samples |>
    dplyr::filter(
      .data$MVT_TIME >= ref_start,
      .data$MVT_TIME <= ref_end,
      .data$VALID_TMA
    ) |>
    dplyr::mutate(SECTOR_KNOWN = !is.na(.data$SECTOR) & nzchar(.data$SECTOR))

  if (!include_unknown) {
    filtered_samples <- dplyr::filter(
      filtered_samples,
      .data$RWY_KNOWN,
      .data$CLASS_KNOWN,
      .data$SECTOR_KNOWN
    )
  }

  reference <- filtered_samples |>
    dplyr::summarise(
      N = dplyr::n(),
      REF_TMA = calc_tma_reference_value(.data$TMA_TIME, variant = variant),
      SECTOR_LABEL = dplyr::first(.data$SECTOR_LABEL),
      BEARING_FROM = dplyr::first(.data$BEARING_FROM),
      BEARING_TO = dplyr::first(.data$BEARING_TO),
      START_ROTATION = dplyr::first(.data$START_ROTATION),
      CUT_FROM = dplyr::first(.data$CUT_FROM),
      CUT_TO = dplyr::first(.data$CUT_TO),
      .by = c("ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR")
    ) |>
    dplyr::mutate(
      REF_START = as.POSIXct(ref_start, tz = lubridate::tz(ref_start)),
      REF_END = as.POSIXct(ref_end, tz = lubridate::tz(ref_end)),
      REF_PERIOD = build_reference_period_label(ref_start, ref_end),
      REF_VARIANT = variant,
      MIN_N = min_n,
      IS_VALID_SAMPLE = .data$N >= min_n
    ) |>
    dplyr::arrange(
      .data$ICAO, .data$PHASE, .data$RANGE_NM,
      .data$CLASS, .data$RWY, .data$SECTOR
    )

  if (!keep_below_threshold) {
    reference <- dplyr::filter(reference, .data$IS_VALID_SAMPLE)
  }

  reference
}

#' Check Terminal-Airspace Reference Coverage
#'
#' Summarises the coverage of a terminal-airspace reference dataset and
#' optionally compares it with the combinations needed for an analysis sample.
#'
#' @param reference_data A reference tibble from `build_tma_reference()`.
#' @param analysis_samples Optional terminal-airspace sample tibble for the
#'   analysis period.
#'
#' @return A tibble with coverage indicators.
#' @export
check_tma_reference_coverage <- function(reference_data, analysis_samples = NULL) {
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c("ICAO", "PHASE", "RANGE_NM", "SECTOR", "IS_VALID_SAMPLE")
  )

  coverage <- tibble::as_tibble(reference_data) |>
    dplyr::summarise(
      N_GROUPS = dplyr::n(),
      N_VALID_GROUPS = sum(.data$IS_VALID_SAMPLE, na.rm = TRUE),
      .by = c("ICAO", "PHASE", "RANGE_NM")
    )

  if (is.null(analysis_samples)) {
    return(coverage)
  }

  needed <- tibble::as_tibble(analysis_samples) |>
    dplyr::distinct(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$CLASS, .data$RWY, .data$SECTOR) |>
    dplyr::left_join(
      dplyr::select(
        reference_data,
        "ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR",
        "IS_VALID_SAMPLE"
      ),
      by = c("ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR")
    ) |>
    dplyr::mutate(
      HAS_REFERENCE = !is.na(.data$IS_VALID_SAMPLE),
      HAS_VALID_REFERENCE = .data$IS_VALID_SAMPLE %in% TRUE
    ) |>
    dplyr::summarise(
      N_NEEDED_GROUPS = dplyr::n(),
      N_MATCHED_GROUPS = sum(.data$HAS_REFERENCE, na.rm = TRUE),
      N_VALID_MATCHED_GROUPS = sum(.data$HAS_VALID_REFERENCE, na.rm = TRUE),
      .by = c("ICAO", "PHASE", "RANGE_NM")
    )

  dplyr::left_join(coverage, needed, by = c("ICAO", "PHASE", "RANGE_NM"))
}

#' Build a PBWG Terminal-Airspace Reference File Name
#'
#' Builds a file name using the PBWG convention for terminal-airspace reference
#' files.
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
build_pbwg_tma_reference_filename <- function(
    airport,
    ref_period,
    variant,
    min_n,
    region = "EUR",
    ext = "csv"
) {
  stringr::str_c(
    stringr::str_c(
      "PBWG", region, airport, "ref-tma", ref_period, variant, stringr::str_c("n", min_n, sep = ""),
      sep = "-"
    ),
    ".",
    ext
  )
}

#' Write PBWG Terminal-Airspace Reference Output
#'
#' Writes a PBWG terminal-airspace reference table for one airport.
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
write_pbwg_tma_reference <- function(
    data,
    airport,
    ref_period,
    variant,
    min_n,
    output_dir,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_tma_reference_filename(
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

#' Create Canonical Annual PBWG Terminal-Airspace Reference Files from an APDF
#' Archive
#'
#' Processes a reference-year APDF archive and writes canonical terminal-
#' airspace reference files, one per airport, to the requested output
#' directory.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param ref_year Reference year used to build the reference dataset.
#' @param sector_definitions A tibble of airport-specific sector definitions.
#' @param output_dir Directory where the reference files will be written.
#' @param airports Optional character vector of ICAO airport codes to keep.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param ranges Numeric vector of ranges in nautical miles to keep.
#' @param variant Reference algorithm variant.
#' @param min_n Minimum number of movements to consider a sample valid.
#' @param max_tma Maximum travel time in minutes kept as a candidate sample.
#' @param keep_below_threshold When `TRUE`, keep groups below `min_n` and flag
#'   them as invalid.
#' @param include_unknown When `TRUE`, keep groups with unknown runway, class,
#'   or sector.
#' @param region Region label included in the output file names.
#'
#' @return A named character vector of output file paths.
#' @export
create_pbwg_tma_reference_annual_file <- function(
    zipped_archive_path,
    ref_year,
    sector_definitions,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    ranges = c(40, 100),
    variant = c("icao_ganp_p20", "pbwg_avg_p05_p15"),
    min_n = 5,
    max_tma = 180,
    keep_below_threshold = TRUE,
    include_unknown = FALSE,
    region = "EUR"
) {
  variant <- base::match.arg(variant)
  type <- base::match.arg(type)

  tma_samples <- prepare_apdf_tma_reference_input_from_zip(
    zipped_archive_path = zipped_archive_path,
    files = files,
    type = type,
    ranges = ranges,
    max_tma = max_tma
  )

  if (!base::is.null(airports)) {
    tma_samples <- dplyr::filter(tma_samples, .data$ICAO %in% airports)
  }

  tma_samples <- assign_tma_sector(
    tma_samples = tma_samples,
    sector_definitions = sector_definitions
  )

  ref_start <- lubridate::ymd_hms(stringr::str_c(ref_year, "-01-01 00:00:00"), tz = "UTC")
  ref_end <- lubridate::ymd_hms(stringr::str_c(ref_year, "-12-31 23:59:59"), tz = "UTC")

  reference <- build_tma_reference(
    tma_samples = tma_samples,
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
      write_pbwg_tma_reference(
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

#' Apply Terminal-Airspace Reference Data to APDF Samples
#'
#' Joins terminal-airspace reference values to movement-level samples and
#' computes the additional terminal-airspace time as observed time minus the
#' chosen reference time for each `ICAO` / `PHASE` / `RANGE_NM` / `CLASS` /
#' `RWY` / `SECTOR` combination.
#'
#' @param tma_samples A sector-assigned sample tibble from `assign_tma_sector()`.
#' @param reference_data A reference tibble from `build_tma_reference()`.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#'
#' @return A movement-level tibble with joined reference values and additional
#'   terminal-airspace time.
#' @export
apply_tma_reference <- function(
    tma_samples,
    reference_data,
    valid_reference_only = TRUE
) {
  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR",
      "MVT_TIME", "TMA_TIME", "VALID_TMA"
    )
  )
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR",
      "REF_TMA", "REF_VARIANT", "REF_PERIOD", "MIN_N", "IS_VALID_SAMPLE"
    )
  )

  reference_lookup <- tibble::as_tibble(reference_data)

  if (valid_reference_only) {
    reference_lookup <- dplyr::filter(reference_lookup, .data$IS_VALID_SAMPLE)
  }

  tibble::as_tibble(tma_samples) |>
    dplyr::left_join(
      dplyr::select(
        reference_lookup,
        "ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR",
        "REF_TMA", "REF_VARIANT", "REF_PERIOD", "MIN_N", "IS_VALID_SAMPLE"
      ),
      by = c("ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR")
    ) |>
    dplyr::mutate(
      DATE = lubridate::date(.data$MVT_TIME),
      HAS_REFERENCE = !is.na(.data$REF_TMA),
      TMA_NA = !.data$HAS_REFERENCE,
      ADD_TMA = dplyr::if_else(
        .data$VALID_TMA & .data$HAS_REFERENCE,
        .data$TMA_TIME - .data$REF_TMA,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$MVT_TIME)
}

#' Summarise Daily Additional Terminal-Airspace Time
#'
#' Aggregates augmented terminal-airspace samples to the daily PBWG output
#' format used for arrival (`ASMA`) and departure (`DSMA`) additional time
#' analyses.
#'
#' @param augmented_tma A tibble from `apply_tma_reference()`.
#' @param year Optional reporting year filter.
#' @param valid_only When `TRUE`, only movements with valid terminal-airspace
#'   times are counted.
#'
#' @return A tibble with daily summary metrics.
#' @export
summarise_pbwg_tma_daily <- function(
    augmented_tma,
    year = NULL,
    valid_only = TRUE
) {
  stop_if_apdf_columns_missing(
    augmented_tma,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "DATE", "TMA_TIME", "REF_TMA",
      "ADD_TMA", "TMA_NA", "VALID_TMA"
    )
  )

  summary_input <- tibble::as_tibble(augmented_tma)

  if (valid_only) {
    summary_input <- dplyr::filter(summary_input, .data$VALID_TMA)
  }

  daily_summary <- summary_input |>
    dplyr::summarise(
      MVTS = dplyr::n(),
      TOT_TMA_TIME = sum(.data$TMA_TIME, na.rm = TRUE),
      TOT_REF = sum(.data$REF_TMA, na.rm = TRUE),
      TOT_ADD_TIME = sum(.data$ADD_TMA, na.rm = TRUE),
      TMA_NA = sum(.data$TMA_NA, na.rm = TRUE),
      .by = c("ICAO", "PHASE", "RANGE_NM", "DATE")
    ) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$DATE)

  if (is.null(year)) {
    return(daily_summary)
  }

  dplyr::filter(daily_summary, lubridate::year(.data$DATE) == year)
}

#' Prepare Augmented APDF Terminal-Airspace Data Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares terminal-airspace samples, joins
#' a chosen reference dataset, and produces movement-level augmented
#' terminal-airspace data.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param reference_data A reference tibble from `build_tma_reference()`.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param ranges Numeric vector of ranges in nautical miles to keep.
#' @param max_tma Maximum travel time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#'
#' @return A movement-level tibble with reference values and additional
#'   terminal-airspace time.
#' @export
prepare_apdf_tma_augmented_zip <- function(
    zipped_archive_path,
    reference_data,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    ranges = c(40, 100),
    max_tma = 180,
    valid_reference_only = TRUE
) {
  type <- base::match.arg(type)
  sector_definitions <- reference_to_tma_sector_definitions(reference_data)

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
        prepare_apdf_tma_input() |>
        prepare_tma_reference_input(
          ranges = ranges,
          max_tma = max_tma
        ) |>
        assign_tma_sector(sector_definitions = sector_definitions) |>
        apply_tma_reference(
          reference_data = reference_data,
          valid_reference_only = valid_reference_only
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$MVT_TIME)

  if (is.null(year)) {
    return(augmented)
  }

  dplyr::filter(augmented, lubridate::year(.data$DATE) == year)
}

#' Prepare Daily Additional Terminal-Airspace Time Directly from a ZIP Archive
#'
#' Reads APDF files from an archive, prepares terminal-airspace samples,
#' applies a chosen reference dataset, and returns daily summary metrics.
#'
#' @param zipped_archive_path Full path to the ZIP archive.
#' @param reference_data A reference tibble from `build_tma_reference()`.
#' @param files Optional character vector of archived files to process. If
#'   `NULL`, all archived files are processed.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param year Optional reporting year filter.
#' @param ranges Numeric vector of ranges in nautical miles to keep.
#' @param max_tma Maximum travel time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#' @param valid_only When `TRUE`, only movements with valid terminal-airspace
#'   times are counted in the daily summary.
#'
#' @return A tibble with daily summary metrics.
#' @export
prepare_apdf_tma_daily_zip <- function(
    zipped_archive_path,
    reference_data,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    year = NULL,
    ranges = c(40, 100),
    max_tma = 180,
    valid_reference_only = TRUE,
    valid_only = TRUE
) {
  prepare_apdf_tma_augmented_zip(
    zipped_archive_path = zipped_archive_path,
    reference_data = reference_data,
    files = files,
    type = type,
    year = year,
    ranges = ranges,
    max_tma = max_tma,
    valid_reference_only = valid_reference_only
  ) |>
    summarise_pbwg_tma_daily(year = year, valid_only = valid_only)
}

#' Build a PBWG Daily Terminal-Airspace File Name
#'
#' Builds a file name using the PBWG convention for annual and multi-year
#' additional terminal-airspace products. When `airport` is `NULL`, the file
#' name is for the project-level aggregate.
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
build_pbwg_tma_filename <- function(
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

  pieces <- c("PBWG", region, airport, "tma-analytic", year_label, stringr::str_c("ref", ref_period), variant)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]

  stringr::str_c(stringr::str_c(pieces, collapse = "-"), ".", ext)
}

#' Build a PBWG Augmented Terminal-Airspace File Name
#'
#' Builds a file name for the movement-level augmented terminal-airspace
#' dataset used for verification and troubleshooting.
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
build_pbwg_tma_augmented_filename <- function(
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
      "PBWG", region, airport, "tma-augmented", year_label,
      stringr::str_c("ref", ref_period), variant,
      sep = "-"
    ),
    ".",
    ext
  )
}

#' Write PBWG Daily Terminal-Airspace Output
#'
#' Writes a PBWG terminal-airspace summary either for one airport or for the
#' project aggregate to an explicit output directory.
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
write_pbwg_tma <- function(
    data,
    year,
    ref_period,
    variant,
    output_dir,
    airport = NULL,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_tma_filename(
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

#' Write PBWG Augmented Terminal-Airspace Output
#'
#' Writes the movement-level augmented terminal-airspace dataset for one airport
#' to an explicit output directory.
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
write_pbwg_tma_augmented <- function(
    data,
    year,
    airport,
    ref_period,
    variant,
    output_dir,
    region = "EUR"
) {
  fs::dir_create(output_dir)

  output_name <- build_pbwg_tma_augmented_filename(
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

#' Create Canonical Annual PBWG Daily Terminal-Airspace Files from an APDF
#' Archive
#'
#' Processes a yearly APDF archive, applies a chosen reference dataset, and
#' writes canonical annual PBWG daily terminal-airspace files, one per airport,
#' to the requested output directory. Optionally, the movement-level augmented
#' data can be written as well for verification purposes.
#'
#' @param zipped_archive_path Full path to the APDF ZIP archive.
#' @param year Reporting year for the canonical output files.
#' @param reference_data A reference tibble from `build_tma_reference()`.
#' @param output_dir Directory where the daily output files will be written.
#' @param airports Optional character vector of ICAO airport codes to keep. If
#'   `NULL`, all airports in the prepared summary are written.
#' @param files Optional archived files to process.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#' @param ranges Numeric vector of ranges in nautical miles to keep.
#' @param region Region label included in the output file names.
#' @param max_tma Maximum travel time in minutes kept as a candidate sample.
#' @param valid_reference_only When `TRUE`, only references flagged with
#'   `IS_VALID_SAMPLE` are used.
#' @param valid_only When `TRUE`, only movements with valid terminal-airspace
#'   times are counted in the daily summary.
#' @param save_augmented When `TRUE`, write the augmented movement-level data
#'   per airport.
#' @param augmented_dir Directory where augmented movement-level files will be
#'   written. Defaults to `output_dir`.
#'
#' @return A named list with `daily_paths` and `augmented_paths`.
#' @export
create_pbwg_tma_annual_file <- function(
    zipped_archive_path,
    year,
    reference_data,
    output_dir,
    airports = NULL,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    ranges = c(40, 100),
    region = "EUR",
    max_tma = 180,
    valid_reference_only = TRUE,
    valid_only = TRUE,
    save_augmented = FALSE,
    augmented_dir = output_dir
) {
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c("REF_PERIOD", "REF_VARIANT")
  )

  augmented <- prepare_apdf_tma_augmented_zip(
    zipped_archive_path = zipped_archive_path,
    reference_data = reference_data,
    files = files,
    type = type,
    year = year,
    ranges = ranges,
    max_tma = max_tma,
    valid_reference_only = valid_reference_only
  )

  if (!base::is.null(airports)) {
    augmented <- dplyr::filter(augmented, .data$ICAO %in% airports)
  }

  summary_data <- summarise_pbwg_tma_daily(
    augmented_tma = augmented,
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
      write_pbwg_tma(
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
        write_pbwg_tma_augmented(
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

#' Read PBWG Daily Terminal-Airspace Files
#'
#' Reads one or more PBWG daily terminal-airspace files and returns a single
#' combined tibble.
#'
#' @param paths Character vector of file paths.
#'
#' @return A tibble.
#' @export
read_pbwg_tma_files <- function(paths) {
  purrr::map(paths, readr::read_csv, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
}

#' Combine Canonical PBWG Daily Terminal-Airspace Files into a Project Summary
#'
#' Reads canonical annual PBWG daily terminal-airspace files for the requested
#' airports and years, combines them into a project-level summary file, and
#' writes the derived product to the requested output directory.
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
combine_pbwg_tma_project <- function(
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
        build_pbwg_tma_filename(
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
        "Missing annual PBWG daily terminal-airspace files: ",
        stringr::str_flatten(base::basename(missing_paths), ", ")
      )
    )
  }

  if (base::length(existing_paths) == 0) {
    rlang::abort("No annual PBWG daily terminal-airspace files were found to combine.")
  }

  combined_data <- read_pbwg_tma_files(existing_paths) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$DATE)

  write_pbwg_tma(
    data = combined_data,
    year = years,
    ref_period = ref_period,
    variant = variant,
    output_dir = output_dir,
    airport = NULL,
    region = region
  )
}

#' Prepare Terminal-Airspace Sector Plot Inputs
#'
#' Extracts the sample bearings and sector definitions needed to visually check
#' the chosen terminal-airspace sectors for one airport, phase, and range.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#' @param sector_definitions A tibble containing sector definitions.
#' @param airport ICAO location indicator.
#' @param phase Operational phase, typically `"ARR"` or `"DEP"`.
#' @param range_nm Terminal-airspace range in nautical miles.
#' @param valid_only When `TRUE`, only valid terminal-airspace samples are kept.
#'
#' @return A list with `samples` and `sectors`.
#' @export
prepare_tma_sector_plot_input <- function(
    tma_samples,
    sector_definitions,
    airport,
    phase,
    range_nm,
    valid_only = TRUE
) {
  stop_if_apdf_columns_missing(
    tma_samples,
    required_columns = c("ICAO", "PHASE", "RANGE_NM", "BEARING", "VALID_TMA")
  )

  defs <- validate_tma_sector_definitions(sector_definitions)

  samples <- tibble::as_tibble(tma_samples) |>
    dplyr::filter(
      .data$ICAO %in% airport,
      .data$PHASE %in% phase,
      .data$RANGE_NM %in% range_nm
    )

  if (valid_only) {
    samples <- dplyr::filter(samples, .data$VALID_TMA)
  }

  sectors <- defs |>
    dplyr::filter(
      .data$ICAO %in% airport,
      .data$PHASE %in% phase,
      .data$RANGE_NM %in% range_nm
    ) |>
    dplyr::arrange(.data$SECTOR_SEQ)

  list(
    samples = samples,
    sectors = sectors
  )
}

#' Plot Terminal-Airspace Sector Diagnostics
#'
#' Draws a bearing histogram and overlays the selected sector boundaries so an
#' analyst can visually verify the chosen ASMA or DSMA sectors.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#' @param sector_definitions A tibble containing sector definitions.
#' @param airport ICAO location indicator.
#' @param phase Operational phase, typically `"ARR"` or `"DEP"`.
#' @param range_nm Terminal-airspace range in nautical miles.
#' @param valid_only When `TRUE`, only valid terminal-airspace samples are kept.
#' @param breaks Numeric vector of histogram breakpoints in degrees.
#' @param include_labels When `TRUE`, print sector labels above the histogram.
#' @param main Optional plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param col Histogram fill colour.
#' @param border Histogram border colour.
#' @param line_col Sector boundary colour.
#' @param label_cex Text size for sector labels.
#'
#' @return Invisibly returns the plot input list.
#' @export
plot_tma_sector_diagnostics <- function(
    tma_samples,
    sector_definitions,
    airport,
    phase,
    range_nm,
    valid_only = TRUE,
    breaks = seq(0, 360, by = 5),
    include_labels = TRUE,
    main = NULL,
    xlab = "Bearing from ARP (degrees)",
    ylab = "Movements",
    col = "grey85",
    border = "white",
    line_col = "red3",
    label_cex = 0.8
) {
  plot_input <- prepare_tma_sector_plot_input(
    tma_samples = tma_samples,
    sector_definitions = sector_definitions,
    airport = airport,
    phase = phase,
    range_nm = range_nm,
    valid_only = valid_only
  )

  bearings <- plot_input$samples$BEARING
  sectors <- plot_input$sectors

  if (is.null(main)) {
    main <- stringr::str_c(
      airport, " ", phase, " ", range_nm, "NM bearings (n=",
      sum(!is.na(bearings)),
      ")"
    )
  }

  hist_obj <- graphics::hist(
    bearings,
    breaks = breaks,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col = col,
    border = border,
    xlim = c(0, 360)
  )

  if (nrow(sectors) > 0) {
    boundaries <- unique(c(sectors$BEARING_FROM))
    graphics::abline(v = boundaries, col = line_col, lwd = 2, lty = 2)

    if (include_labels) {
      y_pos <- max(hist_obj$counts, na.rm = TRUE)
      if (!is.finite(y_pos)) {
        y_pos <- 0
      }

      label_positions <- purrr::map2_dbl(
        sectors$BEARING_FROM,
        sectors$BEARING_TO,
        midpoint_tma_sector
      )

      graphics::text(
        x = label_positions,
        y = rep(y_pos, length(label_positions)),
        labels = sectors$SECTOR_LABEL,
        cex = label_cex,
        pos = 3,
        xpd = TRUE
      )
    }
  }

  invisible(plot_input)
}

#' Build a PBWG Terminal-Airspace Sector Plot File Name
#'
#' Builds a file name for a sector-diagnostic plot.
#'
#' @param airport ICAO location indicator.
#' @param phase Operational phase.
#' @param range_nm Terminal-airspace range in nautical miles.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_pbwg_tma_sector_plot_filename <- function(
    airport,
    phase,
    range_nm,
    ext = "pdf"
) {
  stringr::str_c(
    "PBWG-",
    airport,
    "-",
    phase,
    "-",
    range_nm,
    "NM-sector-diagnostic.",
    ext
  )
}

#' Write Terminal-Airspace Sector Diagnostic Plots
#'
#' Writes one plot per airport-phase-range combination so the analyst can
#' review the chosen sector boundaries.
#'
#' @param tma_samples A terminal-airspace sample tibble from
#'   `prepare_tma_reference_input()`.
#' @param sector_definitions A tibble containing sector definitions.
#' @param output_dir Directory where the plots will be written.
#' @param valid_only When `TRUE`, only valid terminal-airspace samples are kept.
#' @param breaks Numeric vector of histogram breakpoints in degrees.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#'
#' @return A named character vector of written file paths.
#' @export
write_tma_sector_diagnostic_plots <- function(
    tma_samples,
    sector_definitions,
    output_dir,
    valid_only = TRUE,
    breaks = seq(0, 360, by = 5),
    width = 10,
    height = 6
) {
  stop_if_apdf_columns_missing(
    sector_definitions,
    required_columns = c("ICAO", "PHASE", "RANGE_NM")
  )

  fs::dir_create(output_dir)

  plot_index <- validate_tma_sector_definitions(sector_definitions) |>
    dplyr::distinct(.data$ICAO, .data$PHASE, .data$RANGE_NM) |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM)

  paths <- purrr::pmap_chr(
    plot_index,
    function(ICAO, PHASE, RANGE_NM) {
      output_path <- fs::path(
        output_dir,
        build_pbwg_tma_sector_plot_filename(
          airport = ICAO,
          phase = PHASE,
          range_nm = RANGE_NM
        )
      )

      grDevices::pdf(output_path, width = width, height = height)
      tryCatch(
        {
          plot_tma_sector_diagnostics(
            tma_samples = tma_samples,
            sector_definitions = sector_definitions,
            airport = ICAO,
            phase = PHASE,
            range_nm = RANGE_NM,
            valid_only = valid_only,
            breaks = breaks
          )
        },
        finally = {
          grDevices::dev.off()
        }
      )

      output_path
    }
  )

  stats::setNames(
    paths,
    stringr::str_c(plot_index$ICAO, plot_index$PHASE, plot_index$RANGE_NM, sep = "_")
  )
}

prepare_apdf_tma_reference_input_from_zip <- function(
    zipped_archive_path,
    files = NULL,
    type = c("parquet", "csv", "csv_auto"),
    ranges = c(40, 100),
    max_tma = 180
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
        prepare_apdf_tma_input() |>
        prepare_tma_reference_input(
          ranges = ranges,
          max_tma = max_tma
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$MVT_TIME)
}

build_tma_crossing_spec <- function(ranges) {
  spec <- tibble::tibble(
    RANGE_NM = c(40, 100),
    CROSS_TIME_COL = c("C40_CROSS_TIME", "C100_CROSS_TIME"),
    BEARING_COL = c("C40_BEARING", "C100_BEARING")
  )

  unsupported <- base::setdiff(ranges, spec$RANGE_NM)

  if (base::length(unsupported) > 0) {
    rlang::abort(
      stringr::str_c(
        "Unsupported ranges requested: ",
        stringr::str_flatten(unsupported, ", ")
      )
    )
  }

  dplyr::filter(spec, .data$RANGE_NM %in% ranges) |>
    dplyr::arrange(.data$RANGE_NM)
}

normalise_pbwg_aircraft_class <- function(class) {
  class <- stringr::str_to_upper(trimws(as.character(class)))
  class[class %in% c("", "NA")] <- NA_character_
  class[stringr::str_starts(class, "HEL")] <- NA_character_
  class[stringr::str_starts(class, "H")] <- "H"
  class
}

validate_tma_sector_definitions <- function(sector_definitions) {
  stop_if_apdf_columns_missing(
    sector_definitions,
    required_columns = c("ICAO", "PHASE", "RANGE_NM", "SECTOR", "BEARING_FROM", "BEARING_TO")
  )

  defs <- tibble::as_tibble(sector_definitions)

  if (!"SECTOR_LABEL" %in% names(defs)) {
    defs$SECTOR_LABEL <- defs$SECTOR
  }

  if (!"SECTOR_SEQ" %in% names(defs)) {
    defs$SECTOR_SEQ <- seq_len(nrow(defs))
  }

  for (optional_col in c("START_ROTATION", "CUT_FROM", "CUT_TO")) {
    if (!optional_col %in% names(defs)) {
      defs[[optional_col]] <- NA_real_
    }
  }

  defs <- defs |>
    dplyr::mutate(
      SECTOR = as.character(.data$SECTOR),
      SECTOR_LABEL = dplyr::coalesce(.data$SECTOR_LABEL, .data$SECTOR),
      BEARING_FROM = .data$BEARING_FROM %% 360,
      BEARING_TO = .data$BEARING_TO %% 360
    )

  duplicates <- defs |>
    dplyr::summarise(N = dplyr::n(), .by = c("ICAO", "PHASE", "RANGE_NM", "SECTOR")) |>
    dplyr::filter(.data$N > 1)

  if (nrow(duplicates) > 0) {
    rlang::abort("Sector definitions must be unique by ICAO, PHASE, RANGE_NM, and SECTOR.")
  }

  defs |>
    dplyr::arrange(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$SECTOR_SEQ)
}

bearing_is_in_tma_sector <- function(
    bearing,
    bearing_from,
    bearing_to,
    start_rotation = NA_real_,
    cut_from = NA_real_,
    cut_to = NA_real_
) {
  rotated_available <- !is.na(start_rotation) & !is.na(cut_from) & !is.na(cut_to)
  bearing_rot <- (bearing - start_rotation) %% 360

  ifelse(
    rotated_available,
    !is.na(bearing_rot) & bearing_rot >= cut_from & bearing_rot < cut_to,
    dplyr::case_when(
      is.na(bearing) | is.na(bearing_from) | is.na(bearing_to) ~ FALSE,
      bearing_from < bearing_to ~ bearing >= bearing_from & bearing < bearing_to,
      bearing_from > bearing_to ~ bearing >= bearing_from | bearing < bearing_to,
      .default = TRUE
    )
  )
}

suggest_tma_sector_definitions_one <- function(
    bearings,
    n_sectors = 6,
    min_quantile_n = 100
) {
  bearings <- sort((as.numeric(bearings) %% 360)[!is.na(bearings)])

  if (base::length(bearings) == 0) {
    return(tibble::tibble())
  }

  if (base::length(bearings) < min_quantile_n) {
    start_rotation <- 0
    cuts <- seq(0, 360, length.out = n_sectors + 1)
  } else {
    gaps <- diff(c(bearings, bearings[1] + 360))
    start_rotation <- bearings[which.max(gaps)] %% 360
    rotated <- sort((bearings - start_rotation) %% 360)
    cuts <- as.numeric(
      stats::quantile(
        rotated,
        probs = seq(0, 1, length.out = n_sectors + 1),
        names = FALSE,
        na.rm = TRUE,
        type = 8
      )
    )
    cuts[1] <- 0
    cuts[base::length(cuts)] <- 360

    if (any(diff(cuts) <= 1)) {
      start_rotation <- 0
      cuts <- seq(0, 360, length.out = n_sectors + 1)
    }
  }

  tibble::tibble(
    SECTOR_SEQ = seq_len(n_sectors),
    START_ROTATION = start_rotation,
    CUT_FROM = head(cuts, -1),
    CUT_TO = tail(cuts, -1),
    BEARING_FROM = (start_rotation + head(cuts, -1)) %% 360,
    BEARING_TO = (start_rotation + tail(cuts, -1)) %% 360
  ) |>
    dplyr::mutate(
      SECTOR = build_tma_sector_label(.data$BEARING_FROM, .data$BEARING_TO),
      SECTOR_LABEL = .data$SECTOR
    )
}

calc_tma_reference_value <- function(tma_time, variant) {
  switch(
    variant,
    icao_ganp_p20 = as.numeric(stats::quantile(tma_time, probs = 0.20, names = FALSE, na.rm = TRUE)),
    pbwg_avg_p05_p15 = {
      q05 <- as.numeric(stats::quantile(tma_time, probs = 0.05, names = FALSE, na.rm = TRUE))
      q15 <- as.numeric(stats::quantile(tma_time, probs = 0.15, names = FALSE, na.rm = TRUE))
      (q05 + q15) / 2
    }
  )
}

reference_to_tma_sector_definitions <- function(reference_data) {
  stop_if_apdf_columns_missing(
    reference_data,
    required_columns = c(
      "ICAO", "PHASE", "RANGE_NM", "SECTOR",
      "SECTOR_LABEL", "BEARING_FROM", "BEARING_TO"
    )
  )

  defs <- tibble::as_tibble(reference_data)

  for (optional_col in c("START_ROTATION", "CUT_FROM", "CUT_TO")) {
    if (!optional_col %in% names(defs)) {
      defs[[optional_col]] <- NA_real_
    }
  }

  defs |>
    dplyr::distinct(
      .data$ICAO, .data$PHASE, .data$RANGE_NM, .data$SECTOR,
      .data$SECTOR_LABEL, .data$BEARING_FROM, .data$BEARING_TO,
      .data$START_ROTATION, .data$CUT_FROM, .data$CUT_TO
    ) |>
    dplyr::mutate(SECTOR_SEQ = dplyr::row_number(), .by = c("ICAO", "PHASE", "RANGE_NM"))
}

midpoint_tma_sector <- function(bearing_from, bearing_to) {
  if (is.na(bearing_from) || is.na(bearing_to)) {
    return(NA_real_)
  }

  if (bearing_from <= bearing_to) {
    return((bearing_from + bearing_to) / 2)
  }

  ((bearing_from + ((bearing_to + 360) - bearing_from) / 2) %% 360)
}
