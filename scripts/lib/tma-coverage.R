# Coverage diagnostics only: never changes reference values or production eligibility.
tma_missing_label <- function(x) is.na(x) | toupper(trimws(as.character(x))) %in% c("", "NA", "N/A", "NULL")

tma_review_band <- function(pct) {
  dplyr::case_when(is.na(pct) ~ "NO_DENOMINATOR", pct < 2 ~ "LOW",
    pct < 5 ~ "WATCH", pct < 10 ~ "REVIEW", TRUE ~ "SUBSTANTIAL")
}

read_tma_coverage_references <- function(run, variant = "pbwg_avg_p05_p15") {
  manifest <- readr::read_csv(file.path(run, "manifest.csv"), show_col_types = FALSE)
  selected <- manifest[manifest$PRODUCT == "reference" & !is.na(manifest$REF_VARIANT) &
                         manifest$REF_VARIANT == variant, ]
  dplyr::bind_rows(lapply(seq_len(nrow(selected)), function(i) {
    path <- file.path(run, selected$FILE[[i]])
    if (unname(tools::md5sum(path)) != selected$MD5[[i]]) stop("Reference checksum mismatch: ", path)
    # Literal 'NA' runway groups are intentional diagnostic categories. Preserve them.
    x <- readr::read_csv(path, na = character(), show_col_types = FALSE,
                        col_types = readr::cols(.default = readr::col_character()))
    x |>
      dplyr::transmute(ICAO, PHASE, RANGE_NM = as.integer(RANGE_NM), CLASS, RWY, SECTOR,
                       REF_N = as.integer(N), REF_TMA = as.numeric(REF_TMA))
  }))
}

tma_coverage_samples <- function(raw, airport, sectors, reference) {
  raw <- dplyr::mutate(raw, dplyr::across(dplyr::where(~ inherits(.x, "POSIXt")),
                                        ~ lubridate::with_tz(.x, "UTC")))
  block_date <- if ("BLOCK_TIME_UTC" %in% names(raw)) as.Date(raw$BLOCK_TIME_UTC, tz = "UTC") else
    rep(as.Date(NA), nrow(raw))
  meta <- tibble::tibble(ROW_ID = seq_len(nrow(raw)),
    OBS_DATE = dplyr::coalesce(as.Date(raw$MVT_TIME_UTC, tz = "UTC"), block_date),
    MISSING_MOVEMENT = is.na(raw$MVT_TIME_UTC),
    RWY_UNIDENTIFIED = tma_missing_label(raw$AP_C_RWY),
    CLASS_UNIDENTIFIED = tma_missing_label(raw$AC_CLASS),
    HELICOPTER = !is.na(raw$AC_CLASS) & grepl("^HEL", toupper(trimws(raw$AC_CLASS))))
  input <- prepare_apdf_tma_input(raw)
  input$FLTID <- as.character(meta$ROW_ID) # Private row join key, not a changed source identifier.
  samples <- prepare_tma_reference_input(input) |>
    dplyr::mutate(ROW_ID = as.integer(FLTID), AIRPORT_UNIDENTIFIED = is.na(ICAO)) |>
    dplyr::left_join(meta, by = "ROW_ID", relationship = "many-to-one")
  if (any(!is.na(samples$ICAO) & samples$ICAO != airport)) stop("Unexpected airport in source: ", airport)
  samples <- assign_tma_sector(samples, sectors) |>
    dplyr::mutate(ICAO = airport, ALGORITHM_VALID = VALID_TMA & !AIRPORT_UNIDENTIFIED)
  keys <- c("ICAO", "PHASE", "RANGE_NM", "CLASS", "RWY", "SECTOR")
  samples <- samples |>
    dplyr::left_join(reference, by = keys, relationship = "many-to-one") |>
    dplyr::mutate(HAS_REFERENCE = !is.na(REF_TMA),
      IDENTIFIED = !RWY_UNIDENTIFIED & !CLASS_UNIDENTIFIED & !AIRPORT_UNIDENTIFIED,
      MISSING_CROSSING = !CROSS_TIME_KNOWN,
      BAD_TIMING = CROSS_TIME_KNOWN & !MISSING_MOVEMENT & (!DIRECTION_OK | NONPOSITIVE_TMA | EXTREME_TMA),
      MISSING_BEARING_WITH_CROSSING = CROSS_TIME_KNOWN & !BEARING_KNOWN,
      DATA_ATTENTION = RWY_UNIDENTIFIED | CLASS_UNIDENTIFIED | AIRPORT_UNIDENTIFIED |
        MISSING_MOVEMENT | BAD_TIMING | MISSING_BEARING_WITH_CROSSING,
      MATCHED = ALGORITHM_VALID & HAS_REFERENCE,
      MISS = ALGORITHM_VALID & !HAS_REFERENCE,
      MISS_UNIDENTIFIED = MISS & !IDENTIFIED,
      MISS_IDENTIFIED = MISS & IDENTIFIED,
      MATCHED_IDENTIFIED = MATCHED & IDENTIFIED,
      MATCHED_UNIDENTIFIED = MATCHED & !IDENTIFIED,
      SMALL_REF_5 = MATCHED & !is.na(REF_N) & REF_N < 5,
      SMALL_REF_20 = MATCHED & !is.na(REF_N) & REF_N < 20)
  samples
}

tma_coverage_counts <- function(samples, groups) {
  samples |>
    dplyr::summarise(N_REPORTED = dplyr::n(),
      N_MISSING_CROSSING = sum(MISSING_CROSSING), N_MISSING_MOVEMENT = sum(MISSING_MOVEMENT),
      N_RUNWAY_UNIDENTIFIED = sum(RWY_UNIDENTIFIED), N_CLASS_UNIDENTIFIED = sum(CLASS_UNIDENTIFIED),
      N_HELICOPTER = sum(HELICOPTER), N_AIRPORT_UNIDENTIFIED = sum(AIRPORT_UNIDENTIFIED),
      N_BAD_TIMING = sum(BAD_TIMING), N_MISSING_BEARING_WITH_CROSSING = sum(MISSING_BEARING_WITH_CROSSING),
      N_DATA_ATTENTION = sum(DATA_ATTENTION), N_VALID = sum(ALGORITHM_VALID),
      N_MATCHED = sum(MATCHED), N_MISS = sum(MISS),
      N_MISS_UNIDENTIFIED = sum(MISS_UNIDENTIFIED), N_MISS_IDENTIFIED = sum(MISS_IDENTIFIED),
      N_MATCHED_IDENTIFIED = sum(MATCHED_IDENTIFIED), N_MATCHED_UNIDENTIFIED = sum(MATCHED_UNIDENTIFIED),
      N_SMALL_REF_5 = sum(SMALL_REF_5), N_SMALL_REF_20 = sum(SMALL_REF_20),
      N_OBSERVED_DAYS = dplyr::n_distinct(OBS_DATE),
      .by = dplyr::all_of(groups))
}

tma_coverage_rates <- function(x) {
  pct <- function(n, d) ifelse(d > 0, 100 * n / d, NA_real_)
  x |>
    dplyr::mutate(PCT_MISSING_CROSSING = pct(N_MISSING_CROSSING, N_REPORTED),
      PCT_DATA_ATTENTION = pct(N_DATA_ATTENTION, N_REPORTED),
      PCT_VALID = pct(N_VALID, N_REPORTED),
      PCT_MATCHED_REPORTED = pct(N_MATCHED, N_REPORTED),
      PCT_IDENTIFIED_MATCHED_REPORTED = pct(N_MATCHED_IDENTIFIED, N_REPORTED),
      PCT_MISS_VALID = pct(N_MISS, N_VALID),
      PCT_MISS_IDENTIFIED_VALID = pct(N_MISS_IDENTIFIED, N_VALID),
      PCT_SMALL_REF_5_MATCHED = pct(N_SMALL_REF_5, N_MATCHED),
      PCT_SMALL_REF_20_MATCHED = pct(N_SMALL_REF_20, N_MATCHED),
      CROSSING_BAND = tma_review_band(PCT_MISSING_CROSSING),
      DATA_BAND = tma_review_band(PCT_DATA_ATTENTION),
      REFERENCE_BAND = tma_review_band(PCT_MISS_VALID))
}
