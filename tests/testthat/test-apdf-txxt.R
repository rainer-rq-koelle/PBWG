test_that("prepare_txxt_reference_input computes taxi durations by phase", {
  txxt_input <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("DEP", "ARR"),
    RWY = c("27R", "27L"),
    STND = c("A1", "B2"),
    MVT_TIME = as.POSIXct(c("2024-01-01 10:10:00", "2024-01-01 10:00:00"), tz = "UTC"),
    BLOCK_TIME = as.POSIXct(c("2024-01-01 09:50:00", "2024-01-01 10:12:00"), tz = "UTC")
  )

  prepared <- prepare_txxt_reference_input(txxt_input)

  expect_equal(prepared$TXXT, c(20, 12))
  expect_true(all(prepared$VALID_TXXT))
})

test_that("prepare_txxt_reference_input enforces phase direction and 120 minute cutoff", {
  txxt_input <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL", "EGLL"),
    PHASE = c("DEP", "ARR", "DEP", "ARR"),
    RWY = c("27R", "27L", "27R", "27L"),
    STND = c("A1", "B2", "A1", "B2"),
    MVT_TIME = as.POSIXct(
      c(
        "2024-01-01 10:10:00",
        "2024-01-01 10:00:00",
        "2024-01-01 09:50:00",
        "2024-01-01 10:00:00"
      ),
      tz = "UTC"
    ),
    BLOCK_TIME = as.POSIXct(
      c(
        "2024-01-01 09:50:00",
        "2024-01-01 10:12:00",
        "2024-01-01 10:10:00",
        "2024-01-01 12:30:00"
      ),
      tz = "UTC"
    )
  )

  prepared <- prepare_txxt_reference_input(txxt_input)

  expect_equal(prepared$DIRECTION_OK, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(prepared$EXTREME_TXXT, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(prepared$VALID_TXXT, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("build_txxt_reference supports both variants and validity flags", {
  txxt_samples <- tibble::tibble(
    ICAO = rep("EGLL", 6),
    PHASE = rep("DEP", 6),
    RWY = rep("27R", 6),
    STND = rep("A1", 6),
    BLOCK_TIME = as.POSIXct(
      c("2024-01-01 10:00:00", "2024-01-02 10:00:00", "2024-01-03 10:00:00",
        "2024-01-04 10:00:00", "2024-01-05 10:00:00", "2024-01-06 10:00:00"),
      tz = "UTC"
    ),
    TXXT = c(10, 12, 14, 16, 18, 20),
    VALID_TXXT = TRUE,
    RWY_KNOWN = TRUE,
    STND_KNOWN = TRUE
  )

  ref_ganp <- build_txxt_reference(
    txxt_samples = txxt_samples,
    ref_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ref_end = as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
    variant = "icao_ganp_p20",
    min_n = 5
  )

  ref_pbwg <- build_txxt_reference(
    txxt_samples = txxt_samples,
    ref_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ref_end = as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
    variant = "pbwg_avg_p05_p15",
    min_n = 10
  )

  expect_equal(ref_ganp$N, 6L)
  expect_true(ref_ganp$IS_VALID_SAMPLE)
  expect_false(ref_pbwg$IS_VALID_SAMPLE)
  expect_true(ref_pbwg$REF_TXXT < ref_ganp$REF_TXXT)
})

test_that("apply_txxt_reference joins reference values and flags missing matches", {
  txxt_samples <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("DEP", "DEP"),
    RWY = c("27R", "27L"),
    STND = c("A1", "B2"),
    BLOCK_TIME = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 11:00:00"), tz = "UTC"),
    TXXT = c(20, 18),
    VALID_TXXT = c(TRUE, TRUE)
  )

  reference_data <- tibble::tibble(
    ICAO = "EGLL",
    PHASE = "DEP",
    RWY = "27R",
    STND = "A1",
    REF_TXXT = 15,
    REF_VARIANT = "icao_ganp_p20",
    REF_PERIOD = "2024",
    MIN_N = 5,
    IS_VALID_SAMPLE = TRUE
  )

  augmented <- apply_txxt_reference(txxt_samples, reference_data)

  expect_equal(augmented$REF_TXXT, c(15, NA))
  expect_equal(augmented$ADD_TXXT, c(5, NA))
  expect_equal(augmented$TX_NA, c(FALSE, TRUE))
})

test_that("summarise_pbwg_txxt_daily aggregates totals without averaging", {
  augmented_txxt <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL"),
    PHASE = c("DEP", "DEP", "ARR"),
    DATE = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
    TXXT = c(20, 18, 12),
    REF_TXXT = c(15, NA, 10),
    ADD_TXXT = c(5, NA, 2),
    TX_NA = c(FALSE, TRUE, FALSE),
    VALID_TXXT = c(TRUE, TRUE, TRUE)
  )

  summary_data <- summarise_pbwg_txxt_daily(augmented_txxt)

  dep_row <- dplyr::filter(summary_data, .data$PHASE %in% "DEP")
  arr_row <- dplyr::filter(summary_data, .data$PHASE %in% "ARR")

  expect_equal(dep_row$MVTS, 2L)
  expect_equal(dep_row$TOT_REF, 15)
  expect_equal(dep_row$TOT_ADD_TIME, 5)
  expect_equal(dep_row$TX_NA, 1L)
  expect_equal(arr_row$TOT_REF, 10)
  expect_equal(arr_row$TOT_ADD_TIME, 2)
})

test_that("summarise_txxt_reference_input_quality counts unknown runway and stand", {
  txxt_samples <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("ARR", "ARR"),
    RWY_KNOWN = c(TRUE, FALSE),
    STND_KNOWN = c(FALSE, TRUE),
    VALID_TXXT = c(TRUE, FALSE),
    DIRECTION_OK = c(TRUE, FALSE),
    EXTREME_TXXT = c(FALSE, TRUE),
    NONPOSITIVE_TXXT = c(FALSE, FALSE)
  )

  quality <- summarise_txxt_reference_input_quality(txxt_samples)

  expect_equal(quality$N_TOTAL, 2L)
  expect_equal(quality$N_UNKNOWN_RWY, 1L)
  expect_equal(quality$N_UNKNOWN_STND, 1L)
  expect_equal(quality$N_DIRECTION_ISSUES, 1L)
  expect_equal(quality$N_EXTREME_TXXT, 1L)
})

test_that("build_pbwg_txxt_reference_filename encodes variant and threshold", {
  expect_equal(
    build_pbwg_txxt_reference_filename(
      airport = "EGLL",
      ref_period = 2024,
      variant = "icao_ganp_p20",
      min_n = 5
    ),
    "PBWG-EUR-EGLL-ref-txxt-2024-icao_ganp_p20-n5.csv"
  )
})

test_that("create_pbwg_txxt_reference_annual_file writes airport reference files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP_ICAO,ADES_ICAO,AP_C_RWY,AP_C_STND,MVT_TIME_UTC,BLOCK_TIME_UTC,SRC_PHASE,SRC_AIRPORT",
      "EGLL,KJFK,27R,A1,2024-01-01 10:15:00,2024-01-01 10:00:00,DEP,EGLL",
      "EGLL,KJFK,27R,A1,2024-01-02 10:16:00,2024-01-02 10:00:00,DEP,EGLL",
      "EGLL,KJFK,27R,A1,2024-01-03 10:17:00,2024-01-03 10:00:00,DEP,EGLL",
      "EGLL,KJFK,27R,A1,2024-01-04 10:18:00,2024-01-04 10:00:00,DEP,EGLL",
      "EGLL,KJFK,27R,A1,2024-01-05 10:19:00,2024-01-05 10:00:00,DEP,EGLL"
    ),
    file.path(tmp_dir, "EGLL_2024.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2024.zip", files = "EGLL_2024.csv")

  refs <- create_pbwg_txxt_reference_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2024.zip"),
    ref_year = 2024,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv",
    variant = "icao_ganp_p20",
    min_n = 5
  )

  expect_true(fs::file_exists(unname(refs["EGLL"])))
})

test_that("create_pbwg_txxt_annual_file writes daily and augmented airport files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP_ICAO,ADES_ICAO,AP_C_RWY,AP_C_STND,MVT_TIME_UTC,BLOCK_TIME_UTC,SRC_PHASE,SRC_AIRPORT",
      "EGLL,KJFK,27R,A1,2025-01-01 10:15:00,2025-01-01 10:00:00,DEP,EGLL",
      "EGLL,KJFK,27R,A1,2025-01-02 10:16:00,2025-01-02 10:00:00,DEP,EGLL",
      "KJFK,EGLL,27L,B2,2025-01-03 09:50:00,2025-01-03 10:00:00,ARR,EGLL"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2025.zip", files = "EGLL_2025.csv")

  reference_data <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("DEP", "ARR"),
    RWY = c("27R", "27L"),
    STND = c("A1", "B2"),
    N = c(10L, 8L),
    REF_TXXT = c(14, 9),
    REF_START = as.POSIXct(rep("2024-01-01 00:00:00", 2), tz = "UTC"),
    REF_END = as.POSIXct(rep("2024-12-31 23:59:59", 2), tz = "UTC"),
    REF_PERIOD = "2024",
    REF_VARIANT = "icao_ganp_p20",
    MIN_N = 5L,
    IS_VALID_SAMPLE = TRUE
  )

  outputs <- create_pbwg_txxt_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2025.zip"),
    year = 2025,
    reference_data = reference_data,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv",
    save_augmented = TRUE,
    augmented_dir = tmp_dir
  )

  expect_true(fs::file_exists(unname(outputs$daily_paths["EGLL"])))
  expect_true(fs::file_exists(unname(outputs$augmented_paths["EGLL"])))

  written_daily <- readr::read_csv(unname(outputs$daily_paths["EGLL"]), show_col_types = FALSE)
  expect_true(all(c("ICAO", "PHASE", "DATE", "MVTS", "TOT_REF", "TOT_ADD_TIME", "TX_NA") %in% names(written_daily)))
})
