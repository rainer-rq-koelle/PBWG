source(test_path("..", "lib", "tma-coverage.R"), local = TRUE)

test_that("coverage separates diagnostic matches, absent references and unavailable crossings", {
  time <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  raw <- tibble::tibble(AP_C_FLTID = as.character(1:8), ADEP_ICAO = "EGLL", ADES_ICAO = "LFPG",
    AC_CLASS = c("MJ", "MJ", "MJ", "HEL", "MJ", "MJ", "MJ", "MJ"),
    AP_C_RWY = c("27R", "NA", "09", "27R", "27R", "27R", NA, "27R"),
    SRC_PHASE = "DEP", MVT_TIME_UTC = time, BLOCK_TIME_UTC = time - 600,
    C40_CROSS_TIME = time + c(600, 600, 600, 600, NA, -60, 600, 600),
    C100_CROSS_TIME = time + c(1200, 1200, 1200, 1200, NA, -60, 1200, 1200),
    C40_BEARING = 90, C100_BEARING = 90)
  raw$MVT_TIME_UTC[8] <- NA
  sectors <- tibble::tibble(ICAO = "EGLL", PHASE = "DEP", RANGE_NM = c(40L, 100L),
    SECTOR = "EAST", BEARING_FROM = 0, BEARING_TO = 180)
  refs <- tidyr::expand_grid(ICAO = "EGLL", PHASE = "DEP", RANGE_NM = c(40L, 100L),
    CLASS = "MJ", RWY = c("27R", "NA"), SECTOR = "EAST") |>
    dplyr::mutate(REF_N = 4L, REF_TMA = 8)
  samples <- tma_coverage_samples(raw, "EGLL", sectors, refs)
  x <- tma_coverage_counts(samples, "RANGE_NM") |> tma_coverage_rates()
  expect_equal(x$N_REPORTED, c(8L, 8L))
  expect_equal(x$N_VALID, c(3L, 3L))
  expect_equal(x$N_MATCHED_UNIDENTIFIED, c(1L, 1L))
  expect_equal(x$N_MISS_IDENTIFIED, c(1L, 1L))
  expect_equal(x$N_MISSING_CROSSING, c(1L, 1L))
  expect_equal(x$N_BAD_TIMING, c(1L, 1L))
  expect_equal(x$N_DATA_ATTENTION, c(4L, 4L))
  expect_equal(x$N_HELICOPTER, c(1L, 1L))
  expect_equal(x$PCT_SMALL_REF_5_MATCHED, c(100, 100))
  expect_false(anyNA(dplyr::select(x, starts_with("N_"))))
  expect_true(all(!samples$DATA_ATTENTION[samples$ROW_ID %in% c(4, 5)]))
  expect_true(all(!is.na(samples$OBS_DATE[samples$ROW_ID == 8])))
})

test_that("review bands include boundary values and empty denominators remain unknown", {
  expect_equal(tma_review_band(c(0, 1.999, 2, 4.999, 5, 9.999, 10, NA)),
    c("LOW", "LOW", "WATCH", "WATCH", "REVIEW", "REVIEW", "SUBSTANTIAL", "NO_DENOMINATOR"))
})
