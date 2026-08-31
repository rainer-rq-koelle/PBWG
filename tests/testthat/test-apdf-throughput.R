test_that("calc_throughput aggregates movements by airport and time bin", {
  apdf <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("DEP", "ARR"),
    MVT_TIME = as.POSIXct(
      c("2025-01-01 10:03:00", "2025-01-01 10:17:00"),
      tz = "UTC"
    )
  )

  throughput <- calc_throughput(apdf, unit = "hour")

  expect_equal(nrow(throughput), 1)
  expect_equal(throughput$FLTS, 2L)
})

test_that("calc_throughput can aggregate 15-minute runway bins", {
  apdf <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL"),
    PHASE = c("DEP", "ARR", "ARR"),
    RWY = c("27L", "27L", "09R"),
    MVT_TIME = as.POSIXct(
      c("2025-01-01 10:03:00", "2025-01-01 10:14:00", "2025-01-01 10:17:00"),
      tz = "UTC"
    )
  )

  throughput <- calc_throughput(apdf, unit = "15 mins", by_runway = TRUE)

  expect_equal(names(throughput), c("ICAO", "BIN", "RWY", "ARRS", "DEPS", "FLTS"))
  expect_equal(nrow(throughput), 2)
  expect_equal(throughput$RWY, c("27L", "09R"))
  expect_equal(throughput$ARRS, c(1L, 1L))
  expect_equal(throughput$DEPS, c(1L, 0L))
})

test_that("calc_throughput rejects mixed-airport input", {
  apdf <- tibble::tibble(
    ICAO = c("EGLL", "LGAV"),
    PHASE = c("DEP", "DEP"),
    MVT_TIME = as.POSIXct(
      c("2025-01-01 10:03:00", "2025-01-01 10:11:00"),
      tz = "UTC"
    )
  )

  expect_error(
    calc_throughput(apdf, unit = "hour"),
    "must contain exactly one derived ICAO value"
  )
})

test_that("prepare_apdf_throughput_zip packages throughput from zipped APDF input", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,MVT_TIME_UTC,BLOCK_TIME_UTC,SCHED_TIME_UTC",
      "FLT1,EGLL,KJFK,DEP,EGLL,2025-01-01 10:05:00,2025-01-01 10:05:00,2025-01-01 10:00:00"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf.zip", files = "EGLL_2025.csv")

  throughput <- prepare_apdf_throughput_zip(
    zipped_archive_path = file.path(tmp_dir, "apdf.zip"),
    type = "csv",
    year = 2025,
    unit = "hour"
  )

  expect_equal(nrow(throughput), 1)
  expect_equal(throughput$ICAO, "EGLL")
  expect_equal(throughput$FLTS, 1)
})

test_that("build_pbwg_throughput_filename supports airport and project files", {
  expect_equal(
    build_pbwg_throughput_filename(2025, airport = "EGLL"),
    "PBWG-EUR-EGLL-thru-analytic-2025.csv"
  )

  expect_equal(
    build_pbwg_throughput_filename(c(2024, 2025)),
    "PBWG-EUR-thru-analytic-2024-2025.csv"
  )
})

test_that("create and combine PBWG throughput files work", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,MVT_TIME_UTC,BLOCK_TIME_UTC,SCHED_TIME_UTC",
      "FLT1,EGLL,KJFK,DEP,EGLL,2024-01-01 10:05:00,2024-01-01 10:05:00,2024-01-01 10:00:00"
    ),
    file.path(tmp_dir, "EGLL_2024.csv")
  )

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,MVT_TIME_UTC,BLOCK_TIME_UTC,SCHED_TIME_UTC",
      "FLT2,EDDF,LGAV,ARR,LGAV,2025-01-01 10:12:00,2025-01-01 10:12:00,2025-01-01 10:00:00"
    ),
    file.path(tmp_dir, "LGAV_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2024.zip", files = "EGLL_2024.csv")
  utils::zip(zipfile = "apdf_2025.zip", files = "LGAV_2025.csv")

  annual_2024 <- create_pbwg_throughput_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2024.zip"),
    year = 2024,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv",
    unit = "hour"
  )

  annual_2025 <- create_pbwg_throughput_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2025.zip"),
    year = 2025,
    output_dir = tmp_dir,
    airports = "LGAV",
    type = "csv",
    unit = "hour"
  )

  combined <- combine_pbwg_throughput_project(
    airports = c("EGLL", "LGAV"),
    years = c(2024, 2025),
    annual_dir = tmp_dir
  )

  expect_true(fs::file_exists(unname(annual_2024["EGLL"])))
  expect_true(fs::file_exists(unname(annual_2025["LGAV"])))
  expect_true(fs::file_exists(combined))
  expect_true(grepl("PBWG-EUR-thru-analytic-2024-2025\\.csv$", combined))
})

test_that("throughput load helpers derive ordered curves and load indices", {
  throughput <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL", "EGLL"),
    BIN = as.POSIXct(
      c(
        "2025-01-01 10:00:00",
        "2025-01-01 11:00:00",
        "2025-01-02 10:00:00",
        "2025-01-02 11:00:00"
      ),
      tz = "UTC"
    ),
    ARRS = c(10, 4, 8, 1),
    DEPS = c(12, 2, 7, 1)
  )

  capacities <- tibble::tibble(
    APT_ICAO = "EGLL",
    YEAR = 2025,
    MAX_CAP = 20
  )

  loads <- prepare_throughput_load_characteristics(
    throughput = throughput,
    capacities = capacities,
    base_threshold = 0.25,
    peak_threshold = 0.75
  )

  ordered <- prepare_ordered_throughput(loads, years = 2025)
  summary <- summarise_load_indices(loads)

  expect_equal(loads$TOT_THRU, c(22, 6, 15, 2))
  expect_equal(loads$BLI_THR, rep(5, 4))
  expect_equal(loads$PLI_THR, rep(15, 4))
  expect_equal(ordered$RANK, c(1L, 2L, 3L, 4L))
  expect_equal(summary$BLI, 0.75)
  expect_equal(summary$PLI, 0.5)
})
