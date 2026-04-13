test_that("calc_throughput aggregates movements by airport and time bin", {
  apdf <- tibble::tibble(
    ADEP = c("EGLL", "EGLL", "LGAV"),
    ADES = c("LFPG", "EGLL", "LGAV"),
    PHASE = c("DEP", "ARR", "DEP"),
    MVT_TIME = as.POSIXct(
      c("2025-01-01 10:03:00", "2025-01-01 10:17:00", "2025-01-01 10:11:00"),
      tz = "UTC"
    )
  )

  throughput <- calc_throughput(apdf, unit = "hour")

  expect_equal(nrow(throughput), 2)
  expect_equal(throughput$FLTS, c(2L, 1L))
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
      "FLT2,LGAV,EDDF,ARR,LGAV,2025-01-01 10:12:00,2025-01-01 10:12:00,2025-01-01 10:00:00"
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
