test_that("prepare_apdf_traffic_input trims, decodes, and adds DATE", {
  apdf <- tibble::tibble(
    AP_C_FLTID = "FLT1",
    ADEP_ICAO = "EGLL",
    ADES_ICAO = "KJFK",
    AC_CLASS = "H",
    SRC_PHASE = "DEP",
    SRC_AIRPORT = "EGLL",
    BLOCK_TIME_UTC = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    MVT_TIME_UTC = as.POSIXct("2025-01-01 10:10:00", tz = "UTC")
  )

  prepared <- prepare_apdf_traffic_input(apdf)

  expect_true(all(c("ICAO", "FLTID", "ADEP", "ADES", "CLASS", "PHASE", "DATE") %in% names(prepared)))
  expect_equal(prepared$ICAO, "EGLL")
  expect_equal(prepared$DATE, as.Date("2025-01-01"))
})

test_that("prepare_apdf_daily_traffic aggregates airport daily counts", {
  apdf <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL"),
    DATE = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
    PHASE = c("DEP", "ARR", "DEP"),
    ADEP = c("EGLL", "KJFK", "EGLL"),
    ADES = c("KJFK", "EGLL", "LFPG"),
    CLASS = c("H", "M", "L")
  )

  summary <- prepare_apdf_daily_traffic(apdf)

  expect_equal(summary$ARRS, 1L)
  expect_equal(summary$DEPS, 2L)
  expect_equal(summary$A, 1L)
  expect_equal(summary$D, 1L)
  expect_equal(summary$I, 1L)
  expect_equal(summary$H, 1L)
  expect_equal(summary$M, 1L)
  expect_equal(summary$L, 1L)
})

test_that("prepare_apdf_daily_traffic_zip aggregates multiple airport files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,AC_CLASS,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,MVT_TIME_UTC",
      "FLT1,EGLL,KJFK,H,DEP,EGLL,2025-01-01 10:00:00,2025-01-01 10:10:00"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,AC_CLASS,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,MVT_TIME_UTC",
      "FLT2,LGAV,EDDF,M,DEP,LGAV,2025-01-01 11:00:00,2025-01-01 11:05:00"
    ),
    file.path(tmp_dir, "LGAV_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf.zip", files = c("EGLL_2025.csv", "LGAV_2025.csv"))

  summary <- prepare_apdf_daily_traffic_zip(
    zipped_archive_path = file.path(tmp_dir, "apdf.zip"),
    type = "csv",
    year = 2025
  )

  expect_equal(nrow(summary), 2)
  expect_true(all(c("EGLL", "LGAV") %in% summary$ICAO))
})

test_that("prepare_apdf_daily_traffic rejects mixed-airport input", {
  apdf <- tibble::tibble(
    ICAO = c("EGLL", "LGAV"),
    DATE = as.Date(c("2025-01-01", "2025-01-01")),
    PHASE = c("DEP", "DEP"),
    ADEP = c("EGLL", "LGAV"),
    ADES = c("KJFK", "EDDF"),
    CLASS = c("H", "M")
  )

  expect_error(
    prepare_apdf_daily_traffic(apdf),
    "must contain exactly one derived ICAO value"
  )
})

test_that("split_apdf_by_icao splits prepared APDF data into airport slices", {
  apdf <- tibble::tibble(
    ADEP = c("EGLL", "LGAV"),
    ADES = c("KJFK", "EDDF"),
    PHASE = c("DEP", "DEP"),
    MVT_TIME = as.POSIXct(c("2025-01-01 10:10:00", "2025-01-01 10:20:00"), tz = "UTC"),
    BLOCK_TIME = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:05:00"), tz = "UTC")
  )

  split_data <- split_apdf_by_icao(apdf)

  expect_equal(sort(names(split_data)), c("EGLL", "LGAV"))
  expect_equal(split_data$EGLL$ICAO, "EGLL")
  expect_equal(split_data$LGAV$ICAO, "LGAV")
})

test_that("build_pbwg_airport_traffic_filename supports annual and multi-year files", {
  expect_equal(
    build_pbwg_airport_traffic_filename("EGLL", 2025),
    "PBWG-EUR-EGLL-tfc-2025.csv"
  )

  expect_equal(
    build_pbwg_airport_traffic_filename("EGLL", c(2021, 2025)),
    "PBWG-EUR-EGLL-tfc-2021-2025.csv"
  )
})

test_that("create and combine PBWG airport traffic files work", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,AC_CLASS,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,MVT_TIME_UTC",
      "FLT1,EGLL,KJFK,H,DEP,EGLL,2024-01-01 10:00:00,2024-01-01 10:10:00"
    ),
    file.path(tmp_dir, "EGLL_2024.csv")
  )

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,AC_CLASS,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,MVT_TIME_UTC",
      "FLT2,EGLL,LFPG,M,DEP,EGLL,2025-01-01 11:00:00,2025-01-01 11:05:00"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2024.zip", files = "EGLL_2024.csv")
  utils::zip(zipfile = "apdf_2025.zip", files = "EGLL_2025.csv")

  annual_2024 <- create_pbwg_airport_traffic_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2024.zip"),
    year = 2024,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv"
  )

  annual_2025 <- create_pbwg_airport_traffic_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2025.zip"),
    year = 2025,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv"
  )

  combined <- combine_pbwg_airport_traffic_years(
    airport = "EGLL",
    years = c(2024, 2025),
    annual_dir = tmp_dir
  )

  expect_true(fs::file_exists(unname(annual_2024["EGLL"])))
  expect_true(fs::file_exists(unname(annual_2025["EGLL"])))
  expect_true(fs::file_exists(combined))
  expect_true(grepl("PBWG-EUR-EGLL-tfc-2024-2025\\.csv$", combined))
})
