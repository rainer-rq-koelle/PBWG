test_that("pbwg_punctuality_groups reproduces the default group labels", {
  groups <- pbwg_punctuality_groups()

  expect_equal(groups[1], "(-INF,-60]")
  expect_equal(groups[13], "(-5,0]")
  expect_equal(groups[14], "(0,5)")
  expect_equal(groups[15], "[5,10)")
  expect_equal(groups[length(groups)], "[60,INF)")
})

test_that("add_delay_and_dlygrp assigns delay bins as expected", {
  apdf <- tibble::tibble(
    BLOCK_TIME = as.POSIXct(
      c("2025-01-01 10:00:00", "2025-01-01 10:05:00", "2025-01-01 11:10:00"),
      tz = "UTC"
    ),
    SCHED_TIME = as.POSIXct(
      c("2025-01-01 10:00:00", "2025-01-01 10:00:00", "2025-01-01 10:00:00"),
      tz = "UTC"
    )
  )

  with_dly <- add_delay_and_dlygrp(apdf)

  expect_equal(with_dly$BLOCK_DLY, c(0, 5, 70))
  expect_equal(with_dly$DLY_GRP, c("(-5,0]", "(0,5)", "[60,INF)"))
})

test_that("add_delay_and_dlygrp handles exact positive boundaries like the legacy output", {
  apdf <- tibble::tibble(
    BLOCK_TIME = as.POSIXct(
      c("2025-01-01 10:05:00", "2025-01-01 10:10:00", "2025-01-01 11:00:00"),
      tz = "UTC"
    ),
    SCHED_TIME = as.POSIXct(
      c("2025-01-01 10:00:00", "2025-01-01 10:00:00", "2025-01-01 10:00:00"),
      tz = "UTC"
    )
  )

  with_dly <- add_delay_and_dlygrp(apdf)

  expect_equal(with_dly$DLY_GRP, c("(0,5)", "[10,15)", "[60,INF)"))
})

test_that("package_pbwg_punctuality returns ordered wide output", {
  punc <- tibble::tibble(
    AERODROME = c("EGLL", "EGLL", "EGLL"),
    BLOCK_TIME = as.POSIXct(
      c("2025-01-01 10:00:00", "2025-01-01 10:05:00", "2025-01-01 10:10:00"),
      tz = "UTC"
    ),
    PHASE = c("DEP", "DEP", "DEP"),
    DLY_GRP = c("(-5,0]", "(0,5)", "[5,10)")
  )

  packaged <- package_pbwg_punctuality(punc)

  expect_true(all(c("AERODROME", "DATE", "PHASE", "N_VALID") %in% names(packaged)))
  expect_equal(packaged$N_VALID, 3)
  expect_equal(packaged$`(-5,0]`, 1)
  expect_equal(packaged$`(0,5)`, 1)
  expect_equal(packaged$`[5,10)`, 1)
})

test_that("pbwg_punctuality_groups can extend the positive tail", {
  groups <- pbwg_punctuality_groups(positive_upper = 120)

  expect_equal(groups[length(groups)], "[120,INF)")
  expect_true("[115,120)" %in% groups)
})

test_that("build_pbwg_punctuality_filename supports airport and project files", {
  expect_equal(
    build_pbwg_punctuality_filename(2025, airport = "EGLL"),
    "PBWG-EUR-EGLL-punc-2025.csv"
  )

  expect_equal(
    build_pbwg_punctuality_filename(2025),
    "PBWG-EUR-punc-2025.csv"
  )
})

test_that("prepare_apdf_punctuality_zip packages punctuality from zipped APDF input", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,SCHED_TIME_UTC,MVT_TIME_UTC",
      "FLT1,EGLL,KJFK,DEP,EGLL,2025-01-01 10:05:00,2025-01-01 10:00:00,2025-01-01 10:05:00"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf.zip", files = "EGLL_2025.csv")

  punc <- prepare_apdf_punctuality_zip(
    zipped_archive_path = file.path(tmp_dir, "apdf.zip"),
    type = "csv",
    year = 2025
  )

  expect_equal(nrow(punc), 1)
  expect_equal(punc$AERODROME, "EGLL")
  expect_equal(punc$`(0,5)`, 1)
})

test_that("create and combine PBWG punctuality files work", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,SCHED_TIME_UTC,MVT_TIME_UTC",
      "FLT1,EGLL,KJFK,DEP,EGLL,2024-01-01 10:05:00,2024-01-01 10:00:00,2024-01-01 10:05:00"
    ),
    file.path(tmp_dir, "EGLL_2024.csv")
  )

  writeLines(
    c(
      "AP_C_FLTID,ADEP_ICAO,ADES_ICAO,SRC_PHASE,SRC_AIRPORT,BLOCK_TIME_UTC,SCHED_TIME_UTC,MVT_TIME_UTC",
      "FLT2,LGAV,EDDF,ARR,LGAV,2025-01-01 10:12:00,2025-01-01 10:00:00,2025-01-01 10:12:00"
    ),
    file.path(tmp_dir, "LGAV_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2024.zip", files = "EGLL_2024.csv")
  utils::zip(zipfile = "apdf_2025.zip", files = "LGAV_2025.csv")

  annual_2024 <- create_pbwg_punctuality_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2024.zip"),
    year = 2024,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv"
  )

  annual_2025 <- create_pbwg_punctuality_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2025.zip"),
    year = 2025,
    output_dir = tmp_dir,
    airports = "LGAV",
    type = "csv"
  )

  combined <- combine_pbwg_punctuality_project(
    airports = c("EGLL", "LGAV"),
    years = c(2024, 2025),
    annual_dir = tmp_dir
  )

  expect_true(fs::file_exists(unname(annual_2024["EGLL"])))
  expect_true(fs::file_exists(unname(annual_2025["LGAV"])))
  expect_true(fs::file_exists(combined))
  expect_true(grepl("PBWG-EUR-punc-2024-2025\\.csv$", combined))
  expect_equal(nrow(readr::read_csv(combined, show_col_types = FALSE)), 2)
})
