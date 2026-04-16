test_that("list_nm_flight_archives returns zip files from a directory", {
  tmp_dir <- withr::local_tempdir()
  file.create(file.path(tmp_dir, "nm_2023.zip"))
  file.create(file.path(tmp_dir, "ignore.txt"))

  archives <- list_nm_flight_archives(tmp_dir)

  expect_length(archives, 1)
  expect_true(grepl("nm_2023\\.zip$", archives))
})

test_that("read_nm_flights_zip delegates to zip reader", {
  tmp_dir <- withr::local_tempdir()
  csv_path <- file.path(tmp_dir, "sample.csv")

  writeLines("ADEP,ADES,LOBT,WK_TBL_CAT\nEGLL,KJFK,2023-01-01 10:00:00,H", csv_path)
  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "sample.zip", files = "sample.csv")

  data <- read_nm_flights_zip(file.path(tmp_dir, "sample.zip"), type = "csv")

  expect_s3_class(data, "tbl_df")
  expect_named(data, c("ADEP", "ADES", "LOBT", "WK_TBL_CAT"))
})

test_that("prepare_nm_regional_traffic aggregates daily traffic counts", {
  nm_flights <- tibble::tibble(
    LOBT = as.POSIXct(
      c("2023-01-01 10:00:00", "2023-01-01 11:00:00", "2023-01-01 12:00:00"),
      tz = "UTC"
    ),
    ADEP = c("EGLL", "KJFK", "EGLL"),
    ADES = c("KJFK", "LGAV", "LFPG"),
    WK_TBL_CAT = c("H", "M", NA_character_),
    AIRCRAFT_OPERATOR = c("BAW", "UPS", "DLH"),
    AIRCRAFT_TYPE_ICAO_ID = c("A320", "B748", "E190"),
    ICAO_FLT_TYPE = c("S", "N", "S"),
    AIRCRAFT_ID = c("BAW1", "UPS2", "DLH3"),
    REGISTRATION = c("GBUSY", "N1234", "DTEST")
  )

  rules <- list(
    low_cost_operators = "EZY",
    regional_types = "E190",
    business_types = "F2TH",
    cargo_rules = tibble::tibble(
      OPERATOR = "UPS",
      AC_TYPE = "ALL",
      CALLSIGN = "ALL",
      REGISTRATION = "ALL"
    )
  ) |>
    PBWG:::add_prepared_cargo_sets()

  summary <- prepare_nm_regional_traffic(nm_flights, market_segment_rules = rules)

  expect_equal(summary$FLTS, 3L)
  expect_equal(summary$D, 1L)
  expect_equal(summary$A, 1L)
  expect_equal(summary$I, 1L)
  expect_equal(summary$O, 0L)
  expect_equal(summary$H, 1L)
  expect_equal(summary$M, 1L)
  expect_equal(summary$NN, 1L)
  expect_equal(summary$MAINLINE, 1L)
  expect_equal(summary$ALL_CARGO, 1L)
  expect_equal(summary$REGIONAL, 1L)
  expect_equal(summary$SCHED, 2L)
  expect_equal(summary$CARGO, 1L)
})

test_that("classify_nm_market_segment applies STATFOR-style priorities", {
  nm_flights <- tibble::tibble(
    AIRCRAFT_OPERATOR = c("RCH", "ZZZ", "UPS", "EZY", "DLH", "DLH", "ABC", "HOP"),
    OPERATING_AIRCRAFT_OPERATOR = c(NA, NA, NA, NA, NA, NA, NA, NA),
    AIRCRAFT_TYPE_ICAO_ID = c("C130", "F2TH", "B748", "A320", "E190", "A320", "A320", "H500"),
    ICAO_FLT_TYPE = c("M", "G", "N", "S", "S", "S", "N", "N"),
    AIRCRAFT_ID = c("RCH1", "NJE1", "UPS2", "EZY3", "DLH4", "DLH5", "ABC6", "HOP7"),
    REGISTRATION = paste0("REG", seq_len(8))
  )

  rules <- list(
    low_cost_operators = "EZY",
    regional_types = "E190",
    business_types = "F2TH",
    cargo_rules = tibble::tibble(
      OPERATOR = "UPS",
      AC_TYPE = "ALL",
      CALLSIGN = "ALL",
      REGISTRATION = "ALL"
    )
  ) |>
    PBWG:::add_prepared_cargo_sets()

  expect_equal(
    classify_nm_market_segment(nm_flights, rules = rules),
    c(
      "Military",
      "Business Aviation",
      "All-Cargo",
      "Low-Cost",
      "Regional",
      "Mainline",
      "Charter",
      NA_character_
    )
  )
})

test_that("prepare_nm_regional_traffic_zip aggregates multiple archived files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP,ADES,LOBT,WK_TBL_CAT",
      "EGLL,KJFK,2023-01-01 10:00:00,H",
      "KJFK,LGAV,2023-01-01 11:00:00,M"
    ),
    file.path(tmp_dir, "part1.csv")
  )

  writeLines(
    c(
      "ADEP,ADES,LOBT,WK_TBL_CAT",
      "EGLL,LFPG,2023-01-01 12:00:00,L",
      "KJFK,OMDB,2023-01-02 06:00:00,J"
    ),
    file.path(tmp_dir, "part2.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "nm.zip", files = c("part1.csv", "part2.csv"))

  summary <- prepare_nm_regional_traffic_zip(
    zipped_archive_path = file.path(tmp_dir, "nm.zip"),
    type = "csv"
  )

  expect_equal(nrow(summary), 2)
  expect_equal(summary$FLTS, c(3L, 1L))
  expect_equal(summary$D, c(1L, 0L))
  expect_equal(summary$A, c(1L, 0L))
  expect_equal(summary$I, c(1L, 0L))
  expect_equal(summary$O, c(0L, 1L))
  expect_equal(summary$H, c(1L, 0L))
  expect_equal(summary$M, c(1L, 0L))
  expect_equal(summary$L, c(1L, 0L))
  expect_equal(summary$J, c(0L, 1L))
})

test_that("write_pbwg_network_traffic writes to an explicit directory", {
  tmp_dir <- withr::local_tempdir()
  data <- tibble::tibble(DATE = as.Date("2023-01-01"), FLTS = 1)

  output_path <- write_pbwg_network_traffic(
    data = data,
    year = 2023,
    output_dir = tmp_dir,
    region = "EUR"
  )

  expect_true(fs::file_exists(output_path))
  expect_true(grepl("PBWG-EUR-network-traffic-2023\\.csv$", output_path))
})

test_that("build_pbwg_network_traffic_filename supports annual and multi-year files", {
  expect_equal(
    build_pbwg_network_traffic_filename(2024),
    "PBWG-EUR-network-traffic-2024.csv"
  )

  expect_equal(
    build_pbwg_network_traffic_filename(c(2021, 2024)),
    "PBWG-EUR-network-traffic-2021-2024.csv"
  )
})

test_that("create_pbwg_network_traffic_annual_file writes canonical annual output", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP,ADES,LOBT,WK_TBL_CAT",
      "EGLL,KJFK,2023-01-01 10:00:00,H",
      "KJFK,LGAV,2023-01-02 11:00:00,M"
    ),
    file.path(tmp_dir, "part1.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "nm.zip", files = "part1.csv")

  output_path <- create_pbwg_network_traffic_annual_file(
    zipped_archive_path = file.path(tmp_dir, "nm.zip"),
    year = 2023,
    output_dir = tmp_dir,
    type = "csv"
  )

  expect_true(fs::file_exists(output_path))
  expect_true(grepl("PBWG-EUR-network-traffic-2023\\.csv$", output_path))
})

test_that("combine_pbwg_network_traffic_years creates a multi-year file", {
  tmp_dir <- withr::local_tempdir()

  readr::write_csv(
    tibble::tibble(DATE = as.Date("2023-01-01"), FLTS = 1, REG = "EUR"),
    file.path(tmp_dir, "PBWG-EUR-network-traffic-2023.csv")
  )

  readr::write_csv(
    tibble::tibble(DATE = as.Date("2024-01-01"), FLTS = 2, REG = "EUR"),
    file.path(tmp_dir, "PBWG-EUR-network-traffic-2024.csv")
  )

  output_path <- combine_pbwg_network_traffic_years(
    years = c(2023, 2024),
    annual_dir = tmp_dir
  )

  combined <- readr::read_csv(output_path, show_col_types = FALSE)

  expect_true(fs::file_exists(output_path))
  expect_true(grepl("PBWG-EUR-network-traffic-2023-2024\\.csv$", output_path))
  expect_equal(nrow(combined), 2)
})
