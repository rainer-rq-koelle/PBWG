test_that("prepare_tma_reference_input computes terminal-airspace durations by phase and range", {
  tma_input <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL"),
    PHASE = c("DEP", "ARR", "ARR"),
    ADEP = c("EGLL", "LFPG", "LFPG"),
    ADES = c("KJFK", "EGLL", "EGLL"),
    CLASS = c("HJ", "MJ", "HEL"),
    RWY = c("27R", "27L", "27L"),
    MVT_TIME = as.POSIXct(
      c("2024-01-01 10:00:00", "2024-01-01 10:20:00", "2024-01-01 11:00:00"),
      tz = "UTC"
    ),
    C40_CROSS_TIME = as.POSIXct(
      c("2024-01-01 10:10:00", "2024-01-01 10:05:00", "2024-01-01 10:50:00"),
      tz = "UTC"
    ),
    C40_BEARING = c(45, 120, 150),
    C100_CROSS_TIME = as.POSIXct(
      c("2024-01-01 10:20:00", "2024-01-01 09:50:00", "2024-01-01 10:35:00"),
      tz = "UTC"
    ),
    C100_BEARING = c(60, 135, 160)
  )

  prepared <- prepare_tma_reference_input(tma_input)

  dep_40 <- dplyr::filter(prepared, .data$PHASE %in% "DEP", .data$RANGE_NM == 40)
  dep_100 <- dplyr::filter(prepared, .data$PHASE %in% "DEP", .data$RANGE_NM == 100)
  arr_40 <- dplyr::filter(prepared, .data$PHASE %in% "ARR", .data$RANGE_NM == 40)

  expect_equal(dep_40$TMA_TIME, 10)
  expect_equal(dep_100$TMA_TIME, 20)
  expect_equal(arr_40$TMA_TIME, c(15, 10))
  expect_equal(dep_40$CLASS, "H")
  expect_equal(arr_40$CLASS, c("MJ", NA))
  expect_equal(arr_40$VALID_TMA, c(TRUE, FALSE))
})

test_that("assign_tma_sector supports north-wrapping sector definitions", {
  tma_samples <- tibble::tibble(
    ICAO = c("EDDM", "EDDM"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    BEARING = c(350, 100)
  )

  sector_definitions <- tibble::tibble(
    ICAO = c("EDDM", "EDDM"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    SECTOR = c("BRG330-030", "BRG030-150"),
    BEARING_FROM = c(330, 30),
    BEARING_TO = c(30, 150)
  )

  assigned <- assign_tma_sector(tma_samples, sector_definitions)

  expect_equal(assigned$SECTOR, c("BRG330-030", "BRG030-150"))
})

test_that("prepare_tma_sector_plot_input filters to one airport-phase-range", {
  tma_samples <- tibble::tibble(
    ICAO = c("EDDM", "EDDM", "EGLL"),
    PHASE = c("ARR", "DEP", "ARR"),
    RANGE_NM = c(100, 100, 40),
    BEARING = c(350, 100, 120),
    VALID_TMA = c(TRUE, TRUE, TRUE)
  )

  sector_definitions <- tibble::tibble(
    ICAO = c("EDDM", "EDDM"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    SECTOR = c("BRG330-030", "BRG030-150"),
    BEARING_FROM = c(330, 30),
    BEARING_TO = c(30, 150)
  )

  plot_input <- prepare_tma_sector_plot_input(
    tma_samples = tma_samples,
    sector_definitions = sector_definitions,
    airport = "EDDM",
    phase = "ARR",
    range_nm = 100
  )

  expect_equal(nrow(plot_input$samples), 1L)
  expect_equal(nrow(plot_input$sectors), 2L)
})

test_that("build_tma_reference supports both variants and validity flags", {
  tma_samples <- tibble::tibble(
    ICAO = rep("EGLL", 6),
    PHASE = rep("ARR", 6),
    RANGE_NM = rep(100, 6),
    CLASS = rep("MJ", 6),
    RWY = rep("27L", 6),
    SECTOR = rep("BRG300-060", 6),
    SECTOR_LABEL = rep("BRG300-060", 6),
    BEARING_FROM = rep(300, 6),
    BEARING_TO = rep(60, 6),
    MVT_TIME = as.POSIXct(
      c("2024-01-01 10:00:00", "2024-01-02 10:00:00", "2024-01-03 10:00:00",
        "2024-01-04 10:00:00", "2024-01-05 10:00:00", "2024-01-06 10:00:00"),
      tz = "UTC"
    ),
    TMA_TIME = c(10, 12, 14, 16, 18, 20),
    VALID_TMA = TRUE,
    RWY_KNOWN = TRUE,
    CLASS_KNOWN = TRUE
  )

  ref_ganp <- build_tma_reference(
    tma_samples = tma_samples,
    ref_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ref_end = as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
    variant = "icao_ganp_p20",
    min_n = 5
  )

  ref_pbwg <- build_tma_reference(
    tma_samples = tma_samples,
    ref_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    ref_end = as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
    variant = "pbwg_avg_p05_p15",
    min_n = 10
  )

  expect_equal(ref_ganp$N, 6L)
  expect_true(ref_ganp$IS_VALID_SAMPLE)
  expect_false(ref_pbwg$IS_VALID_SAMPLE)
  expect_true(ref_pbwg$REF_TMA < ref_ganp$REF_TMA)
})

test_that("apply_tma_reference joins reference values and summarise_pbwg_tma_daily aggregates totals", {
  tma_samples <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    CLASS = c("MJ", "MJ"),
    RWY = c("27L", "27R"),
    SECTOR = c("BRG300-060", "BRG060-180"),
    MVT_TIME = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 11:00:00"), tz = "UTC"),
    TMA_TIME = c(20, 18),
    VALID_TMA = c(TRUE, TRUE)
  )

  reference_data <- tibble::tibble(
    ICAO = "EGLL",
    PHASE = "ARR",
    RANGE_NM = 100,
    CLASS = "MJ",
    RWY = "27L",
    SECTOR = "BRG300-060",
    REF_TMA = 15,
    REF_VARIANT = "icao_ganp_p20",
    REF_PERIOD = "2024",
    MIN_N = 5,
    IS_VALID_SAMPLE = TRUE
  )

  augmented <- apply_tma_reference(tma_samples, reference_data)
  summary_data <- summarise_pbwg_tma_daily(augmented)

  expect_equal(augmented$REF_TMA, c(15, NA))
  expect_equal(augmented$ADD_TMA, c(5, NA))
  expect_equal(augmented$TMA_NA, c(FALSE, TRUE))
  expect_equal(summary_data$MVTS, 2L)
  expect_equal(summary_data$TOT_TMA_TIME, 38)
  expect_equal(summary_data$TOT_REF, 15)
  expect_equal(summary_data$TOT_ADD_TIME, 5)
  expect_equal(summary_data$TMA_NA, 1L)
})

test_that("write_tma_sector_diagnostic_plots writes one pdf per sector group", {
  tmp_dir <- withr::local_tempdir()

  tma_samples <- tibble::tibble(
    ICAO = c("EDDM", "EDDM"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    BEARING = c(350, 100),
    VALID_TMA = c(TRUE, TRUE)
  )

  sector_definitions <- tibble::tibble(
    ICAO = c("EDDM", "EDDM"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(100, 100),
    SECTOR = c("BRG330-030", "BRG030-150"),
    BEARING_FROM = c(330, 30),
    BEARING_TO = c(30, 150)
  )

  paths <- write_tma_sector_diagnostic_plots(
    tma_samples = tma_samples,
    sector_definitions = sector_definitions,
    output_dir = tmp_dir
  )

  expect_equal(length(paths), 1L)
  expect_true(fs::file_exists(unname(paths[[1]])))
})

test_that("create_pbwg_tma_reference_annual_file writes airport reference files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP_ICAO,ADES_ICAO,AC_CLASS,AP_C_RWY,MVT_TIME_UTC,C40_CROSS_TIME,C40_BEARING,C100_CROSS_TIME,C100_BEARING,SRC_PHASE,SRC_AIRPORT",
      "LFPG,EGLL,MJ,27L,2024-01-01 10:20:00,2024-01-01 10:05:00,120,2024-01-01 09:50:00,135,ARR,EGLL",
      "LFPG,EGLL,MJ,27L,2024-01-02 10:20:00,2024-01-02 10:04:00,122,2024-01-02 09:49:00,136,ARR,EGLL",
      "LFPG,EGLL,MJ,27L,2024-01-03 10:20:00,2024-01-03 10:03:00,121,2024-01-03 09:48:00,137,ARR,EGLL",
      "LFPG,EGLL,MJ,27L,2024-01-04 10:20:00,2024-01-04 10:02:00,123,2024-01-04 09:47:00,138,ARR,EGLL",
      "LFPG,EGLL,MJ,27L,2024-01-05 10:20:00,2024-01-05 10:01:00,124,2024-01-05 09:46:00,139,ARR,EGLL"
    ),
    file.path(tmp_dir, "EGLL_2024.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2024.zip", files = "EGLL_2024.csv")

  sector_definitions <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(40, 100),
    SECTOR = c("BRG090-180", "BRG090-180"),
    BEARING_FROM = c(90, 90),
    BEARING_TO = c(180, 180)
  )

  refs <- create_pbwg_tma_reference_annual_file(
    zipped_archive_path = file.path(tmp_dir, "apdf_2024.zip"),
    ref_year = 2024,
    sector_definitions = sector_definitions,
    output_dir = tmp_dir,
    airports = "EGLL",
    type = "csv",
    variant = "icao_ganp_p20",
    min_n = 5
  )

  expect_true(fs::file_exists(unname(refs["EGLL"])))
})

test_that("create_pbwg_tma_annual_file writes daily and augmented airport files", {
  tmp_dir <- withr::local_tempdir()

  writeLines(
    c(
      "ADEP_ICAO,ADES_ICAO,AC_CLASS,AP_C_RWY,MVT_TIME_UTC,C40_CROSS_TIME,C40_BEARING,C100_CROSS_TIME,C100_BEARING,SRC_PHASE,SRC_AIRPORT",
      "LFPG,EGLL,MJ,27L,2025-01-01 10:20:00,2025-01-01 10:05:00,120,2025-01-01 09:50:00,135,ARR,EGLL",
      "LFPG,EGLL,MJ,27L,2025-01-02 10:22:00,2025-01-02 10:04:00,121,2025-01-02 09:49:00,136,ARR,EGLL"
    ),
    file.path(tmp_dir, "EGLL_2025.csv")
  )

  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "apdf_2025.zip", files = "EGLL_2025.csv")

  reference_data <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(40, 100),
    CLASS = c("MJ", "MJ"),
    RWY = c("27L", "27L"),
    SECTOR = c("BRG090-180", "BRG090-180"),
    SECTOR_LABEL = c("BRG090-180", "BRG090-180"),
    BEARING_FROM = c(90, 90),
    BEARING_TO = c(180, 180),
    N = c(10L, 10L),
    REF_TMA = c(15, 30),
    REF_START = as.POSIXct(rep("2024-01-01 00:00:00", 2), tz = "UTC"),
    REF_END = as.POSIXct(rep("2024-12-31 23:59:59", 2), tz = "UTC"),
    REF_PERIOD = "2024",
    REF_VARIANT = "icao_ganp_p20",
    MIN_N = 5L,
    IS_VALID_SAMPLE = TRUE
  )

  outputs <- create_pbwg_tma_annual_file(
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
  expect_true(all(c("ICAO", "PHASE", "RANGE_NM", "DATE", "MVTS", "TOT_REF", "TOT_ADD_TIME", "TMA_NA") %in% names(written_daily)))
})
