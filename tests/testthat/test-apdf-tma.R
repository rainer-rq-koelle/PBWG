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
  expect_true(assigned$NORTH_OVERRUN[[1]])
  expect_false(assigned$NORTH_OVERRUN[[2]])
})

test_that("approved sector definitions earmark North overrun", {
  approved <- read_approved_tma_sector_definitions(phase = "ARR")
  eddm_wrap <- dplyr::filter(
    approved,
    .data$AIRPORT == "EDDM",
    .data$RANGE_NM == 100,
    .data$BEARING_FROM == 340,
    .data$BEARING_TO == 75
  )

  expect_true("NORTH_OVERRUN" %in% names(approved))
  expect_true(eddm_wrap$NORTH_OVERRUN[[1]])
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

test_that("prepare_tma_bearing_histogram counts complete circular bearing bins", {
  tma_samples <- tibble::tibble(
    ICAO = c("EGLL", "EGLL", "EGLL", "LSZH"),
    PHASE = c("ARR", "ARR", "ARR", "ARR"),
    RANGE_NM = c(40, 40, 100, 40),
    BEARING = c(0, 359.9, 6, 12),
    VALID_TMA = c(TRUE, FALSE, TRUE, TRUE)
  )

  histogram <- prepare_tma_bearing_histogram(
    tma_samples = tma_samples,
    airport = "EGLL",
    phase = "ARR",
    ranges = c(40, 100),
    bearing_bin_width = 6
  )

  expect_equal(nrow(histogram), 120L)
  expect_equal(dplyr::filter(histogram, .data$RANGE_NM == 40, .data$BIN_ID == 1)$N, 1L)
  expect_equal(dplyr::filter(histogram, .data$RANGE_NM == 40, .data$BIN_ID == 60)$N, 1L)
  expect_equal(dplyr::filter(histogram, .data$RANGE_NM == 100, .data$BIN_ID == 2)$N, 1L)
  expect_true(all(dplyr::filter(histogram, .data$RANGE_NM == 40)$N_BEARINGS == 2L))

  valid_histogram <- prepare_tma_bearing_histogram(
    tma_samples = tma_samples,
    airport = "EGLL",
    phase = "ARR",
    ranges = 40,
    bearing_bin_width = 6,
    valid_only = TRUE
  )

  expect_equal(dplyr::filter(valid_histogram, .data$BIN_ID == 60)$N, 0L)
  expect_true(all(valid_histogram$N_BEARINGS == 1L))
})

test_that("prepare_tma_bearing_density wraps the smoother across North", {
  tma_samples <- tibble::tibble(
    ICAO = c("EGLL", "EGLL"),
    PHASE = c("ARR", "ARR"),
    RANGE_NM = c(40, 40),
    BEARING = c(0, 359)
  )

  density <- prepare_tma_bearing_density(
    tma_samples = tma_samples,
    airport = "EGLL",
    phase = "ARR",
    ranges = 40,
    smoothing_bandwidth = 2
  )

  expect_equal(nrow(density), 360L)
  expect_equal(sum(density$SMOOTHED_N), 2, tolerance = 1e-8)
  expect_gt(
    density$SMOOTHED_N[density$BIN_ID == 1],
    density$SMOOTHED_N[density$BIN_ID == 180]
  )
})

test_that("identify_tma_bearing_extrema measures circular peak prominence", {
  density <- tibble::tibble(
    AIRPORT = "EGLL",
    PHASE = "ARR",
    RANGE_NM = 40,
    BIN_ID = 1:8,
    BEARING_MID = seq(22.5, 337.5, by = 45),
    SMOOTHED_N = c(10, 1, 6, 1, 0, 1, 4, 1)
  )

  extrema <- identify_tma_bearing_extrema(
    tma_density = density,
    min_relative_prominence = 0.2
  )

  expect_equal(sum(extrema$EXTREMUM == "PEAK"), 3L)
  expect_equal(sum(extrema$EXTREMUM == "MINIMUM"), 3L)
  expect_true(all(extrema$IS_SUBSTANTIAL))
  expect_equal(
    extrema$RELATIVE_PROMINENCE[extrema$EXTREMUM == "PEAK" & extrema$BEARING == 22.5],
    0.9
  )
})

test_that("propose_tma_sector_definitions rounds valleys and reports support", {
  tma_samples <- tibble::tibble(
    ICAO = rep("EGLL", 20),
    PHASE = rep("ARR", 20),
    RANGE_NM = rep(40, 20),
    BEARING = rep(c(90, 270), each = 10),
    VALID_TMA = TRUE,
    CLASS = rep(c("MJ", "H"), each = 10),
    RWY = rep(c("09L", "27R"), each = 10)
  )

  density <- prepare_tma_bearing_density(
    tma_samples = tma_samples,
    airport = "EGLL",
    phase = "ARR",
    ranges = 40,
    smoothing_bandwidth = 2
  )
  extrema <- identify_tma_bearing_extrema(density, min_relative_prominence = 0.02)
  proposal <- propose_tma_sector_definitions(
    tma_density = density,
    extrema = extrema,
    rounding_increment = 5,
    valley_safety_fraction = 0.25
  )
  support <- summarise_tma_sector_support(tma_samples, proposal$sector_definitions)

  expect_equal(nrow(proposal$sector_definitions), 2L)
  expect_true(all(grepl("^EGLL-ARR-BRG", proposal$sector_definitions$SECTOR_ID)))
  expect_true(all(!proposal$sector_definitions$NORTH_OVERRUN))
  expect_equal(sum(support$sector_summary$N_CROSSINGS), 20L)
  expect_equal(sum(support$reference_cells$N), 20L)
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
  expect_equal(ref_ganp$SECTOR_ID, "EGLL-ARR-BRG300-060")
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
