suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})
source(test_path("..", "lib", "refresh-workflow.R"), local = TRUE)
workflow_project_root <- normalizePath(test_path("..", ".."))

test_that("refresh windows preserve UTC and exclude spillover at both boundaries", {
  data <- tibble(BIN = as.POSIXct(c("2025-12-31 23:45:00", "2026-01-01 00:00:00",
    "2026-09-30 23:59:59", "2026-10-01 00:00:00", NA), tz = "UTC"), YEAR = 2026L)
  expect_equal(nrow(refresh_window(data, "BIN", 2026L, "2026-09-30")), 2L)
  source <- apdf_source(2026, "EGLL", "unused.zip", "2026-10-30")
  coverage <- refresh_coverage(data, "BIN", source, "throughput", 2026, "2026-09-30")
  expect_equal(coverage$N_OUTSIDE_WINDOW, 2)
  expect_equal(coverage$N_MISSING_DATE, 1)
  expect_equal(coverage$OUTPUT_LAST, "2026-09-30")
  expect_equal(coverage$COMPLETENESS, "unconfirmed")
})

test_that("source resolution rejects missing and ambiguous airport years", {
  skip_if_not_installed("withr")
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  file.create(c("EGLL_APDF_2024.parquet", "EGLL_APDF_2024-copy.parquet"))
  archive <- file.path(folder, "source.zip")
  utils::zip(archive, list.files(pattern = "parquet$"))
  source <- apdf_source(2024, "EGLL", archive)
  expect_error(resolve_refresh_sources(source, 2024, c("EGLL", "LPPT")), "exactly one source")
  expect_error(resolve_refresh_sources(bind_rows(source, source), 2024, "EGLL"), "exactly one source")
  expect_error(resolve_refresh_sources(source, 2024, "EGLL"), "found 2")
  source$MEMBER <- "EGLL_APDF_2024.parquet"
  expect_equal(resolve_refresh_sources(source, 2024, "EGLL")$MEMBER, source$MEMBER)
})

test_that("refresh produces separate variants and retains unmatched analysis samples", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("withr")
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  make_archive <- function(year, runway) {
    times <- as.POSIXct(paste0(year, "-06-01 12:00:00"), tz = "UTC") + c(0, 3600)
    raw <- tibble(AP_C_FLTID = c("a", "b"), ADEP_ICAO = c("LFPG", "EGLL"),
      ADES_ICAO = c("EGLL", "LFPG"), AC_CLASS = "M", AP_C_RWY = runway,
      SRC_PHASE = c("ARR", "DEP"), SRC_AIRPORT = "EGLL", MVT_TIME_UTC = times,
      BLOCK_TIME_UTC = times + c(600, -600), SCHED_TIME_UTC = times,
      C40_CROSS_TIME = times + c(-600, 600), C100_CROSS_TIME = times + c(-1200, 1200),
      C40_BEARING = 120, C100_BEARING = 120)
    # A real LFPG source contains one arrival without a destination airport.
    unknown <- raw[1, ]
    unknown$ADES_ICAO <- NA_character_
    raw <- bind_rows(raw, unknown)
    member <- paste0("EGLL_APDF_", year, ".parquet")
    arrow::write_parquet(raw, member)
    archive <- file.path(folder, paste0(year, ".zip"))
    utils::zip(archive, member)
    # Exercise a ZIP reference together with a direct Parquet analysis source.
    apdf_source(year, "EGLL", if (year == 2024) archive else file.path(folder, member))
  }
  sources <- bind_rows(make_archive(2024, "27L"), make_archive(2025, "NEW"))
  output <- file.path(folder, "output")
  config <- list(output_dir = output, products = c("tma", "traffic", "punctuality", "throughput"),
    airports = "EGLL", analysis_years = 2025L, reporting_end = c(`2025` = "2025-12-31"),
    apdf_sources = sources)
  project <- workflow_project_root
  suppressWarnings(run_pbwg_refresh(config, project))
  expect_equal(readLines(file.path(output, "RUN-STATUS.txt")), "COMPLETE")
  quality <- read_csv(file.path(output, "source-quality.csv"), show_col_types = FALSE)
  expect_true(all(quality$N_UNKNOWN_AIRPORT == 1))
  manifest <- read_csv(file.path(output, "manifest.csv"), show_col_types = FALSE)
  refs <- manifest |> filter(.data$PRODUCT == "reference")
  expect_equal(nrow(refs), 2L)
  for (i in seq_len(nrow(refs))) {
    data <- read_csv(file.path(output, refs$FILE[[i]]), show_col_types = FALSE)
    expect_equal(unique(data$REF_VARIANT), refs$REF_VARIANT[[i]])
    expect_true(all(data$N == 1L)) # sparse reference cells must remain usable
  }
  match <- read_csv(file.path(output, "tma-match-coverage.csv"), show_col_types = FALSE)
  expect_equal(sum(match$N_VALID_UNMATCHED), 8)
  daily <- manifest |> filter(.data$PRODUCT == "tma")
  for (i in seq_len(nrow(daily))) {
    data <- read_csv(file.path(output, daily$FILE[[i]]), show_col_types = FALSE)
    expect_equal(unique(data$REF_VARIANT), daily$REF_VARIANT[[i]])
    expect_equal(sum(data$TMA_NA), 4)
  }
  expect_error(run_pbwg_refresh(config, project), "new or empty")
})

test_that("network-only refresh uses its own dates and preserves the exchange schema", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("withr")
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  raw <- tibble(ADEP = "EGLL", ADES = "KJFK", WK_TBL_CAT = "H",
    LOBT = as.POSIXct(c("2025-12-31 12:00:00", "2026-01-01 12:00:00",
                        "2026-09-30 12:00:00", "2026-10-01 12:00:00"), tz = "UTC"),
    AIRCRAFT_OPERATOR = "BAW", AIRCRAFT_TYPE_ICAO_ID = "B744", ICAO_FLT_TYPE = "S",
    AIRCRAFT_ID = "BAW1", REGISTRATION = "TEST")
  arrow::write_parquet(raw, "nm.parquet")
  utils::zip("nm.zip", "nm.parquet")
  writeLines("fixture rules", "rules.txt")
  # Substitute only workbook parsing; ZIP ingestion and NM aggregation are real.
  network_env <- new.env(parent = environment(run_pbwg_refresh))
  network_env$read_statfor_market_segment_rules <- function(path) {
    PBWG:::add_prepared_cargo_sets(list(low_cost_operators = character(),
      regional_types = character(), business_types = character(),
      cargo_rules = tibble(OPERATOR = "UPS", AC_TYPE = "ALL", CALLSIGN = "ALL", REGISTRATION = "ALL")))
  }
  runner <- run_pbwg_refresh
  environment(runner) <- network_env
  config <- list(products = "network", output_dir = file.path(folder, "output"),
    analysis_years = 2026L, reporting_end = c(`2026` = "2026-09-30"),
    nm_sources = nm_source(2026, file.path(folder, "nm.zip"), "2026-10-30"),
    statfor_rules = file.path(folder, "rules.txt"))
  runner(config, workflow_project_root, check_only = TRUE)
  expect_false(dir.exists(config$output_dir))
  runner(config, workflow_project_root)
  data <- read_csv(file.path(config$output_dir, "network-traffic", "PBWG-EUR-network-traffic-2026p.csv"),
                   show_col_types = FALSE)
  expect_equal(data$DATE, as.Date(c("2026-01-01", "2026-09-30")))
  expect_equal(data$REG, c("EUR", "EUR"))
  expect_equal(data$FLTS, c(1, 1))
  arrow::write_parquet(raw[1:2, ], "q1.parquet")
  arrow::write_parquet(raw[3:4, ], "q2.parquet")
  config$nm_sources <- bind_rows(nm_source(2026, file.path(folder, "q1.parquet")),
                                nm_source(2026, file.path(folder, "q2.parquet")))
  config$output_dir <- file.path(folder, "direct-output")
  runner(config, workflow_project_root)
  direct <- read_csv(file.path(config$output_dir, "network-traffic", "PBWG-EUR-network-traffic-2026p.csv"),
                     show_col_types = FALSE)
  expect_equal(direct, data)
  expect_error(prepare_refresh_network(bind_rows(config$nm_sources, config$nm_sources[1, ]),
    network_env$read_statfor_market_segment_rules("unused")), "overlap")
})
