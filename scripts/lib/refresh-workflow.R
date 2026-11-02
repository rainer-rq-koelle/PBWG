# Operator workflow helpers; deliberately separate from the package API.
pbwg_study_airports <- function() {
  c("EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
    "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM")
}

apdf_source <- function(year, airports, archive, extracted_on = NA_character_,
                        complete_through = NA_character_, member = NA_character_) {
  tibble(YEAR = as.integer(year), ICAO = airports, ARCHIVE = archive,
         MEMBER = member, EXTRACTED_ON = extracted_on, COMPLETE_THROUGH = complete_through)
}

nm_source <- function(year, archive, extracted_on = NA_character_, complete_through = NA_character_) {
  apdf_source(year, "EUR", archive, extracted_on, complete_through)
}

refresh_date <- function(x, what, allow_na = FALSE) {
  result <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  bad <- (!is.na(x) & (is.na(result) | as.character(result) != x)) |
    (!allow_na & is.na(x))
  if (any(bad)) stop(what, " must contain YYYY-MM-DD dates", call. = FALSE)
  result
}

resolve_refresh_sources <- function(sources, years, airports, kind = "APDF") {
  required <- c("YEAR", "ICAO", "ARCHIVE", "MEMBER", "EXTRACTED_ON", "COMPLETE_THROUGH")
  if (!all(required %in% names(sources))) stop(kind, " source table is incomplete.")
  sources <- sources[sources$YEAR %in% years & sources$ICAO %in% airports, ]
  expected <- tidyr::expand_grid(YEAR = as.integer(years), ICAO = airports)
  identity <- if (kind == "APDF") c("YEAR", "ICAO") else c("YEAR", "ICAO", "ARCHIVE", "MEMBER")
  if (anyDuplicated(sources[identity]) ||
      nrow(anti_join(expected, sources, by = c("YEAR", "ICAO")))) {
    stop(kind, " requires exactly one source per selected airport/year (NM may use distinct partitions).")
  }
  if (anyNA(sources$ARCHIVE) || any(!file.exists(sources$ARCHIVE))) {
    stop(kind, " archive(s) missing: ", paste(sources$ARCHIVE[is.na(sources$ARCHIVE) |
      !file.exists(sources$ARCHIVE)], collapse = ", "))
  }
  sources$ARCHIVE <- normalizePath(sources$ARCHIVE, winslash = "/", mustWork = TRUE)
  refresh_date(sources$EXTRACTED_ON, "EXTRACTED_ON", allow_na = TRUE)
  complete <- refresh_date(sources$COMPLETE_THROUGH, "COMPLETE_THROUGH", allow_na = TRUE)
  if (any(!is.na(complete) & format(complete, "%Y") != as.character(sources$YEAR))) {
    stop("COMPLETE_THROUGH must belong to its source YEAR.")
  }
  parquet <- grepl("[.]parquet$", sources$ARCHIVE, ignore.case = TRUE)
  if (any(!parquet & !grepl("[.]zip$", sources$ARCHIVE, ignore.case = TRUE))) {
    stop("Sources must be ZIP archives or Parquet files.")
  }
  if (any(parquet & !is.na(sources$MEMBER))) stop("Parquet sources do not have archive members.")
  if (kind == "NM") {
    for (year in unique(sources$YEAR)) {
      selected <- sources$YEAR == year
      if (sum(selected) > 1L && any(!parquet[selected])) {
        stop("Use one NM ZIP or distinct Parquet partitions for each year, not both.")
      }
    }
  }
  archives <- unique(sources$ARCHIVE[!parquet])
  listings <- lapply(archives, function(path) utils::unzip(path, list = TRUE)$Name)
  names(listings) <- archives
  for (i in seq_len(nrow(sources))) {
    if (parquet[[i]]) next
    members <- listings[[sources$ARCHIVE[[i]]]]
    selected <- sources$MEMBER[[i]]
    if (kind == "APDF" && (is.na(selected) || !nzchar(selected))) {
      hits <- members[grepl(paste0("^", sources$ICAO[[i]], "[_-]"), basename(members)) &
        grepl(paste0("(^|[^0-9])", sources$YEAR[[i]], "([^0-9]|$)"), basename(members)) &
        grepl("[.]parquet$", members, ignore.case = TRUE)]
      if (length(hits) != 1L) stop("Expected one APDF member for ", sources$ICAO[[i]],
        " / ", sources$YEAR[[i]], "; found ", length(hits), ". Set MEMBER explicitly.")
      sources$MEMBER[[i]] <- hits[[1]]
    } else if (!is.na(selected) && !selected %in% members) {
      stop("Archive member not found: ", selected)
    }
  }
  sources
}

read_refresh_apdf <- function(source) {
  if (grepl("[.]parquet$", source$ARCHIVE, ignore.case = TRUE)) {
    arrow::read_parquet(source$ARCHIVE)
  } else read_apdf_zip(source$ARCHIVE, files = source$MEMBER, type = "parquet")
}

prepare_refresh_network <- function(sources, rules) {
  parts <- lapply(seq_len(nrow(sources)), function(i) {
    src <- sources[i, ]
    if (grepl("[.]parquet$", src$ARCHIVE, ignore.case = TRUE)) {
      raw <- arrow::read_parquet(src$ARCHIVE)
      if ("SOURCE_ROWS" %in% names(src) && !is.na(src$SOURCE_ROWS) && nrow(raw) != src$SOURCE_ROWS) {
        stop("NM row count differs from manifest: ", src$ARCHIVE)
      }
      raw <- mutate(raw, across(where(~ inherits(.x, "POSIXt")), ~ lubridate::with_tz(.x, "UTC")))
      prepare_nm_regional_traffic(raw, market_segment_rules = rules)
    } else prepare_nm_regional_traffic_zip(src$ARCHIVE,
      files = if (is.na(src$MEMBER)) NULL else src$MEMBER, type = "parquet",
      airport_classifier = is_ectrl_member_state_airport, market_segment_rules = rules)
  })
  data <- bind_rows(parts)
  if (anyDuplicated(data$DATE)) stop("NM partitions overlap in DATE; select non-overlapping inputs.")
  arrange(data, .data$DATE)
}

validate_refresh_config <- function(config) {
  defaults <- list(airports = pbwg_study_airports(), reference_year = 2024L,
                   save_augmented = FALSE)
  config <- utils::modifyList(defaults, config)
  supported <- c("references", "tma", "traffic", "punctuality", "throughput", "network")
  if (!length(config$products) || any(!config$products %in% supported)) stop("Unknown or empty products.")
  if (!length(config$airports) || anyNA(config$airports) || anyDuplicated(config$airports) ||
      any(!config$airports %in% pbwg_study_airports())) stop("Invalid study airports.")
  # The approved boundaries are specifically based on the 2024 review.
  if (!identical(as.integer(config$reference_year), 2024L)) stop("Approved reference year is 2024.")
  if (!length(config$analysis_years) || anyNA(config$analysis_years) ||
      any(config$analysis_years != as.integer(config$analysis_years)) ||
      anyDuplicated(config$analysis_years)) stop("analysis_years must be distinct integer years.")
  config$analysis_years <- as.integer(config$analysis_years)
  if (is.null(config$reporting_end) || anyDuplicated(names(config$reporting_end)) ||
      !all(as.character(config$analysis_years) %in% names(config$reporting_end))) {
    stop("Set a named reporting_end for every analysis year.")
  }
  dates <- refresh_date(config$reporting_end, "reporting_end")
  if (any(format(dates, "%Y") != names(config$reporting_end))) stop("reporting_end year mismatch.")
  if (length(config$output_dir) != 1 || is.na(config$output_dir) || !nzchar(config$output_dir)) {
    stop("Set output_dir to a new run directory.")
  }
  if (file.exists(config$output_dir) && (!dir.exists(config$output_dir) ||
      length(list.files(config$output_dir, all.files = TRUE, no.. = TRUE)))) {
    stop("output_dir must be new or empty; completed and failed runs are never overwritten.")
  }
  needs_ref <- any(config$products %in% c("references", "tma"))
  needs_apdf <- any(config$products %in% c("traffic", "punctuality", "throughput", "tma"))
  years <- unique(c(if (needs_ref) config$reference_year, if (needs_apdf) config$analysis_years))
  config$apdf_sources <- if (length(years)) {
    resolve_refresh_sources(config$apdf_sources, years, config$airports)
  } else tibble()
  if (needs_ref) {
    ref_sources <- config$apdf_sources[config$apdf_sources$YEAR == 2024L, ]
    if (any(!is.na(ref_sources$COMPLETE_THROUGH) & ref_sources$COMPLETE_THROUGH != "2024-12-31")) {
      stop("A partial 2024 source cannot be used for the full-year reference.")
    }
  }
  if ("network" %in% config$products) {
    config$nm_sources <- resolve_refresh_sources(config$nm_sources, config$analysis_years, "EUR", "NM")
    if (length(config$statfor_rules) != 1 || !file.exists(config$statfor_rules)) {
      stop("Network refresh requires the STATFOR rules workbook.")
    }
  } else config$nm_sources <- tibble()
  config
}

refresh_window <- function(data, column, year, end_date) {
  dates <- as.Date(data[[column]], tz = "UTC")
  start <- as.Date(paste0(year, "-01-01"))
  end <- as.Date(end_date)
  data[!is.na(dates) & dates >= start & dates <= end, , drop = FALSE]
}

refresh_coverage <- function(data, column, source, product, year, end_date) {
  dates <- as.Date(data[[column]], tz = "UTC")
  selected <- refresh_window(data, column, year, end_date)
  kept_dates <- as.Date(selected[[column]], tz = "UTC")
  tibble(PRODUCT = product, YEAR = year, ICAO = source$ICAO,
    EXTRACTED_ON = source$EXTRACTED_ON, COMPLETE_THROUGH = source$COMPLETE_THROUGH,
    COMPLETENESS = ifelse(is.na(source$COMPLETE_THROUGH), "unconfirmed", "source-confirmed"),
    EFFECTIVE_END = as.character(end_date), DATE_BASIS = column,
    OBSERVED_FIRST = if (all(is.na(dates))) NA_character_ else as.character(min(dates, na.rm = TRUE)),
    OBSERVED_LAST = if (all(is.na(dates))) NA_character_ else as.character(max(dates, na.rm = TRUE)),
    OUTPUT_FIRST = if (!length(kept_dates)) NA_character_ else as.character(min(kept_dates)),
    OUTPUT_LAST = if (!length(kept_dates)) NA_character_ else as.character(max(kept_dates)),
    N_INPUT_ROWS = nrow(data), N_OUTPUT_ROWS = nrow(selected),
    N_MISSING_DATE = sum(is.na(dates)), N_OUTSIDE_WINDOW = sum(!is.na(dates)) - nrow(selected),
    N_OBSERVED_DAYS = length(unique(kept_dates)),
    N_DAYS_WITHOUT_OBSERVATIONS = as.integer(as.Date(end_date) - as.Date(paste0(year, "-01-01"))) +
      1L - length(unique(kept_dates)))
}

run_pbwg_refresh <- function(config, project_root, check_only = FALSE) {
  config <- validate_refresh_config(config)
  sectors <- read_approved_tma_sector_definitions(airports = config$airports) |>
    rename(ICAO = "AIRPORT")
  expected <- tidyr::expand_grid(ICAO = config$airports, PHASE = c("ARR", "DEP"), RANGE_NM = c(40, 100))
  if (nrow(anti_join(expected, sectors, by = c("ICAO", "PHASE", "RANGE_NM")))) {
    stop("Approved sectors do not cover all requested airport/phase/range combinations.")
  }
  print(bind_rows(mutate(config$apdf_sources, SOURCE = "APDF"),
                  mutate(config$nm_sources, SOURCE = "NM")), n = Inf)
  if (check_only) {
    message("Preflight passed. No output was written; data contents have not yet been validated.")
    return(invisible(config))
  }
  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(config$output_dir, winslash = "/")
  writeLines("RUNNING", file.path(output, "RUN-STATUS.txt"))
  complete <- FALSE
  on.exit(if (!complete) writeLines("FAILED: outputs are incomplete; do not use this run.",
                                  file.path(output, "RUN-STATUS.txt")), add = TRUE)
  saveRDS(config, file.path(output, "run-config.rds"))
  sources <- bind_rows(mutate(config$apdf_sources, SOURCE = "APDF"),
                       mutate(config$nm_sources, SOURCE = "NM"))
  archive_paths <- unique(sources$ARCHIVE)
  hashes <- tools::md5sum(archive_paths)
  sources$ARCHIVE_MD5 <- unname(hashes[sources$ARCHIVE])
  if ("SOURCE_MD5" %in% names(sources) &&
      any(!is.na(sources$SOURCE_MD5) & sources$SOURCE_MD5 != sources$ARCHIVE_MD5)) {
    stop("Source checksum differs from the pinned R2 manifest.")
  }
  write_csv(sources, file.path(output, "sources.csv"))
  if ("MANIFEST_PATH" %in% names(sources)) {
    manifests <- unique(na.omit(sources$MANIFEST_PATH))
    if (length(manifests)) {
      dir.create(file.path(output, "source-manifests"))
      if (!all(file.copy(manifests, file.path(output, "source-manifests")))) stop("Cannot pin source manifests.")
    }
  }
  write_csv(sectors, file.path(output, "approved-sectors.csv"))
  writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
  metadata_paths <- c(file.path(project_root, "DESCRIPTION"),
    file.path(project_root, "inst", "extdata", c("arr-sectors-approved-2024.parquet", "dep-sectors-approved-2024.parquet")),
    list.files(file.path(project_root, "R"), full.names = TRUE, pattern = "[.]R$"),
    file.path(project_root, "scripts", "lib", "refresh-workflow.R"))
  if ("network" %in% config$products) metadata_paths <- c(metadata_paths, config$statfor_rules)
  write_csv(tibble(FILE = metadata_paths, MD5 = unname(tools::md5sum(metadata_paths))),
            file.path(output, "code-and-method.csv"))
  manifest <- list()
  coverage <- list()
  monthly <- list()
  reference_checks <- list()
  match_checks <- list()
  source_quality <- list()
  aggregates <- list()
  write_product <- function(data, relative, product, year, airport = "ALL", variant = NA_character_) {
    if (!nrow(data)) stop("Empty output: ", relative)
    path <- file.path(output, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write_csv(data, path)
    manifest[[length(manifest) + 1L]] <<- tibble(FILE = relative, PRODUCT = product,
      YEAR = year, ICAO = airport, REF_VARIANT = variant, ROWS = nrow(data),
      MD5 = unname(tools::md5sum(path)))
  }
  observe <- function(data, column, src, product, year, end) {
    requested <- if (product == "reference-input") "2024-12-31" else config$reporting_end[[as.character(year)]]
    coverage[[length(coverage) + 1L]] <<- refresh_coverage(data, column, src, product, year, end) |>
      mutate(REQUESTED_END = requested)
    dates <- as.Date(data[[column]], tz = "UTC")
    monthly[[length(monthly) + 1L]] <<- tibble(MONTH = format(dates, "%Y-%m")) |>
      count(.data$MONTH, name = "N_ROWS") |>
      mutate(PRODUCT = product, YEAR = year, ICAO = src$ICAO, DATE_BASIS = column)
  }
  read_source <- function(src) {
    raw <- read_refresh_apdf(src)
    if ("SOURCE_ROWS" %in% names(src) && !is.na(src$SOURCE_ROWS) && nrow(raw) != src$SOURCE_ROWS) {
      stop("APDF row count differs from manifest: ", src$ARCHIVE)
    }
    # Preserve instants, but use UTC consistently for output date boundaries.
    raw <- mutate(raw, across(where(~ inherits(.x, "POSIXt")), ~ lubridate::with_tz(.x, "UTC")))
    input <- prepare_apdf_traffic_input(raw)
    if (!nrow(input) || any(!is.na(input$ICAO) & input$ICAO != src$ICAO)) {
      stop("Source airport mismatch or empty APDF: ", src$ICAO, " / ", src$YEAR)
    }
    keep <- !is.na(input$ICAO)
    source_quality[[length(source_quality) + 1L]] <<- tibble(
      YEAR = src$YEAR, ICAO = src$ICAO, MEMBER = src$MEMBER,
      N_SOURCE_ROWS = nrow(input), N_UNKNOWN_AIRPORT = sum(!keep), N_RETAINED = sum(keep),
      HAS_UPSTREAM_FLTID = "FLTID" %in% names(raw),
      N_NM_FLTID = if ("FLTID_SOURCE" %in% names(raw)) sum(raw$FLTID_SOURCE == "NM", na.rm = TRUE) else NA_integer_)
    if (!any(keep)) stop("No identifiable airport movements in ", src$MEMBER)
    if (any(!keep)) message("Excluded ", sum(!keep), " row(s) without a derived airport in ", src$MEMBER,
                            "; recorded in source-quality.csv.")
    list(raw = raw[keep, ], input = input[keep, ])
  }
  source_for <- function(year, airport) config$apdf_sources |>
    filter(.data$YEAR == .env$year, .data$ICAO == .env$airport)
  end_for <- function(year, src) {
    requested <- config$reporting_end[[as.character(year)]]
    if (is.na(src$COMPLETE_THROUGH)) requested else min(requested, src$COMPLETE_THROUGH)
  }
  variants <- tma_reference_variants()$REF_VARIANT
  for (airport in config$airports) {
    references <- NULL
    if (any(config$products %in% c("references", "tma"))) {
      message("Building approved 2024 references: ", airport)
      src <- source_for(config$reference_year, airport)
      ref_raw <- read_source(src)
      samples <- prepare_apdf_tma_input(ref_raw$raw) |>
        prepare_tma_reference_input(ranges = c(40, 100), max_tma = 180)
      observe(samples, "MVT_TIME", src, "reference-input", 2024L, "2024-12-31")
      samples <- refresh_window(samples, "MVT_TIME", 2024L, "2024-12-31") |>
        assign_tma_sector(sectors |> filter(.data$ICAO == .env$airport))
      if (any(samples$VALID_TMA & is.na(samples$SECTOR))) stop("Valid reference samples without approved sectors.")
      references <- build_tma_reference(samples,
        as.POSIXct("2024-01-01", tz = "UTC"), as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
        variant = variants)
      if (any(!is.finite(references$REF_TMA)) ||
          nrow(anti_join(expected |> filter(.data$ICAO == .env$airport), references,
                        by = c("ICAO", "PHASE", "RANGE_NM")))) {
        stop("Missing or invalid reference values for ", airport)
      }
      for (variant in variants) {
        ref <- references |> filter(.data$REF_VARIANT == .env$variant)
        ref_check <- check_tma_reference_coverage(ref, samples |> filter(.data$VALID_TMA)) |>
          mutate(REF_VARIANT = variant)
        if (any(ref_check$N_UNMATCHED_GROUPS > 0)) stop("Incomplete valid reference-period coverage: ", airport)
        reference_checks[[length(reference_checks) + 1L]] <- ref_check
        write_product(ref, file.path("airport-tma", "reference-2024",
          build_pbwg_tma_reference_filename(airport, 2024, variant)), "reference", 2024L, airport, variant)
      }
      rm(ref_raw, samples)
    }
    if (!any(config$products %in% c("tma", "traffic", "punctuality", "throughput"))) next
    for (year in config$analysis_years) {
      message("Processing ", airport, " / ", year)
      src <- source_for(year, airport)
      apdf <- read_source(src)
      end <- end_for(year, src)
      label <- if (end < paste0(year, "-12-31")) paste0(year, "p") else as.character(year)
      # Use the same label for all airports in a partial-year project.
      if (any(config$apdf_sources$YEAR == year & !is.na(config$apdf_sources$COMPLETE_THROUGH) &
              config$apdf_sources$COMPLETE_THROUGH < paste0(year, "-12-31"))) label <- paste0(year, "p")
      emit_basic <- function(data, column, family, name) {
        observe(data, column, src, family, year, end)
        data <- refresh_window(data, column, year, end)
        if (column == "BIN") data$BIN <- format(data$BIN, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        write_product(data, file.path(family, name), family, year, airport)
        key <- paste(family, label, sep = "/")
        aggregates[[key]] <<- bind_rows(aggregates[[key]], data)
      }
      if ("traffic" %in% config$products) emit_basic(prepare_apdf_daily_traffic(apdf$input), "DATE",
        "airport-traffic", build_pbwg_airport_traffic_filename(airport, label))
      if ("punctuality" %in% config$products) emit_basic(
        package_pbwg_punctuality(add_delay_and_dlygrp(apdf$input)), "DATE", "airport-punctuality",
        build_pbwg_punctuality_filename(label, airport = airport))
      if ("throughput" %in% config$products) emit_basic(
        calc_throughput(apdf$input, unit = "15 mins", by_runway = TRUE), "BIN", "airport-throughput",
        build_pbwg_throughput_filename(label, airport = airport))
      if ("tma" %in% config$products) {
        samples <- prepare_tma_reference_input(prepare_apdf_tma_input(apdf$raw))
        observe(samples, "MVT_TIME", src, "tma-input", year, end)
        samples <- refresh_window(samples, "MVT_TIME", year, end) |>
          assign_tma_sector(sectors |> filter(.data$ICAO == .env$airport))
        for (variant in variants) {
          # Apply one variant at a time: unmatched movements retain that variant
          # and cannot be lost or counted twice in a multi-variant join.
          augmented <- apply_tma_reference(samples, references |> filter(.data$REF_VARIANT == .env$variant)) |>
            mutate(REF_VARIANT = variant, REF_PERIOD = "2024")
          match_checks[[length(match_checks) + 1L]] <- augmented |>
            summarise(N_TOTAL = n(), N_VALID = sum(.data$VALID_TMA),
              N_VALID_MATCHED = sum(.data$VALID_TMA & .data$HAS_REFERENCE),
              N_VALID_UNMATCHED = sum(.data$VALID_TMA & !.data$HAS_REFERENCE),
              .by = c("ICAO", "PHASE", "RANGE_NM", "REF_VARIANT")) |>
            mutate(YEAR = year)
          daily <- summarise_pbwg_tma_daily(augmented)
          write_product(daily, file.path("airport-tma", paste0("daily-", label),
            build_pbwg_tma_filename(label, 2024, variant, airport)), "tma", year, airport, variant)
          key <- paste("airport-tma", label, variant, sep = "/")
          aggregates[[key]] <- bind_rows(aggregates[[key]], daily)
          if (isTRUE(config$save_augmented)) write_product(augmented,
            file.path("airport-tma", paste0("augmented-", label),
              build_pbwg_tma_augmented_filename(label, airport, 2024, variant)),
            "tma-augmented", year, airport, variant)
          rm(augmented, daily)
        }
        rm(samples)
      }
      rm(apdf)
    }
  }
  for (key in names(aggregates)) {
    pieces <- strsplit(key, "/", fixed = TRUE)[[1]]
    family <- pieces[[1]]
    label <- pieces[[2]]
    variant <- if (length(pieces) == 3) pieces[[3]] else NA_character_
    name <- switch(family,
      "airport-traffic" = paste0("PBWG-EUR-airport-traffic-", label, ".csv"),
      "airport-punctuality" = build_pbwg_punctuality_filename(label),
      "airport-throughput" = build_pbwg_throughput_filename(label),
      "airport-tma" = build_pbwg_tma_filename(label, 2024, variant))
    write_product(aggregates[[key]], file.path(family, name), family,
                  as.integer(sub("p$", "", label)), variant = variant)
  }
  if ("network" %in% config$products) {
    rules <- read_statfor_market_segment_rules(config$statfor_rules)
    for (year in unique(config$nm_sources$YEAR)) {
      year_sources <- config$nm_sources[config$nm_sources$YEAR == year, ]
      src <- year_sources[1, ]
      # A year's completeness cannot be inferred from any one quarter.
      if (nrow(year_sources) > 1L) {
        src$COMPLETE_THROUGH <- NA_character_
        if (length(unique(year_sources$EXTRACTED_ON)) > 1L) src$EXTRACTED_ON <- NA_character_
      }
      end <- end_for(year, src)
      data <- prepare_refresh_network(year_sources, rules)
      observe(data, "DATE", src, "network-traffic", year, end)
      data <- refresh_window(data, "DATE", year, end)
      data <- mutate(data, REG = "EUR")
      label <- if (end < paste0(year, "-12-31")) paste0(year, "p") else as.character(year)
      write_product(data, file.path("network-traffic", build_pbwg_network_traffic_filename(label)),
                    "network-traffic", year, "EUR")
    }
  }
  write_csv(bind_rows(coverage), file.path(output, "coverage.csv"))
  write_csv(bind_rows(monthly), file.path(output, "monthly-observations.csv"))
  if (length(source_quality)) write_csv(distinct(bind_rows(source_quality)), file.path(output, "source-quality.csv"))
  if (length(reference_checks)) write_csv(bind_rows(reference_checks), file.path(output, "reference-coverage.csv"))
  if (length(match_checks)) write_csv(bind_rows(match_checks), file.path(output, "tma-match-coverage.csv"))
  write_csv(bind_rows(manifest), file.path(output, "manifest.csv"))
  writeLines("COMPLETE", file.path(output, "RUN-STATUS.txt"))
  complete <- TRUE
  message("Completed refresh: ", output, "\nReview coverage.csv and tma-match-coverage.csv before reporting.")
  invisible(output)
}
