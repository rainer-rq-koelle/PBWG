#!/usr/bin/env Rscript

# Build the 2024 DSMA sector-review artefact used by the Technical Note.
# APDF paths may be overridden for the work infrastructure without changing
# the analysis or the portable output.

library(dplyr)
library(purrr)
library(tidyr)

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1]])
} else {
  file.path(getwd(), "scripts", "build-dsma-sector-review.R")
}
project_root <- normalizePath(file.path(dirname(script_path), ".."))
devtools::load_all(project_root, quiet = TRUE)

reference_year <- 2024L
study_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
  "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM"
)

smoothing_bandwidth <- 6
analysis_bandwidths <- c(4, 6, 8)
minimum_relative_prominence <- 0.02
rounding_increment <- 5
valley_safety_fraction <- 0.25
minimum_valley_to_lower_peak_ratio <- 0.25
median_merge_threshold_minutes <- 80 / 60
borderline_valley_to_lower_peak_ratio <- 0.15
pooling_shift_threshold_minutes <- 30 / 60
pooling_share_threshold <- 0.01

apdf_archive <- Sys.getenv(
  "PBWG_APDF_2024",
  unset = "/Users/rainerkoelle/RProjects/__DATA/APDF/apdf-2024.zip"
)
lgav_apdf_archive <- Sys.getenv(
  "PBWG_APDF_LGAV_2024",
  unset = "/Users/rainerkoelle/RProjects/__DATA/APDF/apdf-LGAV-2019-2024.zip"
)
lppt_apdf_archive <- Sys.getenv(
  "PBWG_APDF_LPPT_2024",
  unset = "/Users/rainerkoelle/RProjects/__DATA/APDF/LPPT-apdf.zip"
)
output_path <- file.path(project_root, "inst", "dsma-sector-review-2024.rds")

study_airport_sources <- tibble::tibble(
  ICAO = study_airports,
  ARCHIVE = case_when(
    .data$ICAO == "LGAV" ~ lgav_apdf_archive,
    .data$ICAO == "LPPT" ~ lppt_apdf_archive,
    .default = apdf_archive
  ),
  FILE = case_when(
    .data$ICAO == "LGAV" ~ "LGAV_01-JAN-2024_31-DEC-2024.parquet",
    .data$ICAO == "LPPT" ~ "LPPT_01-JAN-2024_31-DEC-2024.gz.parquet",
    .default = paste0(.data$ICAO, "_01-JAN-2024_31-DEC-2024.gz.parquet")
  )
)

if (any(!file.exists(study_airport_sources$ARCHIVE))) {
  missing_archives <- unique(study_airport_sources$ARCHIVE[!file.exists(study_airport_sources$ARCHIVE)])
  stop("APDF archive(s) not found: ", paste(missing_archives, collapse = ", "))
}

circular_distance <- function(first, second) {
  abs(((first - second + 180) %% 360) - 180)
}

build_boundary_utility <- function(samples, proposal, airport) {
  assigned_samples <- summarise_tma_sector_support(
    tma_samples = samples,
    sector_definitions = proposal$sector_definitions
  )$assigned_samples

  cell_references <- assigned_samples |>
    filter(.data$VALID_TMA, !is.na(.data$SECTOR)) |>
    summarise(
      N = n(),
      GANP_P20 = as.numeric(stats::quantile(.data$TMA_TIME, probs = 0.20, names = FALSE)),
      TYPICAL_P50 = stats::median(.data$TMA_TIME),
      .by = c("RANGE_NM", "SECTOR", "RWY", "CLASS")
    )

  boundary_pairs <- proposal$cut_audit |>
    left_join(
      proposal$sector_definitions |>
        transmute(
          RANGE_NM = .data$RANGE_NM,
          CUT_ROUNDED = .data$BEARING_TO,
          LEFT_SECTOR = .data$SECTOR
        ),
      by = c("RANGE_NM", "CUT_ROUNDED")
    ) |>
    transmute(
      ICAO = airport,
      RANGE_NM = .data$RANGE_NM,
      CUT = .data$CUT_ROUNDED,
      PEAK_SEPARATION_DEG = .data$PEAK_SEPARATION_DEG,
      VALLEY_TO_LOWER_PEAK_RATIO = .data$VALLEY_TO_LOWER_PEAK_RATIO,
      PAIRWISE_SEPARATION = .data$PAIRWISE_SEPARATION,
      LEFT_SECTOR = .data$LEFT_SECTOR,
      RIGHT_SECTOR = .data$SECTOR
    )

  pairwise_utility <- boundary_pairs |>
    left_join(
      cell_references |>
        rename(
          LEFT_SECTOR = .data$SECTOR,
          N_LEFT = .data$N,
          P20_LEFT = .data$GANP_P20,
          P50_LEFT = .data$TYPICAL_P50
        ),
      by = c("RANGE_NM", "LEFT_SECTOR")
    ) |>
    inner_join(
      cell_references |>
        rename(
          RIGHT_SECTOR = .data$SECTOR,
          N_RIGHT = .data$N,
          P20_RIGHT = .data$GANP_P20,
          P50_RIGHT = .data$TYPICAL_P50
        ),
      by = c("RANGE_NM", "RIGHT_SECTOR", "RWY", "CLASS")
    ) |>
    summarise(
      N_SHARED_CELLS = n(),
      MIN_SHARED_CELL_N = min(pmin(.data$N_LEFT, .data$N_RIGHT)),
      WEIGHTED_ABS_P20_DIFF = stats::weighted.mean(
        abs(.data$P20_LEFT - .data$P20_RIGHT),
        w = pmin(.data$N_LEFT, .data$N_RIGHT)
      ),
      WEIGHTED_ABS_P50_DIFF = stats::weighted.mean(
        abs(.data$P50_LEFT - .data$P50_RIGHT),
        w = pmin(.data$N_LEFT, .data$N_RIGHT)
      ),
      .by = c(
        "ICAO", "RANGE_NM", "CUT", "PEAK_SEPARATION_DEG",
        "VALLEY_TO_LOWER_PEAK_RATIO", "PAIRWISE_SEPARATION"
      )
    )

  pooling_impact <- pmap_dfr(
    boundary_pairs,
    function(
        ICAO, RANGE_NM, CUT, PEAK_SEPARATION_DEG,
        VALLEY_TO_LOWER_PEAK_RATIO, PAIRWISE_SEPARATION,
        LEFT_SECTOR, RIGHT_SECTOR
    ) {
      pooled_samples <- assigned_samples |>
        filter(.data$VALID_TMA, .data$SECTOR %in% c(LEFT_SECTOR, RIGHT_SECTOR))
      sector_references <- pooled_samples |>
        summarise(
          N = n(),
          P20 = as.numeric(stats::quantile(.data$TMA_TIME, probs = 0.20, names = FALSE)),
          .by = c("SECTOR", "RWY", "CLASS")
        )
      pooled_references <- pooled_samples |>
        summarise(
          P20_POOLED = as.numeric(stats::quantile(.data$TMA_TIME, probs = 0.20, names = FALSE)),
          .by = c("RWY", "CLASS")
        )

      sector_references |>
        left_join(pooled_references, by = c("RWY", "CLASS")) |>
        mutate(P20_SHIFT = abs(.data$P20 - .data$P20_POOLED)) |>
        summarise(
          N_POOL_MOVEMENTS = sum(.data$N),
          POOL_WEIGHTED_P20_SHIFT = stats::weighted.mean(.data$P20_SHIFT, .data$N),
          POOL_PCT_P20_SHIFT_OVER_80S = sum(.data$N[.data$P20_SHIFT > 80 / 60]) / sum(.data$N),
          POOL_MAX_P20_SHIFT = max(.data$P20_SHIFT)
        ) |>
        mutate(ICAO = ICAO, RANGE_NM = RANGE_NM, CUT = CUT)
    }
  )

  left_join(pairwise_utility, pooling_impact, by = c("ICAO", "RANGE_NM", "CUT"))
}

analyse_airport <- function(airport) {
  source <- filter(study_airport_sources, .data$ICAO == airport)
  message("Analysing ", airport, " DEP")
  samples <- prepare_apdf_tma_reference_input_from_zip(
    zipped_archive_path = source$ARCHIVE[[1]],
    files = source$FILE[[1]],
    type = "parquet",
    ranges = c(40, 100)
  )
  density <- prepare_tma_bearing_density(
    tma_samples = samples,
    airport = airport,
    phase = "DEP",
    ranges = c(40, 100),
    smoothing_bandwidth = smoothing_bandwidth
  )
  extrema <- identify_tma_bearing_extrema(
    tma_density = density,
    min_relative_prominence = minimum_relative_prominence
  )
  proposal <- propose_tma_sector_definitions(
    tma_density = density,
    extrema = extrema,
    rounding_increment = rounding_increment,
    valley_safety_fraction = valley_safety_fraction
  )
  multiscale_cuts <- map_dfr(analysis_bandwidths, function(bandwidth) {
    scale_density <- prepare_tma_bearing_density(
      tma_samples = samples,
      airport = airport,
      phase = "DEP",
      ranges = c(40, 100),
      smoothing_bandwidth = bandwidth
    )
    scale_extrema <- identify_tma_bearing_extrema(
      tma_density = scale_density,
      min_relative_prominence = minimum_relative_prominence
    )
    mutate(
      propose_tma_sector_definitions(
        tma_density = scale_density,
        extrema = scale_extrema,
        rounding_increment = 1,
        valley_safety_fraction = valley_safety_fraction
      )$cut_audit,
      SMOOTHING_BANDWIDTH = bandwidth
    )
  })

  list(
    density = density,
    proposal = proposal,
    support = summarise_tma_sector_support(samples, proposal$sector_definitions),
    boundary_utility = build_boundary_utility(samples, proposal, airport),
    multiscale_cuts = multiscale_cuts
  )
}

results <- map(study_airports, analyse_airport) |>
  setNames(study_airports)

density <- imap_dfr(results, ~ mutate(.x$density, ICAO = .y))
definitions <- imap_dfr(results, ~ mutate(.x$proposal$sector_definitions, ICAO = .y))
cuts <- imap_dfr(results, ~ mutate(.x$proposal$cut_audit, ICAO = .y))
multiscale_cuts <- imap_dfr(results, ~ mutate(.x$multiscale_cuts, ICAO = .y))
boundary_utility <- map_dfr(results, "boundary_utility")
sector_support <- imap_dfr(results, ~ mutate(.x$support$sector_summary, ICAO = .y))

cut_stability <- cuts |>
  transmute(ICAO, RANGE_NM, CUT = .data$CUT_ROUNDED) |>
  pmap_dfr(function(ICAO, RANGE_NM, CUT) {
    tibble::tibble(
      ICAO = ICAO,
      RANGE_NM = RANGE_NM,
      CUT = CUT,
      N_STABLE_BANDWIDTHS = sum(map_lgl(analysis_bandwidths, function(bandwidth) {
        candidates <- multiscale_cuts |>
          filter(
            .data$ICAO == ICAO,
            .data$RANGE_NM == RANGE_NM,
            .data$SMOOTHING_BANDWIDTH == bandwidth
          ) |>
          pull(.data$CUT_ROUNDED)
        any(circular_distance(candidates, CUT) <= rounding_increment)
      }))
    )
  })

boundary_register <- cuts |>
  transmute(
    ICAO, RANGE_NM,
    CUT = .data$CUT_ROUNDED,
    PEAK_SEPARATION_DEG = .data$PEAK_SEPARATION_DEG,
    VALLEY_TO_LOWER_PEAK_RATIO = .data$VALLEY_TO_LOWER_PEAK_RATIO,
    PAIRWISE_SEPARATION = .data$PAIRWISE_SEPARATION
  ) |>
  left_join(
    boundary_utility |>
      select(
        .data$ICAO, .data$RANGE_NM, .data$CUT,
        .data$N_SHARED_CELLS, .data$MIN_SHARED_CELL_N,
        .data$WEIGHTED_ABS_P20_DIFF, .data$WEIGHTED_ABS_P50_DIFF,
        .data$N_POOL_MOVEMENTS, .data$POOL_WEIGHTED_P20_SHIFT,
        .data$POOL_PCT_P20_SHIFT_OVER_80S, .data$POOL_MAX_P20_SHIFT
      ),
    by = c("ICAO", "RANGE_NM", "CUT")
  ) |>
  left_join(cut_stability, by = c("ICAO", "RANGE_NM", "CUT")) |>
  mutate(
    REVIEW_REASON = case_when(
      .data$PAIRWISE_SEPARATION < 0.75 ~ "Shallow valley",
      .data$PEAK_SEPARATION_DEG < 3 * smoothing_bandwidth ~ "Close peaks",
      .data$N_STABLE_BANDWIDTHS < length(analysis_bandwidths) ~ "Smoothing-sensitive cut",
      .default = NA_character_
    ),
    MERGE_RECOMMENDED =
      .data$VALLEY_TO_LOWER_PEAK_RATIO >= minimum_valley_to_lower_peak_ratio &
      !is.na(.data$WEIGHTED_ABS_P50_DIFF) &
      .data$WEIGHTED_ABS_P50_DIFF <= median_merge_threshold_minutes,
    EVIDENCE_DECISION = case_when(
      .data$MERGE_RECOMMENDED &
        .data$POOL_WEIGHTED_P20_SHIFT <= pooling_shift_threshold_minutes &
        .data$POOL_PCT_P20_SHIFT_OVER_80S <= pooling_share_threshold ~
        "Propose merge",
      .data$VALLEY_TO_LOWER_PEAK_RATIO < borderline_valley_to_lower_peak_ratio |
        .data$WEIGHTED_ABS_P50_DIFF > median_merge_threshold_minutes |
        .data$POOL_WEIGHTED_P20_SHIFT > pooling_shift_threshold_minutes |
        .data$POOL_PCT_P20_SHIFT_OVER_80S > pooling_share_threshold ~
        "Propose retain",
      .default = "Analyst review"
    ),
    EVIDENCE_REASON = case_when(
      .data$EVIDENCE_DECISION == "Propose merge" ~
        "Shallow valley and low pooling impact",
      .data$EVIDENCE_DECISION == "Propose retain" ~
        "Deep valley or material timing/pooling impact",
      .default = "Mixed geometry and pooling evidence"
    )
  ) |>
  arrange(.data$ICAO, .data$RANGE_NM, .data$CUT) |>
  mutate(
    MERGED_FAMILY_EDGE = !.data$MERGE_RECOMMENDED & (
      lag(.data$MERGE_RECOMMENDED, default = last(.data$MERGE_RECOMMENDED)) |
        lead(.data$MERGE_RECOMMENDED, default = first(.data$MERGE_RECOMMENDED))
    ),
    BOUNDARY_ROLE = case_when(
      .data$MERGE_RECOMMENDED ~ "Suggested merge interior",
      .data$MERGED_FAMILY_EDGE ~ "Suggested merged-family edge",
      .default = "Retained candidate boundary"
    ),
    .by = c("ICAO", "RANGE_NM")
  ) |>
  arrange(.data$ICAO, .data$RANGE_NM, .data$CUT)

build_merged_sector_labels <- function(cuts) {
  cuts <- sort(unique(cuts %% 360))
  next_cuts <- c(cuts[-1], cuts[[1]])
  paste(build_tma_sector_label(cuts, next_cuts), collapse = "; ")
}

merged_sector_register <- boundary_register |>
  filter(!.data$MERGE_RECOMMENDED) |>
  summarise(
    MERGED_SECTORS = build_merged_sector_labels(.data$CUT),
    .by = c("ICAO", "RANGE_NM")
  )

proposal_register <- definitions |>
  summarise(
    N_INITIAL_SECTORS = n(),
    INITIAL_SECTORS = paste(.data$SECTOR, collapse = "; "),
    NORTH_OVERRUN = first(.data$NORTH_OVERRUN),
    .by = c("ICAO", "RANGE_NM")
  ) |>
  left_join(merged_sector_register, by = c("ICAO", "RANGE_NM")) |>
  left_join(
    boundary_register |>
      filter(.data$MERGE_RECOMMENDED) |>
      summarise(MERGE_CUTS = paste(.data$CUT, collapse = "; "), .by = c("ICAO", "RANGE_NM")),
    by = c("ICAO", "RANGE_NM")
  ) |>
  left_join(
    boundary_register |>
      filter(!is.na(.data$REVIEW_REASON)) |>
      summarise(
        REVIEW_CUTS = paste0(.data$CUT, " (", .data$REVIEW_REASON, ")", collapse = "; "),
        .by = c("ICAO", "RANGE_NM")
      ),
    by = c("ICAO", "RANGE_NM")
  ) |>
  mutate(
    MERGE_CUTS = coalesce(.data$MERGE_CUTS, "None"),
    REVIEW_CUTS = coalesce(.data$REVIEW_CUTS, "None"),
    N_MERGED_SECTORS = stringr::str_count(.data$MERGED_SECTORS, ";") + 1L,
    PROPOSAL_STATUS = case_when(
      .data$MERGE_CUTS != "None" ~ "Merge recommendation requires analyst approval",
      .data$REVIEW_CUTS != "None" ~ "Analyst boundary decision required",
      .data$N_INITIAL_SECTORS > 6 ~ "Analyst complexity review",
      .default = "Initial proposal ready for approval"
    )
  ) |>
  arrange(.data$ICAO, .data$RANGE_NM)

saveRDS(
  list(
    metadata = list(
      reference_year = reference_year,
      phase = "DEP",
      study_airports = study_airports,
      smoothing_bandwidth = smoothing_bandwidth,
      analysis_bandwidths = analysis_bandwidths,
      minimum_relative_prominence = minimum_relative_prominence,
      rounding_increment = rounding_increment,
      valley_safety_fraction = valley_safety_fraction,
      median_merge_threshold_seconds = 80
    ),
    density = density,
    definitions = definitions,
    boundary_register = boundary_register,
    proposal_register = proposal_register,
    sector_support = sector_support
  ),
  output_path
)

message("Wrote ", output_path)
