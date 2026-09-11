#!/usr/bin/env Rscript

# Run a bounded, reproducible TMA reference smoke test for selected airports.

library(dplyr)
library(purrr)

project_root <- normalizePath(file.path(getwd()))
devtools::load_all(project_root, quiet = TRUE)

all_airports <- c(
  "EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
  "LFPG", "LGAV", "LIRF", "LPPT", "LSZH", "LTFM"
)
requested <- Sys.getenv("PBWG_TMA_SMOKE_AIRPORTS", unset = "")
airports <- if (nzchar(requested)) trimws(strsplit(requested, ",", fixed = TRUE)[[1]]) else all_airports
output_path <- Sys.getenv(
  "PBWG_TMA_SMOKE_OUTPUT",
  unset = file.path(tempdir(), "pbwg-tma-reference-smoke.rds")
)

stopifnot(all(airports %in% all_airports))

source_for <- function(airport) {
  list(
    archive = dplyr::case_when(
      airport == "LGAV" ~ "/Users/rainerkoelle/RProjects/__DATA/APDF/apdf-LGAV-2019-2024.zip",
      airport == "LPPT" ~ "/Users/rainerkoelle/RProjects/__DATA/APDF/LPPT-apdf.zip",
      .default = "/Users/rainerkoelle/RProjects/__DATA/APDF/apdf-2024.zip"
    ),
    file = dplyr::case_when(
      airport == "LGAV" ~ "LGAV_01-JAN-2024_31-DEC-2024.parquet",
      airport == "LPPT" ~ "LPPT_01-JAN-2024_31-DEC-2024.gz.parquet",
      .default = paste0(airport, "_01-JAN-2024_31-DEC-2024.gz.parquet")
    )
  )
}

samples <- map_dfr(airports, function(airport) {
  source <- source_for(airport)
  prepare_apdf_tma_reference_input_from_zip(
    zipped_archive_path = source$archive,
    files = source$file,
    type = "parquet",
    ranges = c(40, 100)
  )
})

sectors <- read_approved_tma_sector_definitions() |>
  filter(.data$AIRPORT %in% airports) |>
  rename(ICAO = AIRPORT)
assigned <- assign_tma_sector(samples, sectors)
references <- build_tma_reference(
  tma_samples = assigned,
  ref_start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
  ref_end = as.POSIXct("2024-12-31 23:59:59", tz = "UTC"),
  variant = tma_reference_variants()$REF_VARIANT
)
augmented <- apply_tma_reference(assigned, references)

coverage <- augmented |>
  filter(.data$VALID_TMA) |>
  summarise(
    N_ROWS = n(),
    N_MATCHED = sum(.data$HAS_REFERENCE),
    N_UNMATCHED = sum(!.data$HAS_REFERENCE),
    N_MATCHED_VARIANTS = n_distinct(.data$REF_VARIANT[.data$HAS_REFERENCE]),
    .by = c("ICAO", "PHASE", "RANGE_NM")
)

if (any(coverage$N_UNMATCHED > 0 | coverage$N_MATCHED_VARIANTS != 2L)) {
  stop("Reference-period smoke test found incomplete valid-tuple coverage.")
}

saveRDS(
  list(
    reference_summary = references |>
      summarise(
        N_REFERENCE_ROWS = n(),
        N_TUPLES = n_distinct(interaction(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$CLASS, .data$RWY, .data$SECTOR)),
        N_SPARSE_TUPLES = n_distinct(interaction(.data$ICAO, .data$PHASE, .data$RANGE_NM, .data$CLASS, .data$RWY, .data$SECTOR)[.data$N < 5]),
        .by = "REF_VARIANT"
      ),
    coverage = coverage,
    daily = summarise_pbwg_tma_daily(augmented)
  ),
  output_path
)

message("Wrote ", output_path)
