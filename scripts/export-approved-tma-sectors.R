#!/usr/bin/env Rscript

# Derive package-readable approved sector files from the reviewed CSV sources.

library(arrow)
library(dplyr)
library(readr)

project_root <- normalizePath(file.path(getwd()))
extdata_dir <- file.path(project_root, "inst", "extdata")

arr_sectors <- read_csv(
  file.path(extdata_dir, "arr-sectors-approved-2024.csv"),
  show_col_types = FALSE
) |>
  arrange(.data$AIRPORT, .data$RANGE_NM, .data$SECTOR_SEQ)

dep_sectors <- read_csv(
  file.path(project_root, "inst", "dsma-sector-decisions-2024.csv"),
  show_col_types = FALSE
) |>
  filter(.data$DECISION_STATUS == "APPROVED") |>
  transmute(
    AIRPORT = .data$ICAO,
    .data$PHASE,
    .data$RANGE_NM,
    .data$SECTOR,
    .data$SECTOR_LABEL,
    .data$SECTOR_SEQ,
    .data$BEARING_FROM,
    .data$BEARING_TO
  ) |>
  arrange(.data$AIRPORT, .data$RANGE_NM, .data$SECTOR_SEQ)

validate_sectors <- function(sectors, phase) {
  stopifnot(all(sectors$PHASE == phase))
  stopifnot(!anyDuplicated(sectors[c("AIRPORT", "PHASE", "RANGE_NM", "SECTOR")]))
  stopifnot(all(grepl(paste0("^", phase, " BRG[0-9]{3}-[0-9]{3}$"), sectors$SECTOR_LABEL)))
}

validate_sectors(arr_sectors, "ARR")
validate_sectors(dep_sectors, "DEP")

write_csv(dep_sectors, file.path(extdata_dir, "dep-sectors-approved-2024.csv"))
write_parquet(arr_sectors, file.path(extdata_dir, "arr-sectors-approved-2024.parquet"))
write_parquet(dep_sectors, file.path(extdata_dir, "dep-sectors-approved-2024.parquet"))

message("Exported approved ARR and DEP sector definitions for ",
        n_distinct(c(arr_sectors$AIRPORT, dep_sectors$AIRPORT)), " airports.")
