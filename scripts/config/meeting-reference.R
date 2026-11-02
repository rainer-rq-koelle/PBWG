# Copy this file outside the package for machine-specific settings.
# Use forward slashes on Windows, including for mapped/network paths.
data_root <- Sys.getenv("PBWG_SOURCE_ROOT", unset = file.path(dirname(project_root), "__DATA"))
apdf_root <- file.path(data_root, "APDF")
airports <- pbwg_study_airports()

config <- list(
  # Must be a NEW or EMPTY directory. Choose a meaningful name for each run.
  output_dir = Sys.getenv("PBWG_REFRESH_OUTPUT", unset = file.path(
    dirname(project_root), "PBWG-data", "runs", "meeting-reference-2024"
  )),
  products = c("references"),
  airports = airports,
  reference_year = 2024L,
  analysis_years = 2024L,
  reporting_end = c(`2024` = "2024-12-31"),
  save_augmented = FALSE,
  apdf_sources = bind_rows(
    apdf_source(2024, setdiff(airports, c("LGAV", "LPPT")),
                file.path(apdf_root, "apdf-2024.zip"), complete_through = "2024-12-31"),
    apdf_source(2024, "LGAV", file.path(apdf_root, "apdf-LGAV-2019-2024.zip"),
                complete_through = "2024-12-31"),
    apdf_source(2024, "LPPT", file.path(apdf_root, "LPPT-apdf.zip"),
                complete_through = "2024-12-31")
  )
)

# To produce meeting ASMA/DSMA outputs, set products = c("tma"). This also
# rebuilds both references from approved sectors. Add each analysis year's
# APDF sources, reporting_end, and analysis_years (e.g. 2023:2025).
# Each airport/year must resolve to exactly one archive member.
# MEMBER can be set explicitly if an archive uses a different naming scheme.
#
# After the new extraction arrives, add e.g.:
# config$apdf_sources <- bind_rows(config$apdf_sources,
#   apdf_source(2026, airports, "D:/data/APDF/apdf-2026-latest.zip",
#               extracted_on = "2026-10-30"))
# config$analysis_years <- 2026L
# config$reporting_end <- c(`2026` = "2026-09-30")
# config$products <- c("traffic", "punctuality", "throughput", "tma", "network")
# config$output_dir <- "D:/PBWG-data/runs/2026-meeting-refresh"
# config$nm_sources <- nm_source(2026, "D:/data/NM/nm-flt-2026-latest.zip",
#                                extracted_on = "2026-10-30")
# config$statfor_rules <- "D:/data/statfor-market-segments-sid-2025.xlsx"
#
# reporting_end is the requested comparison ceiling, shared across products.
# Use a later ceiling if retaining all available NM days is intended.
# APDF COMPLETE_THROUGH is OPTIONAL source-confirmed coverage, per airport.
# Set it to August for airports only confirmed through August. Leave NA when
# unconfirmed: the output will explicitly say completeness is unconfirmed.
# EXTRACTED_ON is never inferred from an archive name or file modification time.
