# Existing full-year meeting baseline. Copy and adapt after the 2026 extraction.
# Source the reference configuration in this configuration's own environment.
sys.source(file.path(project_root, "scripts", "config", "meeting-reference.R"),
           envir = environment())
config$products <- "tma"
config$analysis_years <- 2023:2025
config$reporting_end <- c(`2023` = "2023-12-31", `2024` = "2024-12-31", `2025` = "2025-12-31")
config$output_dir <- Sys.getenv("PBWG_REFRESH_OUTPUT", unset = file.path(
  dirname(project_root), "PBWG-data", "runs", "meeting-tma-2023-2025-ref2024"
))
config$apdf_sources <- bind_rows(config$apdf_sources,
  apdf_source(2023, airports, file.path(apdf_root, "apdf-2023.zip"), complete_through = "2023-12-31"),
  apdf_source(2025, airports, file.path(apdf_root, "apdf-2025.zip"), complete_through = "2025-12-31")
)
