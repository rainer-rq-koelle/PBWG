# Run after cache-aviation-data.R has verified selected 2026 airport files.
sys.source(file.path(project_root, "scripts", "config", "meeting-reference.R"), envir = environment())
source(file.path(project_root, "scripts", "lib", "aviation-r2.R"))
cache <- Sys.getenv("PBWG_R2_CACHE", unset = file.path(project_root, ".tmp", "aviation-data", "2026-11-02"))
selection <- Sys.getenv("PBWG_AIRPORTS", unset = paste(config$airports, collapse = ","))
config$airports <- strsplit(selection, ",", fixed = TRUE)[[1]]
# Preserve the approved local 2024 baseline; the 2026 source is refreshed.
config$apdf_sources <- bind_rows(
  config$apdf_sources |> filter(.data$ICAO %in% config$airports),
  aviation_refresh_sources(cache, "apdf", 2026, config$airports))
config$products <- c("traffic", "punctuality", "throughput", "tma")
config$analysis_years <- 2026L
config$reporting_end <- c(`2026` = "2026-09-30")
config$output_dir <- Sys.getenv("PBWG_REFRESH_OUTPUT", unset = file.path(
  project_root, "outputs", "pbwg-refresh-2026-20261102", "apdf"))
