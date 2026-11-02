source(file.path(project_root, "scripts", "lib", "aviation-r2.R"))
cache <- Sys.getenv("PBWG_R2_CACHE", unset = file.path(project_root, ".tmp", "aviation-data", "2026-11-02"))
config <- list(products = "network", analysis_years = 2026L,
  reporting_end = c(`2026` = "2026-10-28"),
  nm_sources = aviation_refresh_sources(cache, "nm-flt", 2026),
  statfor_rules = Sys.getenv("PBWG_STATFOR_RULES"),
  output_dir = Sys.getenv("PBWG_REFRESH_OUTPUT", unset = file.path(
    project_root, "outputs", "pbwg-refresh-2026-20261102", "network")))
