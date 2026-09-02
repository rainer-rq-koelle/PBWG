################################################################################
# DSMA Single Airport Test - Debug peak detection
################################################################################

library(dplyr)
library(arrow)
devtools::load_all(quiet = TRUE)

cat("Testing DSMA pipeline for EDDF only\n\n")
apdf_archive <- Sys.getenv(
  "PBWG_APDF_2024",
  unset = "/Users/rainerkoelle/RProjects/__DATA/APDF/apdf-2024.zip"
)
smoothing_bandwidth <- 6

# Read EDDF data
temp <- tempfile(fileext = ".parquet")
unzip(
  apdf_archive,
  files = "EDDF_01-JAN-2024_31-DEC-2024.gz.parquet",
  exdir = dirname(temp),
  junkpaths = TRUE
)

apdf_eddf <- read_parquet(file.path(dirname(temp), "EDDF_01-JAN-2024_31-DEC-2024.gz.parquet"))

cat("Loaded", nrow(apdf_eddf), "rows\n")
cat("SRC_PHASE values:", unique(apdf_eddf$SRC_PHASE), "\n\n")

# Prepare TMA samples
tma_dep <- apdf_eddf %>%
  filter(SRC_PHASE == "DEP") %>%
  filter(!is.na(C40_BEARING) | !is.na(C100_BEARING)) %>%
  tidyr::pivot_longer(
    cols = c(C40_BEARING, C100_BEARING),
    names_to = "RANGE_TYPE",
    values_to = "BEARING"
  ) %>%
  mutate(
    RANGE_NM = if_else(RANGE_TYPE == "C40_BEARING", 40, 100),
    TMA_TRANSIT_TIME_MIN = if_else(RANGE_NM == 40, C40_TRANSIT_TIME_MIN, C100_TRANSIT_TIME_MIN),
    PHASE = SRC_PHASE,
    DATE = as.Date(MVT_TIME_UTC),
    RWY = AP_C_RWY,
    FLIGHT_ID = AP_C_FLTID,
    ICAO = "EDDF"
  ) %>%
  filter(
    !is.na(BEARING),
    BEARING >= 0,
    BEARING < 360,
    !is.na(TMA_TRANSIT_TIME_MIN),
    TMA_TRANSIT_TIME_MIN > 0
  ) %>%
  select(ICAO, PHASE, DATE, RANGE_NM, BEARING, TMA_TRANSIT_TIME_MIN, RWY, AC_CLASS, FLIGHT_ID)

cat("TMA samples:", nrow(tma_dep), "\n")
cat("Sample by range:\n")
print(table(tma_dep$RANGE_NM))
cat("\n")

# Test density for 40NM
cat("Testing 40NM density...\n")
density_40 <- prepare_tma_bearing_density(
  tma_samples = tma_dep,
  airport = "EDDF",
  phase = "DEP",
  ranges = 40,
  smoothing_bandwidth = smoothing_bandwidth
)

cat("Density output:\n")
cat("  Columns:", paste(names(density_40), collapse = ", "), "\n")
cat("  Rows:", nrow(density_40), "\n")
cat("  Sample:\n")
print(head(density_40, 10))
cat("\n")

# Test extrema
cat("Testing extrema detection...\n")
extrema_40 <- identify_tma_bearing_extrema(
  density_40,
  min_relative_prominence = 0.02
)

cat("Extrema output:\n")
cat("  Columns:", paste(names(extrema_40), collapse = ", "), "\n")
cat("  Rows:", nrow(extrema_40), "\n")
if (nrow(extrema_40) > 0) {
  cat("  Sample:\n")
  print(head(extrema_40, 10))
} else {
  cat("  ⚠ NO EXTREMA FOUND!\n")
}
cat("\n")

cat("Summary:\n")
if (nrow(extrema_40) > 0) {
  print(table(extrema_40$EXTREMUM, extrema_40$IS_SUBSTANTIAL))
} else {
  cat("No peaks or valleys detected - check smoothing parameters or data\n")
}
