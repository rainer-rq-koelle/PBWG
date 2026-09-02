################################################################################
# DSMA Three-Airport Validation
# Test: EDDF, EDDM, EGLL
# Goal: Validate approach scales beyond single airport
################################################################################

library(dplyr)
library(arrow)
library(tidyr)
devtools::load_all(quiet = TRUE)

test_airports <- c("EDDF", "EDDM", "EGLL")

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║         DSMA THREE-AIRPORT VALIDATION TEST                         ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# Read all three airports from zip
apdf_list <- list()

for (apt in test_airports) {
  filename <- paste0(apt, "_01-JAN-2024_31-DEC-2024.gz.parquet")
  cat("Reading", apt, "...")

  temp <- tempfile(fileext = ".parquet")
  unzip(
    "../xx-test-gotcha/data/apdf-annual/apdf-2024.zip",
    files = filename,
    exdir = dirname(temp),
    junkpaths = TRUE
  )

  apdf_list[[apt]] <- read_parquet(file.path(dirname(temp), filename)) %>%
    mutate(ICAO = apt)

  cat(" ✓\n")
}

apdf_all <- bind_rows(apdf_list)
cat("\n✓ Loaded", nrow(apdf_all), "total rows\n\n")

# Prepare TMA samples
cat("Preparing DEP TMA samples...\n")

tma_dep <- apdf_all %>%
  filter(SRC_PHASE == "DEP") %>%
  filter(!is.na(C40_BEARING) | !is.na(C100_BEARING)) %>%
  pivot_longer(
    cols = c(C40_BEARING, C100_BEARING),
    names_to = "RANGE_TYPE",
    values_to = "BEARING"
  ) %>%
  mutate(
    RANGE_NM = if_else(RANGE_TYPE == "C40_BEARING", 40, 100),
    TMA_ADDL_TIME_MIN = if_else(RANGE_NM == 40, C40_TRANSIT_TIME_MIN, C100_TRANSIT_TIME_MIN),
    PHASE = SRC_PHASE,
    DATE = as.Date(MVT_TIME_UTC),
    RWY = AP_C_RWY,
    FLIGHT_ID = AP_C_FLTID
  ) %>%
  filter(
    !is.na(BEARING),
    BEARING >= 0,
    BEARING < 360,
    !is.na(TMA_ADDL_TIME_MIN),
    TMA_ADDL_TIME_MIN > 0
  ) %>%
  select(ICAO, PHASE, DATE, RANGE_NM, BEARING, TMA_ADDL_TIME_MIN, RWY, AC_CLASS, FLIGHT_ID)

cat("✓ Prepared", nrow(tma_dep), "DEP movements\n\n")

# Summary by airport
sample_summary <- tma_dep %>%
  summarise(N = n(), .by = c(ICAO, RANGE_NM)) %>%
  arrange(ICAO, RANGE_NM)

cat("Sample sizes:\n")
print(sample_summary, n = Inf)
cat("\n")

# Process each airport
cat("═══════════════════════════════════════════════════════════════════\n")
cat("PROCESSING AIRPORTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

results <- list()

for (apt in test_airports) {
  cat("─────────────────────────────────────────────────────────────────\n")
  cat(apt, "\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  for (range in c(40, 100)) {
    cat("  ", range, "NM: ")

    # Generate density
    density <- prepare_tma_bearing_density(
      tma_samples = tma_dep,
      airport = apt,
      phase = "DEP",
      ranges = range,
      smoothing_bandwidth = 12
    )

    # Detect extrema
    extrema <- identify_tma_bearing_extrema(
      density,
      min_relative_prominence = 0.02
    )

    n_peaks <- sum(extrema$EXTREMUM == "PEAK" & extrema$IS_SUBSTANTIAL)
    n_valleys <- sum(extrema$EXTREMUM == "MINIMUM" & extrema$IS_SUBSTANTIAL)

    cat(n_peaks, "peaks,", n_valleys, "valleys")

    # Propose sectors
    if (n_peaks > 0) {
      proposal <- propose_tma_sector_definitions(
        tma_density = density,
        extrema = extrema,
        rounding_increment = 5,
        valley_safety_fraction = 0.25
      )

      n_sectors <- nrow(proposal)
      cat(" →", n_sectors, "sectors\n")

      results[[paste(apt, range)]] <- list(
        airport = apt,
        range = range,
        peaks = n_peaks,
        valleys = n_valleys,
        sectors = n_sectors,
        proposal = proposal
      )
    } else {
      cat(" → 0 sectors (no peaks!)\n")
      results[[paste(apt, range)]] <- list(
        airport = apt,
        range = range,
        peaks = 0,
        valleys = 0,
        sectors = 0,
        proposal = NULL
      )
    }
  }
  cat("\n")
}

cat("═══════════════════════════════════════════════════════════════════\n")
cat("SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

summary_df <- bind_rows(lapply(results, function(r) {
  tibble(
    Airport = r$airport,
    Range = paste0(r$range, "NM"),
    Peaks = r$peaks,
    Valleys = r$valleys,
    Sectors = r$sectors,
    Status = if_else(r$sectors > 0, "✓ Success", "✗ Failed")
  )
}))

print(summary_df, n = Inf)

cat("\n")

# Check for failures
failures <- summary_df %>% filter(Status == "✗ Failed")
if (nrow(failures) > 0) {
  cat("⚠ FAILURES DETECTED:\n")
  print(failures, n = Inf)
} else {
  cat("✓ ALL TESTS PASSED - APPROACH VALIDATED!\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("CONCLUSION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

if (nrow(failures) == 0) {
  cat("✅ DSMA approach validated for multiple airports\n")
  cat("✅ Ready to scale to all 12 airports\n")
  cat("✅ Same parameters as ASMA work for DSMA\n\n")
  cat("Next step: Run full batch for all 12 airports\n")
} else {
  cat("⚠ Issues found - investigate failures before scaling\n")
}
