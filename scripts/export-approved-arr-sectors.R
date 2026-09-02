################################################################################
# Export Approved ARR Sector Definitions
# Generates the sector definitions we approved during our review
################################################################################

library(dplyr)
library(arrow)
devtools::load_all(quiet = TRUE)

cat("Generating approved ARR sector definitions...\n\n")

airports <- c("EDDF", "EDDM", "EGKK", "EGLL", "EHAM", "LEBL", "LEMD",
              "LFPG", "LGAV", "LIRF", "LSZH", "LTFM")

all_sectors <- list()

for (apt in airports) {
  cat(sprintf("%-4s: ", apt))

  # Load APDF
  if (apt == "LGAV") {
    temp <- tempfile()
    unzip("../xx-test-gotcha/data/apdf-annual/apdf-LGAV-2019-2024.zip",
          files = "LGAV_01-JAN-2024_31-DEC-2024.parquet",
          exdir = dirname(temp), junkpaths = TRUE)
    apdf <- read_parquet(file.path(dirname(temp), "LGAV_01-JAN-2024_31-DEC-2024.parquet"))
  } else {
    temp <- tempfile()
    filename <- paste0(apt, "_01-JAN-2024_31-DEC-2024.gz.parquet")
    unzip("../xx-test-gotcha/data/apdf-annual/apdf-2024.zip",
          files = filename, exdir = dirname(temp), junkpaths = TRUE)
    apdf <- read_parquet(file.path(dirname(temp), filename))
  }

  # Prepare TMA samples
  tma <- apdf %>%
    filter(SRC_PHASE == "ARR", !is.na(C40_BEARING) | !is.na(C100_BEARING)) %>%
    tidyr::pivot_longer(
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
      FLIGHT_ID = AP_C_FLTID,
      ICAO = apt
    ) %>%
    filter(
      !is.na(BEARING), BEARING >= 0, BEARING < 360,
      !is.na(TMA_ADDL_TIME_MIN), TMA_ADDL_TIME_MIN > 0
    ) %>%
    select(ICAO, PHASE, DATE, RANGE_NM, BEARING, TMA_ADDL_TIME_MIN,
           RWY, AC_CLASS, FLIGHT_ID)

  # Generate sector definitions for both ranges
  for (range in c(40, 100)) {
    tryCatch({
      d <- prepare_tma_bearing_density(tma, apt, "ARR", range, 12)
      e <- identify_tma_bearing_extrema(d, 0.02)
      p_result <- propose_tma_sector_definitions(d, e, 5, 0.25)

      # Extract sector definitions from result list
      p <- p_result$sector_definitions

      # Add metadata
      sectors <- p %>%
        mutate(
          AIRPORT = apt,
          PHASE = "ARR",
          RANGE_NM = range,
          SECTOR = build_tma_sector_label(BEARING_FROM, BEARING_TO),
          SECTOR_LABEL = paste("ARR", SECTOR),
          SECTOR_SEQ = row_number()
        ) %>%
        select(AIRPORT, PHASE, RANGE_NM, SECTOR, SECTOR_LABEL, SECTOR_SEQ,
               BEARING_FROM, BEARING_TO)

      all_sectors[[paste(apt, range)]] <- sectors
    }, error = function(e) {
      cat(sprintf("ERROR at %dNM: %s\n", range, e$message))
    })
  }

  # Count this airport's sectors
  apt_sectors <- bind_rows(all_sectors) %>% filter(AIRPORT == apt)
  if (nrow(apt_sectors) > 0) {
    n_40 <- nrow(filter(apt_sectors, RANGE_NM == 40))
    n_100 <- nrow(filter(apt_sectors, RANGE_NM == 100))
    cat(sprintf("40NM=%d, 100NM=%d sectors\n", n_40, n_100))
  } else {
    cat("No sectors generated\n")
  }
}

# Combine all
approved_arr_sectors <- bind_rows(all_sectors) %>%
  arrange(AIRPORT, RANGE_NM, SECTOR_SEQ)

cat("\n")
cat(sprintf("Total: %d sector definitions across 12 airports\n", nrow(approved_arr_sectors)))

# Save to inst/extdata
extdata_dir <- "inst/extdata"
if (!dir.exists(extdata_dir)) {
  dir.create(extdata_dir, recursive = TRUE)
}

# Save as parquet (primary format)
write_parquet(approved_arr_sectors, file.path(extdata_dir, "arr-sectors-approved-2024.parquet"))

# Also save as CSV for inspection
write.csv(approved_arr_sectors, file.path(extdata_dir, "arr-sectors-approved-2024.csv"),
          row.names = FALSE)

cat("\n✓ Saved to inst/extdata/:\n")
cat("  - arr-sectors-approved-2024.parquet\n")
cat("  - arr-sectors-approved-2024.csv\n\n")

cat("Preview:\n")
print(head(approved_arr_sectors, 10))
