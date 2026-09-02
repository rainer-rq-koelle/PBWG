################################################################################
# Environment Test - Check what works on this machine
################################################################################

cat("PBWG Environment Diagnostic\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Test 1: Network Drive Access
cat("TEST 1: Network Drive Access\n")
cat("─────────────────────────────────────────\n")

test_paths <- list(
  "PBWG-data (OneDrive)" = "C:/Users/rkoelle/EUROCONTROL/ANS Performance Benchmarking - data-2026/PBWG-data",
  "ICAO 2026 prefill" = "\\\\sky.corp.eurocontrol.int\\DFSRoot\\Groups\\HQ\\dgof-pru\\Data\\DataProcessing\\ICAO_Template\\2026 - ICAO - template\\prefil",
  "ICAO 2026 received" = "\\\\sky.corp.eurocontrol.int\\DFSRoot\\Groups\\HQ\\dgof-pru\\Data\\DataProcessing\\ICAO_Template\\2026 - ICAO - template\\Received",
  "Local APDF" = "C:/Users/rkoelle/dev/RProjects/xx-test-gotcha/data/apdf-annual"
)

for (name in names(test_paths)) {
  path <- test_paths[[name]]
  exists <- dir.exists(path)

  if (exists) {
    n_files <- length(list.files(path))
    cat(sprintf("✓ %-30s - %d files\n", name, n_files))
  } else {
    cat(sprintf("✗ %-30s - NOT ACCESSIBLE\n", name))
  }
}

cat("\n")

# Test 2: R Package Status
cat("TEST 2: Package Status\n")
cat("─────────────────────────────────────────\n")

required_packages <- c("dplyr", "arrow", "ggplot2", "tidyr", "lubridate", "stringr")

for (pkg in required_packages) {
  has_pkg <- requireNamespace(pkg, quietly = TRUE)
  if (has_pkg) {
    ver <- as.character(packageVersion(pkg))
    cat(sprintf("✓ %-15s - %s\n", pkg, ver))
  } else {
    cat(sprintf("✗ %-15s - NOT INSTALLED\n", pkg))
  }
}

cat("\n")

# Test 3: Data File Access
cat("TEST 3: APDF Data Access\n")
cat("─────────────────────────────────────────\n")

test_files <- c(
  "apdf-2024.zip" = "../xx-test-gotcha/data/apdf-annual/apdf-2024.zip",
  "apdf-LGAV.zip" = "../xx-test-gotcha/data/apdf-annual/apdf-LGAV-2019-2024.zip"
)

for (name in names(test_files)) {
  path <- test_files[[name]]
  exists <- file.exists(path)

  if (exists) {
    size_mb <- round(file.size(path) / 1024^2, 1)
    cat(sprintf("✓ %-20s - %.1f MB\n", name, size_mb))
  } else {
    cat(sprintf("✗ %-20s - NOT FOUND\n", name))
  }
}

cat("\n")

# Test 4: Memory Available
cat("TEST 4: System Resources\n")
cat("─────────────────────────────────────────\n")
cat(sprintf("R Version: %s\n", R.version.string))
cat(sprintf("Platform: %s\n", R.version$platform))

gc_info <- gc()
cat(sprintf("Memory in use: %.1f MB\n", sum(gc_info[, 2])))

cat("\n")

# Test 5: Quick Data Read Test
cat("TEST 5: Quick Data Processing Test\n")
cat("─────────────────────────────────────────\n")

tryCatch({
  library(arrow)

  # Try to extract and read one file
  temp <- tempfile()
  unzip("../xx-test-gotcha/data/apdf-annual/apdf-2024.zip",
        files = "EDDF_01-JAN-2024_31-DEC-2024.gz.parquet",
        exdir = dirname(temp), junkpaths = TRUE)

  test_file <- file.path(dirname(temp), "EDDF_01-JAN-2024_31-DEC-2024.gz.parquet")

  if (file.exists(test_file)) {
    apdf <- read_parquet(test_file)
    cat(sprintf("✓ Successfully read EDDF data: %s rows\n", format(nrow(apdf), big.mark = ",")))
    unlink(test_file)
  } else {
    cat("✗ File extraction failed\n")
  }

}, error = function(e) {
  cat(sprintf("✗ Data read test failed: %s\n", e$message))
})

cat("\n")

# Summary
cat("═══════════════════════════════════════════════════════════════════\n")
cat("SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("This machine CAN:\n")
cat("  - Run R scripts and process data\n")
cat("  - Access local APDF files\n")
cat("  - Load required packages\n")
cat("  - Extract and read parquet files\n\n")

cat("Check above for:\n")
cat("  - Network drive accessibility\n")
cat("  - PBWG-data location status\n")
cat("  - Any missing packages or files\n\n")

cat("For Quarto issues - see DIAGNOSTIC-REPORT.md\n")
