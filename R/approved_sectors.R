#' Read Approved TMA Sector Definitions
#'
#' Loads the approved terminal airspace sector definitions from the package data.
#' These sectors were validated and approved based on the 2024 reference data
#' using the valley-based sectorization algorithm.
#'
#' @param phase Character. Phase filter: "ARR" for arrivals, "DEP" for departures,
#'   or NULL for both (default NULL).
#' @param airports Character vector of ICAO airport codes to filter, or NULL for
#'   all airports (default NULL).
#' @param range_nm Numeric. Range filter in nautical miles (40 or 100), or NULL
#'   for both ranges (default NULL).
#'
#' @return A tibble with columns: AIRPORT, PHASE, RANGE_NM, SECTOR, SECTOR_LABEL,
#'   SECTOR_SEQ, BEARING_FROM, BEARING_TO, and NORTH_OVERRUN.
#'
#' @details
#' The approved sector definitions are stored in `inst/extdata/` and loaded from
#' the installed package. Currently only ARR (arrival) sectors are available.
#' DEP (departure) sectors are pending approval.
#'
#' Sector definitions include:
#' \itemize{
#'   \item AIRPORT: ICAO airport code
#'   \item PHASE: Flight phase (ARR or DEP)
#'   \item RANGE_NM: Distance from airport (40 or 100 NM)
#'   \item SECTOR: Bearing sector label (e.g., "BRG000-090")
#'   \item SECTOR_LABEL: Human-readable label (e.g., "ARR BRG000-090")
#'   \item SECTOR_SEQ: Sector sequence number (1, 2, 3, ...)
#'   \item BEARING_FROM: Starting bearing in degrees (0-359)
#'   \item BEARING_TO: Ending bearing in degrees (0-359)
#'   \item NORTH_OVERRUN: Whether the sector spans across 360/000 degrees
#' }
#'
#' @examples
#' \dontrun{
#' # Load all approved ARR sectors
#' arr_sectors <- read_approved_tma_sector_definitions(phase = "ARR")
#'
#' # Load sectors for specific airports
#' eddf_sectors <- read_approved_tma_sector_definitions(
#'   phase = "ARR",
#'   airports = c("EDDF", "EDDM")
#' )
#'
#' # Load only 40 NM sectors
#' sectors_40nm <- read_approved_tma_sector_definitions(
#'   phase = "ARR",
#'   range_nm = 40
#' )
#' }
#'
#' @export
read_approved_tma_sector_definitions <- function(
    phase = NULL,
    airports = NULL,
    range_nm = NULL
) {
  # Determine which file to load based on phase
  if (is.null(phase)) {
    # Try to load both ARR and DEP, combine if both exist
    arr_file <- system.file("extdata", "arr-sectors-approved-2024.parquet", package = "PBWG")
    dep_file <- system.file("extdata", "dep-sectors-approved-2024.parquet", package = "PBWG")

    sectors_list <- list()

    if (file.exists(arr_file) && nzchar(arr_file)) {
      sectors_list[["ARR"]] <- arrow::read_parquet(arr_file)
    }

    if (file.exists(dep_file) && nzchar(dep_file)) {
      sectors_list[["DEP"]] <- arrow::read_parquet(dep_file)
    }

    if (length(sectors_list) == 0) {
      stop("No approved sector definition files found in package")
    }

    sectors <- dplyr::bind_rows(sectors_list)

  } else if (phase == "ARR") {
    file_path <- system.file("extdata", "arr-sectors-approved-2024.parquet", package = "PBWG")

    if (!file.exists(file_path) || !nzchar(file_path)) {
      stop("ARR sector definitions file not found. Run scripts/export-approved-arr-sectors.R to generate it.")
    }

    sectors <- arrow::read_parquet(file_path)

  } else if (phase == "DEP") {
    file_path <- system.file("extdata", "dep-sectors-approved-2024.parquet", package = "PBWG")

    if (!file.exists(file_path) || !nzchar(file_path)) {
      stop("DEP sector definitions not yet approved. Only ARR sectors are currently available.")
    }

    sectors <- arrow::read_parquet(file_path)

  } else {
    stop("phase must be NULL, 'ARR', or 'DEP'")
  }

  # Apply filters
  if (!is.null(airports)) {
    sectors <- dplyr::filter(sectors, .data$AIRPORT %in% airports)
  }

  if (!is.null(range_nm)) {
    sectors <- dplyr::filter(sectors, .data$RANGE_NM == range_nm)
  }

  tibble::as_tibble(sectors) |>
    dplyr::mutate(
      NORTH_OVERRUN = .data$BEARING_FROM > .data$BEARING_TO &
        .data$BEARING_TO != 0
    )
}
