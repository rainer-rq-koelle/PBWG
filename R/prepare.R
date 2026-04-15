#' Prepare Network Data
#'
#' Applies the standard first-stage harmonisation for NM flight table data.
#'
#' @param nm_flights A decoded NM flight table tibble.
#'
#' @return A tibble prepared for downstream project-specific processing.
#' @export
prepare_network_data <- function(nm_flights) {
  tibble::as_tibble(nm_flights)
}

#' Prepare Airport Data
#'
#' Applies the standard first-stage harmonisation for APDF airport data and can
#' optionally filter the data to a set of aerodromes.
#'
#' @param apdf A decoded APDF tibble.
#' @param airports Optional character vector of ICAO airport codes.
#'
#' @return A tibble prepared for downstream project-specific processing.
#' @export
prepare_airport_data <- function(apdf, airports = NULL) {
  apdf <- tibble::as_tibble(apdf)

  if (is.null(airports)) {
    return(apdf)
  }

  dplyr::filter(apdf, .data$ICAO %in% airports)
}
