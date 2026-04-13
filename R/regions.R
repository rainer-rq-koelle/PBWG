#' EUROCONTROL Member State ICAO Prefixes
#'
#' Returns the two-letter ICAO prefixes used for simplified regional
#' classification in PBWG workflows. Iceland (`BI`) is included.
#'
#' @return A character vector of ICAO prefixes.
#' @export
eurocontrol_member_state_prefixes <- function() {
  c(
    "BI", "EB", "ED", "EE", "EF", "EG", "EH", "EI", "EK", "EL", "EN", "EP",
    "ES", "ET", "EV", "EY", "GC", "GM", "LA", "LB", "LC", "LD", "LE", "LF",
    "LG", "LH", "LI", "LJ", "LK", "LM", "LN", "LO", "LP", "LQ", "LR", "LS",
    "LT", "LU", "LV", "LW", "LX", "LY", "LZ", "UB", "UD", "UG", "UK"
  )
}

#' Check Whether Airports Match the EUROCONTROL Member State Lookup
#'
#' @param icao_vec Character vector of ICAO airport indicators.
#' @param prefixes Optional character vector of two-letter ICAO prefixes.
#'
#' @return A logical vector.
#' @export
is_eurocontrol_airport <- function(
    icao_vec,
    prefixes = eurocontrol_member_state_prefixes()
) {
  stringr::str_sub(icao_vec, 1, 2) %in% prefixes
}
