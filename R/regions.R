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

#' Brazil ICAO Indicator Prefixes
#'
#' Returns the official two-letter ICAO prefixes assigned to Brazil for the
#' PBWG regional traffic workflows.
#'
#' @return A character vector of ICAO prefixes.
#' @export
brazil_airport_prefixes <- function() {
  c("SB", "SD", "SI", "SJ", "SN", "SS", "SW")
}

#' Check Whether Indicators Belong to the Brazil Regional Scope
#'
#' The Brazil network extract contains official ICAO aerodrome indicators and
#' non-standard numeric indicators used for locations such as offshore
#' helicopter platforms. PBWG treats both groups as part of the Brazilian
#' regional scope for network traffic classification.
#'
#' @param indicator_vec Character vector of aerodrome or location indicators.
#' @param prefixes Optional character vector of official two-letter prefixes.
#' @param include_numeric Whether indicators starting with a digit should be
#'   treated as Brazilian regional locations.
#'
#' @return A logical vector.
#' @export
is_brazil_airport_indicator <- function(
    indicator_vec,
    prefixes = brazil_airport_prefixes(),
    include_numeric = TRUE
) {
  indicators <- stringr::str_to_upper(stringr::str_trim(as.character(indicator_vec)))
  official_indicator <- stringr::str_sub(indicators, 1, 2) %in% prefixes
  numeric_indicator <- include_numeric &
    dplyr::coalesce(stringr::str_detect(indicators, "^[0-9]"), FALSE)

  official_indicator | numeric_indicator
}
