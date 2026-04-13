#' NM Flight Table Dictionary
#'
#' Returns a template dictionary used to map source column names from NM flight
#' table extracts to harmonised package names.
#'
#' @return A tibble with `SOURCE_NAME`, `TARGET_NAME`, and `DESCRIPTION`.
#' @export
nm_flights_dictionary <- function() {
  tibble::tibble(
    SOURCE_NAME = c("ADEP", "ADES", "EOBT", "AOBT", "ATOT", "ALDT", "PHASE"),
    TARGET_NAME = c("ADEP", "ADES", "EOBT", "AOBT", "ATOT", "ALDT", "PHASE"),
    DESCRIPTION = c(
      "Departure aerodrome ICAO indicator",
      "Arrival aerodrome ICAO indicator",
      "Estimated off-block time",
      "Actual off-block time",
      "Actual take-off time",
      "Actual landing time",
      "Flight phase or operational status"
    )
  )
}

#' APDF Dictionary
#'
#' Returns a template dictionary used to map source column names from APDF
#' extracts to harmonised package names.
#'
#' @return A tibble with `SOURCE_NAME`, `TARGET_NAME`, and `DESCRIPTION`.
#' @export
apdf_dictionary <- function() {
  tibble::tibble(
    SOURCE_NAME = c("AERODROME", "FLIGHT_DATE", "MOVEMENT_TYPE", "ATOT", "ALDT"),
    TARGET_NAME = c("AERODROME", "FLIGHT_DATE", "MOVEMENT_TYPE", "ATOT", "ALDT"),
    DESCRIPTION = c(
      "Airport ICAO indicator",
      "Flight operation date",
      "Arrival or departure movement type",
      "Actual take-off time",
      "Actual landing time"
    )
  )
}

#' Decode NM Flight Table Columns
#'
#' Renames source columns in an NM flight table extract to harmonised names
#' according to a dictionary.
#'
#' @param data A tibble containing source data.
#' @param dictionary A tibble with `SOURCE_NAME` and `TARGET_NAME`.
#'
#' @return A tibble with harmonised column names.
#' @export
decode_nm_flights <- function(data, dictionary = nm_flights_dictionary()) {
  decode_columns(data = data, dictionary = dictionary)
}

#' Decode APDF Columns
#'
#' Renames source columns in an APDF extract to harmonised names according to a
#' dictionary.
#'
#' @param data A tibble containing source data.
#' @param dictionary A tibble with `SOURCE_NAME` and `TARGET_NAME`.
#'
#' @return A tibble with harmonised column names.
#' @export
decode_apdf <- function(data, dictionary = apdf_dictionary()) {
  decode_columns(data = data, dictionary = dictionary)
}

decode_columns <- function(data, dictionary) {
  stop_if_dictionary_invalid(dictionary)

  source_names <- names(data)
  mapping <- stats::setNames(dictionary$TARGET_NAME, dictionary$SOURCE_NAME)

  names(data) <- purrr::map_chr(
    source_names,
    function(.x) {
      mapped_name <- unname(mapping[.x])

      if (length(mapped_name) == 0 || is.na(mapped_name)) {
        return(.x)
      }

      mapped_name
    }
  )

  tibble::as_tibble(data)
}

stop_if_dictionary_invalid <- function(dictionary) {
  required_columns <- c("SOURCE_NAME", "TARGET_NAME")

  if (!all(required_columns %in% names(dictionary))) {
    rlang::abort(
      paste0(
        "Dictionary must contain columns: ",
        stringr::str_flatten(required_columns, ", ")
      )
    )
  }

  if (anyDuplicated(dictionary$SOURCE_NAME) > 0) {
    rlang::abort("Dictionary SOURCE_NAME values must be unique.")
  }
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
