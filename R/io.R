#' Read an NM Flight Table Extract
#'
#' Reads a warehouse extract into a tibble without changing the source column
#' names. Use [decode_nm_flights()] to map source names to harmonised names.
#'
#' @param path Path to the extracted file.
#' @param delim Delimiter used in the file.
#' @param ... Additional arguments passed to [readr::read_delim()].
#'
#' @return A tibble.
#' @export
read_nm_flights <- function(path, delim = ",", ...) {
  read_delim_upper(path = path, delim = delim, ...)
}

#' Read an APDF Extract
#'
#' Reads an airport operator flow extract into a tibble without changing the
#' source column names. Use [decode_apdf()] to map source names to harmonised
#' names.
#'
#' @param path Path to the extracted file.
#' @param delim Delimiter used in the file.
#' @param ... Additional arguments passed to [readr::read_delim()].
#'
#' @return A tibble.
#' @export
read_apdf <- function(path, delim = ",", ...) {
  read_delim_upper(path = path, delim = delim, ...)
}

read_delim_upper <- function(path, delim = ",", ...) {
  readr::read_delim(
    file = path,
    delim = delim,
    show_col_types = FALSE,
    progress = FALSE,
    ...
  ) |>
    tibble::as_tibble()
}
