#' Build an Output File Name
#'
#' Creates a standard output file name that can encode the product type,
#' relevant year or year range, and an optional variant suffix.
#'
#' @param prefix Product or file family prefix.
#' @param years Integer or character vector of years.
#' @param suffix Optional suffix such as a reference-time variant.
#' @param ext File extension without a leading dot.
#'
#' @return A length-one character string.
#' @export
build_output_filename <- function(prefix, years, suffix = NULL, ext = "csv") {
  year_label <- if (length(years) == 1) {
    as.character(years)
  } else {
    stringr::str_c(min(years), max(years), sep = "-")
  }

  pieces <- c(prefix, year_label, suffix)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  base_name <- stringr::str_c(pieces, collapse = "_")

  stringr::str_c(base_name, ".", ext)
}
