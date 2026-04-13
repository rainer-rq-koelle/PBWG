#' Check the Contents of a ZIP Archive
#'
#' @param path Path to the directory containing the ZIP file.
#' @param archive Name of the ZIP file.
#'
#' @return A data frame as returned by [utils::unzip()] with `list = TRUE`.
#' @export
check_zip_content <- function(path, archive) {
  this_zip <- fs::path(path, archive)
  utils::unzip(this_zip, list = TRUE)
}

#' Read Selected Files from a ZIP Archive
#'
#' Extracts files from a ZIP archive to a temporary directory, reads them into
#' R, and removes the temporary files on exit.
#'
#' @param path Path to the directory containing the ZIP file.
#' @param archive Name of the ZIP file.
#' @param files Optional character vector of files to extract. If `NULL`, all
#'   archived files are extracted and read.
#' @param type File type inside the archive. One of `"parquet"`, `"csv"`, or
#'   `"csv_auto"`.
#'
#' @return A tibble when one file is read, otherwise a named list of tibbles.
#' @export
read_zip_content <- function(path, archive, files = NULL, type = c("parquet", "csv", "csv_auto")) {
  type <- base::match.arg(type)
  this_zip <- fs::path(path, archive)
  tmp_dir <- fs::path(base::tempdir(), fs::path_ext_remove(archive))

  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  if (base::is.null(files)) {
    files <- utils::unzip(this_zip, exdir = tmp_dir)
    files <- base::basename(files)
  } else {
    utils::unzip(this_zip, files = files, exdir = tmp_dir)
  }

  reader <- zip_reader(type)

  ds <- fs::path(tmp_dir, files) |>
    purrr::set_names(fs::path_ext_remove(base::basename(files))) |>
    purrr::map(reader) |>
    purrr::map(tibble::as_tibble)

  if (base::length(ds) == 1) {
    ds <- ds[[1]]
  }

  ds
}

#' Read CSV Files with Automatic Delimiter Detection
#'
#' Wraps [readr::read_csv()] and [readr::read_csv2()] by probing the file first
#' and then selecting the suitable parser.
#'
#' @param fn File name including path.
#' @param colspec Optional readr column specification.
#' @param show_col_types Passed to the readr parser.
#' @param ... Additional arguments passed to the selected readr parser.
#'
#' @return A tibble.
#' @export
read_csv12 <- function(fn, colspec = NULL, show_col_types = FALSE, ...) {
  tst <- readr::read_csv(fn, n_max = 3, show_col_types = show_col_types)
  siz <- dim(tst)[2]

  if (siz > 1) {
    return(
      readr::read_csv(
        fn,
        col_types = colspec,
        show_col_types = show_col_types,
        ...
      )
    )
  }

  readr::read_csv2(
    fn,
    col_types = colspec,
    show_col_types = show_col_types,
    ...
  )
}

zip_reader <- function(type) {
  switch(
    type,
    parquet = parquet_reader(),
    csv = readr::read_csv,
    csv_auto = read_csv12
  )
}

parquet_reader <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    rlang::abort(
      "Reading parquet files from ZIP archives requires the optional 'arrow' package."
    )
  }

  arrow::read_parquet
}
